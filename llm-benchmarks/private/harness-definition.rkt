#lang racket/base

(require json
         racket/file
         racket/path
         racket/string
         "util.rkt")

(provide
 create-harness!
 default-harness-draft
 harness-config
 harness-template-draft
 validate-harness!)

(define (default-harness-draft [id ""])
  (hasheq 'id id 'image "" 'script "" 'resources "" 'config_directory ""
          'environment "{}" 'environment_from_host "" 'labels "{}"
          'cpus "4" 'memory "4g" 'pids "256" 'wall_seconds "2700"
          'grace_seconds "10" 'max_cost_usd "0"))

(define (json-object text label)
  (define value
    (with-handlers ([exn:fail? (lambda (_e) (error 'harness "~a must be a JSON object." label))])
      (string->jsexpr text)))
  (unless (and (hash? value) (andmap string? (hash-values value)))
    (error 'harness "~a must be a JSON object with string values." label))
  value)

(define (host-variables draft)
  (filter non-empty-string? (map string-trim (string-split (hash-ref draft 'environment_from_host) "\n"))))

(define (harness-config draft)
  (define config
    (hasheq
     'schema_version 1 'id (hash-ref draft 'id) 'script "run.sh"
     'environment (json-object (hash-ref draft 'environment) "Environment")
     'environment_from_host (host-variables draft)
     'labels (json-object (hash-ref draft 'labels) "Labels")
     'docker (hasheq 'image (hash-ref draft 'image)
                     'cpus (string->number (hash-ref draft 'cpus))
                     'memory (hash-ref draft 'memory)
                     'pids_limit (string->number (hash-ref draft 'pids)))
     'limits (hasheq 'wall_seconds (string->number (hash-ref draft 'wall_seconds))
                     'termination_grace_seconds (string->number (hash-ref draft 'grace_seconds))
                     'max_cost_usd (string->number (hash-ref draft 'max_cost_usd)))))
  (define directory (hash-ref draft 'config_directory))
  (if (non-empty-string? directory)
      (hash-set config 'config_directory directory)
      config))

(define (harness-template-draft directory id)
  (define config (read-json-file (build-path directory "config.json")))
  (define docker (hash-ref config 'docker))
  (define limits (hash-ref config 'limits))
  (hasheq
   'id id 'resources (path->string (simple-form-path directory))
   'script (file->string (build-path directory (hash-ref config 'script)))
   'image (hash-ref docker 'image)
   'cpus (number->string (hash-ref docker 'cpus 4))
   'memory (hash-ref docker 'memory "4g")
   'pids (number->string (hash-ref docker 'pids_limit 256))
   'wall_seconds (number->string (hash-ref limits 'wall_seconds 2700))
   'grace_seconds (number->string (hash-ref limits 'termination_grace_seconds 10))
   'max_cost_usd (number->string (hash-ref limits 'max_cost_usd 0))
   'config_directory (hash-ref config 'config_directory "")
   'environment (jsexpr->string (hash-ref config 'environment #hasheq()))
   'environment_from_host (string-join (hash-ref config 'environment_from_host null) "\n")
   'labels (jsexpr->string (hash-ref config 'labels #hasheq()))))

(define (safe-relative-path? path)
  (and (non-empty-string? path) (relative-path? path)
       (not (regexp-match? #px"(^|/)[.][.]?(/|$)|\\\\" path))))

(define (validate-harness! root draft)
  (define id (hash-ref draft 'id))
  (unless (regexp-match? #px"^[a-z][a-z0-9-]*$" id)
    (error 'harness "ID must start with a lowercase letter and use only letters, digits, and hyphens."))
  (define destination (build-path root "harnesses" id))
  (when (or (file-exists? destination) (directory-exists? destination) (link-exists? destination))
    (error 'harness "Harness already exists: ~a" id))
  (for ([key '(image script)])
    (unless (non-empty-string? (string-trim (hash-ref draft key)))
      (error 'harness "~a is required." key)))
  (for ([key '(pids wall_seconds grace_seconds)])
    (unless (exact-positive-integer? (string->number (hash-ref draft key)))
      (error 'harness "~a must be a positive whole number." key)))
  (for ([key '(cpus max_cost_usd)])
    (define value (string->number (hash-ref draft key)))
    (unless (and (real? value) (<= 0 value) (< value +inf.0)
                 (or (eq? key 'max_cost_usd) (positive? value)))
      (error 'harness "~a must be a ~a number." key (if (eq? key 'cpus) "positive" "nonnegative"))))
  (unless (regexp-match? #px"^[1-9][0-9]*[bkmgBKMG]?$" (hash-ref draft 'memory))
    (error 'harness "Memory must be a positive size, such as 4g or 512m."))
  (define config (harness-config draft))
  (define environment (hash-ref config 'environment))
  (for ([name (in-list (append (map symbol->string (hash-keys environment)) (host-variables draft)))])
    (unless (regexp-match? #px"^[A-Za-z_][A-Za-z0-9_]*$" name)
      (error 'harness "Invalid environment variable name: ~a" name)))
  (for ([name (in-list (host-variables draft))])
    (when (hash-has-key? environment (string->symbol name))
      (error 'harness "~a cannot be both configured and forwarded from the host." name)))
  (define resources (hash-ref draft 'resources))
  (unless (string=? resources "")
    (unless (directory-exists? resources)
      (error 'harness "Resource directory does not exist: ~a" resources))
    (define relative (find-relative-path (simple-form-path resources) (simple-form-path destination)))
    (when (and (relative-path? relative) (not (member 'up (explode-path relative))))
      (error 'harness "The resource directory cannot contain the new harness.")))
  (define directory (hash-ref draft 'config_directory))
  (unless (string=? directory "")
    (unless (and (safe-relative-path? directory) (non-empty-string? resources)
                 (directory-exists? (build-path resources directory)))
      (error 'harness "Configuration directory must name a folder inside the resource directory.")))
  config)

(define (copy-resources! source destination)
  (make-directory* destination)
  (for ([entry (in-list (directory-list source))]
         #:unless (member (path->string entry) '(".git" "compiled")))
    (define from (build-path source entry))
    (define to (build-path destination entry))
    (cond
      [(link-exists? from) (error 'harness "Resources must not contain symlinks: ~a" from)]
      [(directory-exists? from) (copy-resources! from to)]
      [else (copy-file from to)])))

(define (create-harness! root draft)
  (define config (validate-harness! root draft))
  (define harnesses (build-path root "harnesses"))
  (make-directory* harnesses)
  (define staging (make-temporary-file ".new-harness-~a" 'directory harnesses))
  (define destination (build-path harnesses (hash-ref draft 'id)))
  (dynamic-wind
    void
    (lambda ()
      (define resources (hash-ref draft 'resources))
      (unless (string=? resources "") (copy-resources! resources staging))
      (write-json-file (build-path staging "config.json") config)
      (define script (build-path staging "run.sh"))
      (display-to-file (hash-ref draft 'script) script #:exists 'truncate/replace)
      (file-or-directory-permissions script #o755)
      (rename-file-or-directory staging destination)
      destination)
    (lambda ()
      (when (directory-exists? staging) (delete-directory/files staging)))))
