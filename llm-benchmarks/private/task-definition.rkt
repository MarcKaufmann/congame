#lang racket/base

(require json
         racket/file
         racket/path
         racket/string
         "util.rkt")

(provide
 create-task!
 default-study-source
 parse-checks
 task-config
 validate-task!)

(define (default-study-source study-id)
  (format "#lang conscript\n\n(provide ~a)\n\n(defstep (start)\n  @md{# Study\n\n      Implement the task described in TASK.md.})\n\n(defstudy ~a\n  [start --> start])\n" study-id study-id))

(define (parse-checks text)
  (for/list ([line (in-list (string-split text "\n"))]
             #:unless (string=? (string-trim line) ""))
    (define command
      (with-handlers ([exn:fail? (lambda (_e) (error 'task "Invalid check: ~a" line))])
        (string->jsexpr line)))
    (unless (and (list? command) (pair? command)
                 (andmap (lambda (arg) (and (string? arg) (non-empty-string? arg))) command))
      (error 'task "Each check must be a JSON array of non-empty arguments: ~a" line))
    command))

(define (allowed-paths draft)
  (map string-trim (string-split (hash-ref draft 'allowed_paths) "\n")))

(define (task-config draft)
  (hasheq
   'schema_version 1
   'id (hash-ref draft 'id)
   'version 1
   'title (hash-ref draft 'title)
   'study_id (hash-ref draft 'study_id)
   'study_path "study.rkt"
   'prompt "prompt.md"
   'fixture "fixture"
   'starter_study "starter/study.rkt"
   'allowed_paths (allowed-paths draft)
   'references (list (hasheq 'source "conscript" 'destination "conscript")
                     (hasheq 'source "congame-doc" 'destination "congame-doc")
                     (hasheq 'source "conscript/examples" 'destination "examples"))
   'check_timeout_seconds (string->number (hash-ref draft 'timeout))
   'visible_checks (parse-checks (hash-ref draft 'visible_checks))
   'hidden_checks (parse-checks (hash-ref draft 'hidden_checks))
   'rubric (hash-ref draft 'rubric)))

(define (validate-task! root draft)
  (for ([key '(id study_id)])
    (unless (regexp-match? #px"^[a-z][a-z0-9-]*$" (hash-ref draft key))
      (error 'task "~a must start with a lowercase letter and use only letters, digits, and hyphens." key)))
  (for ([key '(title prompt)])
    (when (string=? (string-trim (hash-ref draft key)) "")
      (error 'task "~a is required." key)))
  (define destination (build-path root "tasks" (hash-ref draft 'id)))
  (when (or (directory-exists? destination) (file-exists? destination) (link-exists? destination))
    (error 'task "Task already exists: ~a" (hash-ref draft 'id)))
  (define rubric-id (hash-ref draft 'rubric))
  (unless (and (regexp-match? #px"^[a-zA-Z0-9-]+$" rubric-id)
               (file-exists? (build-path root "rubrics" (string-append rubric-id ".json"))))
    (error 'task "Choose an existing rubric."))
  (define timeout (string->number (hash-ref draft 'timeout)))
  (unless (exact-positive-integer? timeout)
    (error 'task "Check timeout must be a positive whole number of seconds."))
  (for ([path (in-list (allowed-paths draft))])
    (unless (and (non-empty-string? path)
                 (relative-path? path)
                 (not (regexp-match? #px"(^|/)[.][.]?(/|$)|\\\\" path)))
      (error 'task "Allowed paths must be relative files or directories without . or ..: ~a" path)))
  (unless (member "study.rkt" (allowed-paths draft))
    (error 'task "Include study.rkt in the allowed paths."))
  (for ([key '(fixture_directory checks_directory)])
    (define source (hash-ref draft key ""))
    (unless (string=? source "")
      (unless (directory-exists? source)
        (error 'task "Directory does not exist: ~a" source))
      (define relative (find-relative-path (simple-form-path source) (simple-form-path destination)))
      (when (and (relative-path? relative) (not (member 'up (explode-path relative))))
        (error 'task "The source directory cannot contain the new task directory."))))
  (define fixture (hash-ref draft 'fixture_directory ""))
  (unless (or (string=? fixture "") (file-exists? (build-path fixture "study.rkt")))
    (error 'task "The fixture directory must contain study.rkt."))
  (define starter (hash-ref draft 'starter_file ""))
  (unless (or (string=? starter "") (file-exists? starter))
    (error 'task "Starter file does not exist: ~a" starter))
  (task-config draft))

(define (copy-task-files! source destination)
  (make-directory* destination)
  (for ([entry (in-list (directory-list source))]
         #:unless (member (path->string entry) '(".git" "compiled")))
    (define from (build-path source entry))
    (define to (build-path destination entry))
    (cond
      [(link-exists? from) (error 'task "Source directories must not contain symlinks: ~a" from)]
      [(directory-exists? from) (copy-task-files! from to)]
      [else (copy-file from to)])))

(define (create-task! root draft)
  (define config (validate-task! root draft))
  (define tasks (build-path root "tasks"))
  (make-directory* tasks)
  (define staging (make-temporary-file ".new-task-~a" 'directory tasks))
  (define destination (build-path tasks (hash-ref draft 'id)))
  (dynamic-wind
    void
    (lambda ()
      (write-json-file (build-path staging "config.json") config)
      (display-to-file (hash-ref draft 'prompt) (build-path staging "prompt.md"))
      (define source (default-study-source (hash-ref draft 'study_id)))
      (define fixture (hash-ref draft 'fixture_directory ""))
      (cond
        [(string=? fixture "")
         (make-directory (build-path staging "fixture"))
         (display-to-file source (build-path staging "fixture" "study.rkt"))]
        [else (copy-task-files! fixture (build-path staging "fixture"))])
      (make-directory (build-path staging "starter"))
      (define starter (hash-ref draft 'starter_file ""))
      (if (string=? starter "")
          (display-to-file source (build-path staging "starter" "study.rkt"))
          (copy-file starter (build-path staging "starter" "study.rkt")))
      (define checks (hash-ref draft 'checks_directory ""))
      (unless (string=? checks "")
        (copy-task-files! checks (build-path staging "checks")))
      (rename-file-or-directory staging destination)
      destination)
    (lambda ()
      (when (directory-exists? staging)
        (delete-directory/files staging)))))
