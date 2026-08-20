#lang racket/base

(require racket/path
         "util.rkt")

(provide
 benchmark-config-root
 list-config-ids
 load-harness
 load-suite
 load-task
 resolve-benchmark-path)

(define benchmark-config-root (make-parameter #f))

(define (root)
  (or (benchmark-config-root)
      (error 'config "benchmark-config-root is not configured")))

(define (resolve-benchmark-path . parts)
  (apply build-path (root) parts))

(define (config-path kind id)
  (build-path (root) kind id "config.json"))

(define (load-config kind id)
  (define path (config-path kind id))
  (unless (file-exists? path)
    (error 'load-config "unknown ~a configuration ~s (~a)" kind id path))
  (read-json-file path))

(define (load-harness id) (load-config "harnesses" id))
(define (load-task id) (load-config "tasks" id))

(define (load-suite id)
  (define path (build-path (root) "suites" (string-append id ".json")))
  (unless (file-exists? path)
    (error 'load-suite "unknown suite ~s (~a)" id path))
  (read-json-file path))

(define (list-config-ids kind)
  (define path (build-path (root) kind))
  (cond
    [(equal? kind "suites")
     (sort
      (for/list ([entry (in-list (directory-list path))]
                 #:when (equal? (path-get-extension entry) #".json"))
        (path->string (path-replace-extension entry #"")))
      string<?)]
    [else
     (sort
      (for/list ([entry (in-list (directory-list path))]
                 #:when (file-exists? (build-path path entry "config.json")))
        (path->string entry))
      string<?)]))
