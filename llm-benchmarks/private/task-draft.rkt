#lang racket/base

(require racket/file
         racket/path
         "util.rkt")

(provide
 load-task-draft
 save-task-draft!
 task-drafts-root)

(define (task-drafts-root root)
  (build-path root "task-drafts"))

(define (load-task-draft path)
  (define saved (read-json-file path))
  (unless (and (hash? saved)
               (equal? (hash-ref saved 'schema_version #f) 1)
               (hash? (hash-ref saved 'draft #f))
               (for/and ([value (in-hash-values (hash-ref saved 'draft))])
                 (string? value))
               (exact-integer? (hash-ref saved 'step #f))
               (<= 0 (hash-ref saved 'step) 4))
    (error 'task-draft "Not a supported task draft: ~a" path))
  saved)

(define (save-task-draft! root draft step #:path [saved-path #f])
  (define directory (if saved-path (path-only (path->complete-path saved-path)) (task-drafts-root root)))
  (make-directory* directory)
  (define path (or saved-path (make-temporary-file "draft-~a.json" #f directory)))
  (define temporary (make-temporary-file ".draft-save-~a.json" #f directory))
  (dynamic-wind
    void
    (lambda ()
      (write-json-file temporary (hasheq 'schema_version 1 'saved_at (timestamp)
                                         'step step 'draft draft))
      (rename-file-or-directory temporary path #t)
      path)
    (lambda ()
      (when (file-exists? temporary) (delete-file temporary)))))
