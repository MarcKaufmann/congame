#lang racket/base

(require racket/file
         racket/path
         "util.rkt")

(provide
 load-wizard-draft
 save-wizard-draft!
 wizard-drafts-root)

(define (wizard-drafts-root root [kind "task"])
  (build-path root (string-append kind "-drafts")))

(define (load-wizard-draft path #:kind [kind "task"])
  (define saved (read-json-file path))
  (unless (and (hash? saved)
               (equal? (hash-ref saved 'schema_version #f) 1)
               (equal? (hash-ref saved 'kind "task") kind)
               (hash? (hash-ref saved 'draft #f))
               (for/and ([value (in-hash-values (hash-ref saved 'draft))])
                 (string? value))
               (exact-integer? (hash-ref saved 'step #f))
               (<= 0 (hash-ref saved 'step) 4))
    (error 'wizard-draft "Not a supported ~a draft: ~a" kind path))
  saved)

(define (save-wizard-draft! root draft step #:path [saved-path #f] #:kind [kind "task"])
  (define directory (if saved-path (path-only (path->complete-path saved-path)) (wizard-drafts-root root kind)))
  (make-directory* directory)
  (define path (or saved-path (make-temporary-file "draft-~a.json" #f directory)))
  (define temporary (make-temporary-file ".draft-save-~a.json" #f directory))
  (dynamic-wind
    void
    (lambda ()
      (write-json-file temporary (hasheq 'schema_version 1 'kind kind 'saved_at (timestamp)
                                         'step step 'draft draft))
      (rename-file-or-directory temporary path #t)
      path)
    (lambda ()
      (when (file-exists? temporary) (delete-file temporary)))))
