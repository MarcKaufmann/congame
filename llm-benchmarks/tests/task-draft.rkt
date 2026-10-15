#lang racket/base

(require racket/file
         rackunit
         "../private/util.rkt"
         "../private/wizard-draft.rkt")

(test-case "incomplete drafts round-trip all fields and resume the current step"
  (define root (make-temporary-file "task-draft-test-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (define draft (hasheq 'id "" 'prompt "unfinished\ntext" 'visible_checks "["))
      (define path (save-wizard-draft! root draft 2))
      (define saved (load-wizard-draft path))
      (check-equal? (hash-ref saved 'draft) draft)
      (check-equal? (hash-ref saved 'step) 2)
      (check-false (directory-exists? (build-path root "tasks")))
      (define updated (hash-set draft 'title "Changed title"))
      (check-equal? (save-wizard-draft! root updated 3 #:path path) path)
      (check-equal? (hash-ref (load-wizard-draft path) 'draft) updated)
      (check-equal? (hash-ref (load-wizard-draft path) 'step) 3)
      (check-equal? (length (directory-list (wizard-drafts-root root))) 1))
    (lambda () (delete-directory/files root))))

(test-case "unrelated JSON is rejected as a draft"
  (define root (make-temporary-file "draft-invalid-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (define path (build-path root "not-a-draft.json"))
      (write-json-file path (hasheq 'schema_version 1 'step 99 'draft #hasheq()))
      (check-exn #rx"Not a supported task draft" (lambda () (load-wizard-draft path))))
    (lambda () (delete-directory/files root))))
