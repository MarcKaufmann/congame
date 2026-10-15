#lang racket/base

(require racket/file
         rackunit
         "../private/task-definition.rkt"
         "../private/util.rkt")

(define draft
  (hasheq 'id "new-study" 'title "New study" 'study_id "benchmark-study"
          'prompt "Implement a study." 'rubric "study-v1"
          'allowed_paths "study.rkt\nassets" 'timeout "300"
          'visible_checks "[\"raco\", \"make\", \"study.rkt\"]"
          'hidden_checks ""))

(test-case "wizard creates a loadable task and never overwrites an existing task"
  (define root (make-temporary-file "task-definition-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (write-json-file (build-path root "rubrics" "study-v1.json") (hasheq 'id "study-v1"))
      (define destination (create-task! root draft))
      (define config (read-json-file (build-path destination "config.json")))
      (check-equal? (hash-ref config 'visible_checks) '(("raco" "make" "study.rkt")))
      (check-equal? (file->string (build-path destination "prompt.md")) "Implement a study.")
      (check-equal? (file->string (build-path destination "starter" "study.rkt"))
                    (default-study-source "benchmark-study"))
      (check-true (file-exists? (build-path destination "fixture" "study.rkt")))
      (check-exn #rx"already exists" (lambda () (create-task! root draft)))
      (check-equal? (directory-list (build-path root "tasks")) (list (string->path "new-study"))))
    (lambda () (delete-directory/files root))))

(test-case "invalid task definitions fail before creating files"
  (define root (make-temporary-file "task-invalid-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (write-json-file (build-path root "rubrics" "study-v1.json") (hasheq 'id "study-v1"))
      (for ([invalid (list (hash-set draft 'id "../escape")
                           (hash-set draft 'prompt "")
                           (hash-set draft 'allowed_paths "study.rkt\n../secret")
                           (hash-set draft 'timeout "0")
                           (hash-set draft 'visible_checks "raco make study.rkt"))])
        (check-exn exn:fail? (lambda () (create-task! root invalid))))
      (check-false (directory-exists? (build-path root "tasks"))))
    (lambda () (delete-directory/files root))))

(test-case "custom task sources are copied and failed copies leave no task behind"
  (define root (make-temporary-file "task-copy-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (write-json-file (build-path root "rubrics" "study-v1.json") (hasheq 'id "study-v1"))
      (define fixture (build-path root "source"))
      (make-directory* (build-path fixture "compiled"))
      (display-to-file "custom study" (build-path fixture "study.rkt"))
      (display-to-file "ignore me" (build-path fixture "compiled" "study.zo"))
      (define custom (hash-set draft 'fixture_directory (path->string fixture)))
      (define destination (create-task! root custom))
      (check-equal? (file->string (build-path destination "fixture" "study.rkt")) "custom study")
      (check-false (directory-exists? (build-path destination "fixture" "compiled")))
      (make-file-or-directory-link (build-path fixture "study.rkt") (build-path fixture "link.rkt"))
      (check-exn #rx"symlinks" (lambda () (create-task! root (hash-set custom 'id "failed-copy"))))
      (check-equal? (directory-list (build-path root "tasks")) (list (string->path "new-study"))))
    (lambda () (delete-directory/files root))))
