#lang racket/base

(require racket/file
         racket/path
         rackunit
         "../private/git.rkt"
         "../private/workspace.rkt")

(define (write-text path text)
  (make-directory* (or (path-only path) (current-directory)))
  (call-with-output-file path
    #:exists 'truncate/replace
    (lambda (out) (display text out))))

(test-case "prepared workspaces omit compiled artifacts"
  (define root (make-temporary-file "llm-bench-prepare-test-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (define task-directory (build-path root "task"))
      (define fixture (build-path task-directory "fixture"))
      (define run-root (build-path root "run"))
      (make-directory* (build-path fixture "compiled"))
      (write-text (build-path fixture "study.rkt") "#lang racket/base\n")
      (write-text (build-path fixture "compiled" "study_rkt.zo") "generated\n")
      (write-text (build-path task-directory "prompt.md") "Do the task.\n")
      (define workspace
        (prepare-workspace!
         root
         run-root
         (hasheq 'fixture "fixture"
                 'prompt "prompt.md"
                 'study_id "test-study")
         task-directory
         #:server-url "http://127.0.0.1:5100"
         #:instance-slug "test-instance"))
      (check-true (file-exists? (build-path workspace "study.rkt")))
      (check-regexp-match
       #rx"CLI authentication is already configured"
       (file->string (build-path workspace "TASK.md")))
      (check-false (directory-exists? (build-path workspace "compiled"))))
    (lambda () (delete-directory/files root))))

(test-case "workspace snapshots replay edits and new files"
  (define root (make-temporary-file "llm-bench-workspace-test-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (define fixture (build-path root "fixture"))
      (define workspace (build-path root "workspace"))
      (define result (build-path root "result"))
      (define replay (build-path root "replay"))
      (make-directory* fixture)
      (make-directory* result)
      (write-text (build-path fixture "study.rkt") "before\n")
      (copy-directory/files fixture workspace)
      (initialize-workspace-git! workspace)
      (write-text (build-path workspace "study.rkt") "after\n")
      (write-text (build-path workspace "assets" "note.txt") "new\n")
      (write-text (build-path workspace "compiled" "study_rkt.zo") "generated\n")
      (define snapshot
        (snapshot-workspace! workspace result '("study.rkt" "assets")))
      (check-equal? (workspace-snapshot-violations snapshot) null)
      (check-equal? (sort (workspace-snapshot-changed-paths snapshot) string<?)
                    '("assets/note.txt" "study.rkt"))
      (apply-snapshot! fixture replay (workspace-snapshot-patch-path snapshot))
      (check-equal? (file->string (build-path replay "study.rkt")) "after\n")
      (check-equal? (file->string (build-path replay "assets" "note.txt")) "new\n"))
    (lambda () (delete-directory/files root))))

(test-case "workspace snapshots report scope violations"
  (define root (make-temporary-file "llm-bench-scope-test-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (define workspace (build-path root "workspace"))
      (define result (build-path root "result"))
      (make-directory* workspace)
      (make-directory* result)
      (write-text (build-path workspace "study.rkt") "before\n")
      (initialize-workspace-git! workspace)
      (write-text (build-path workspace "TASK.md") "changed\n")
      (define snapshot
        (snapshot-workspace! workspace result '("study.rkt")))
      (check-equal? (workspace-snapshot-violations snapshot) '("TASK.md")))
    (lambda () (delete-directory/files root))))
