#lang racket/base

;; Requires local PostgreSQL and Congame; does not use Docker or an LLM.
(require net/http-easy
         net/url
         racket/file
         racket/runtime-path
         racket/tcp
         rackunit
         "../private/git.rkt"
         "../private/process.rkt"
         "../private/replay.rkt"
         "../private/server.rkt"
         "../private/util.rkt")

(define-runtime-path repository-root "../..")

(test-case "live replay serves the submission and cleans up after normal exit and errors"
  (define root (make-temporary-file "replay-integration-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (define workspace (build-path root "baseline"))
      (copy-directory/files (build-path repository-root "llm-benchmarks" "tasks" "smoke-study" "fixture") workspace)
      (initialize-workspace-git! workspace)
      (define result (build-path root "result"))
      (make-directory result)
      (git-capture workspace (list "bundle" "create" (path-string (build-path result "baseline.bundle")) "HEAD"))
      (display-to-file "" (build-path result "changes.patch"))
      (write-json-file (build-path result "manifest.json")
                       (hasheq 'task (hasheq 'study_id "benchmark-study" 'study_path "study.rkt")))
      (for ([fail? '(#f #t)])
        (define server-url #f)
        (define database #f)
        (define replay-workspace #f)
        (define (replay)
          (call-with-replayed-run
           repository-root result
           (lambda (context restored participant-url)
             (set! server-url (string->url (server-context-url context)))
             (set! database (server-context-database-name context))
             (set! replay-workspace restored)
             (check-equal? (response-status-code (get participant-url)) 200)
             (when fail? (error 'test "callback failure")))))
        (if fail?
            (check-exn #rx"callback failure" replay)
            (replay))
        (check-not-false server-url)
        (check-false (directory-exists? replay-workspace))
        (check-exn exn:fail:network? (lambda () (tcp-connect (url-host server-url) (url-port server-url))))
        (define-values (_result output _error)
          (run-command/capture
           "psql"
           (list "-h" (or (getenv "PGHOST") "127.0.0.1")
                 "-p" (or (getenv "PGPORT") "5432")
                 "-U" (or (getenv "PG_ADMIN_USER") (getenv "USER") "postgres")
                 "-d" "postgres" "-X" "-tA" "-c"
                 (format "SELECT count(*) FROM pg_database WHERE datname = '~a'" database))
           #:environment (make-environment (hasheq 'PGPASSWORD (or (getenv "PG_ADMIN_PASSWORD") (getenv "PGPASSWORD") "congame")))))
        (check-equal? output "0\n")))
    (lambda () (delete-directory/files root))))
