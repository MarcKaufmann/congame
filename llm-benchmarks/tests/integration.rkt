#lang racket/base

;; Host-service integration test. This intentionally is not required by all.rkt:
;; it creates and drops two PostgreSQL databases and starts Congame twice, but
;; it does not contact an LLM.

(require racket/file
         racket/path
         racket/runtime-path
         rackunit
         "../private/config.rkt"
         "../private/runner.rkt"
         "../private/util.rkt")

(define-runtime-path tests-root ".")
(define repository-root
  (simplify-path (build-path tests-root 'up 'up)))
(define source-benchmark-root
  (build-path repository-root "llm-benchmarks"))

(define (write-text path text)
  (make-directory* (or (path-only path) (current-directory)))
  (call-with-output-file path
    #:exists 'truncate/replace
    (lambda (out) (display text out))))

(test-case "complete runner lifecycle without an LLM"
  (define benchmark-root
    (make-temporary-file "congame-llm-integration-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (make-directory* (build-path benchmark-root "tasks"))
      (copy-directory/files
       (build-path source-benchmark-root "tasks" "smoke-study")
       (build-path benchmark-root "tasks" "smoke-study"))
      (define harness-root (build-path benchmark-root "harnesses" "noop"))
      (make-directory* harness-root)
      (write-json-file
       (build-path harness-root "config.json")
       (hasheq
        'schema_version 1
        'id "noop"
        'script "run.sh"
        'docker (hasheq 'image "debian:bullseye-slim"
                        'cpus 1
                        'memory "256m"
                        'pids_limit 32)
        'limits (hasheq 'wall_seconds 10 'termination_grace_seconds 1)))
      (write-text (build-path harness-root "run.sh")
                  "#!/usr/bin/env bash\nset -euo pipefail\necho noop\n")
      (make-directory* (build-path benchmark-root "results"))
      (parameterize ([benchmark-config-root benchmark-root])
        (define run-id
          (run-one! benchmark-root repository-root "smoke-study" "noop" 1
                    #:commit? #f))
        (define outcome
          (read-json-file (build-path benchmark-root "results" run-id "outcome.json")))
        (check-equal? (hash-ref outcome 'status) "completed")
        (check-true
         (hash-ref (hash-ref (hash-ref outcome 'checks) 'upload) 'passed))))
    (lambda () (delete-directory/files benchmark-root))))
