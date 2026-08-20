#lang racket/base

(require racket/runtime-path
         rackunit
         "../private/config.rkt")

(define-runtime-path benchmark-root "..")

(parameterize ([benchmark-config-root benchmark-root])
  (test-case "checked-in configurations load"
    (check-equal? (hash-ref (load-task "smoke-study") 'study_id)
                  "benchmark-study")
    (check-equal? (hash-ref (load-harness "pi-qwen3.8-27b-mlx-medium") 'script)
                  "run.sh")
    (check-equal? (hash-ref (load-suite "smoke") 'tasks)
                  '("smoke-study")))
  (test-case "configuration IDs are discoverable"
    (check-not-false (member "smoke-study" (list-config-ids "tasks")))
    (check-not-false (member "smoke" (list-config-ids "suites")))))
