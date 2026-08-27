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
    (check-equal?
     (hash-ref (load-harness "pi-kimi-k3-openrouter-max")
               'environment_from_host)
     '("OPENROUTER_API_KEY"))
    (check-equal?
     (hash-ref (hash-ref (load-harness "pi-glm-5.3-flash-openrouter-max")
                         'labels)
               'model)
     "z-ai/glm-5.3-flash")
    (check-equal? (hash-ref (load-suite "smoke") 'tasks)
                  '("smoke-study")))
  (test-case "configuration IDs are discoverable"
    (check-not-false (member "smoke-study" (list-config-ids "tasks")))
    (check-not-false
     (member "pi-kimi-k3-openrouter-max" (list-config-ids "harnesses")))
    (check-not-false
     (member "pi-glm-5.3-flash-openrouter-max"
             (list-config-ids "harnesses")))
    (check-not-false (member "smoke" (list-config-ids "suites")))))
