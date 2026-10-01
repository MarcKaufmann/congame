#lang racket/base

(require racket/cmdline
         racket/match
         racket/runtime-path
         "private/config.rkt"
         "private/results.rkt"
         "private/runner.rkt")

(provide main)

(define-runtime-path benchmark-root ".")
(define repository-root (simplify-path (build-path benchmark-root 'up)))

(define (display-help)
  (displayln #<<HELP
usage: raco congame-llm-bench <command> [options]

commands:
  list
      list configured harnesses, tasks, and suites

  run [--no-commit] [--stream-view] <suite>
      run every task/harness/repetition in a suite

  run-task --harness <harness> [--repetitions N] [--no-commit] [--stream-view] <task>
      run one task with one harness

  grade --from <grade.json> [--no-commit] <run-id>
      attach a human grade to an existing result
HELP
  ))

(define (handle-list)
  (for ([kind (in-list '("harnesses" "tasks" "suites"))])
    (printf "~a:\n" kind)
    (for ([id (in-list (list-config-ids kind))])
      (printf "  ~a\n" id))))

(define (handle-run args)
  (define commit? #t)
  (define stream-view? #f)
  (define suite-id
    (parameterize ([current-command-line-arguments (list->vector args)])
      (command-line
       #:program "raco congame-llm-bench run"
       #:once-each
       [("--no-commit") "do not commit result directories" (set! commit? #f)]
       [("--stream-view") "render Pi JSON events as a compact live stream"
        (set! stream-view? #t)]
       #:args (suite)
       suite)))
  (for ([run-id (in-list (run-suite! benchmark-root repository-root suite-id
                                     #:commit? commit?
                                     #:stream-view? stream-view?))])
    (displayln run-id)))

(define (handle-run-task args)
  (define commit? #t)
  (define stream-view? #f)
  (define harness-id #f)
  (define repetitions 1)
  (define task-id
    (parameterize ([current-command-line-arguments (list->vector args)])
      (command-line
       #:program "raco congame-llm-bench run-task"
       #:once-each
       [("--harness") id "harness configuration ID" (set! harness-id id)]
       [("--repetitions") count "number of independent repetitions"
        (set! repetitions (string->number count))]
       [("--no-commit") "do not commit result directories" (set! commit? #f)]
       [("--stream-view") "render Pi JSON events as a compact live stream"
        (set! stream-view? #t)]
       #:args (task)
       task)))
  (unless harness-id
    (error 'run-task "--harness is required"))
  (unless (exact-positive-integer? repetitions)
    (error 'run-task "--repetitions must be a positive integer"))
  (for ([repetition (in-range 1 (add1 repetitions))])
    (displayln
     (run-one! benchmark-root repository-root task-id harness-id repetition
               #:commit? commit?
               #:stream-view? stream-view?))))

(define (handle-grade args)
  (define commit? #t)
  (define grade-source #f)
  (define run-id
    (parameterize ([current-command-line-arguments (list->vector args)])
      (command-line
       #:program "raco congame-llm-bench grade"
       #:once-each
       [("--from") path "JSON grade file" (set! grade-source path)]
       [("--no-commit") "do not commit the grade" (set! commit? #f)]
       #:args (run)
       run)))
  (unless grade-source
    (error 'grade "--from is required"))
  (record-grade! benchmark-root repository-root run-id grade-source #:commit? commit?)
  (printf "graded ~a\n" run-id))

(define (main)
  (parameterize ([benchmark-config-root benchmark-root])
    (match (vector->list (current-command-line-arguments))
      [(list) (display-help)]
      [(cons "help" _) (display-help)]
      [(list "list") (handle-list)]
      [(cons "run" args) (handle-run args)]
      [(cons "run-task" args) (handle-run-task args)]
      [(cons "grade" args) (handle-grade args)]
      [(cons command _)
       (eprintf "unknown command: ~a\n\n" command)
       (display-help)
       (exit 1)])))

(module+ main
  (main))
