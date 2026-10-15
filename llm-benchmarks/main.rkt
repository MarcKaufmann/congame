#lang racket/base

(require json
         racket/cmdline
         racket/file
         racket/match
         racket/runtime-path
         "private/config.rkt"
         "private/cost.rkt"
         "private/results.rkt"
         "private/run-cli.rkt"
         "private/runner.rkt"
         "private/util.rkt")

(provide main)

(define-runtime-path benchmark-root ".")
(define-runtime-module-path grade-gui-module "private/grade-gui.rkt")
(define-runtime-module-path task-gui-module "private/task-gui.rkt")
(define-runtime-module-path harness-gui-module "private/harness-gui.rkt")
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

  runs [--task ID] [--harness ID] [--ungraded] [--json]
      list recorded runs, newest first

  new-task [--resume <draft.json>] [task-id]
      open the task creation wizard

  new-harness [--resume <draft.json>] [harness-id]
      open the harness creation wizard

  cost <run-id | prefix | latest | all>
      calculate and store costs from saved harness logs

  grade [--cli | --from <grade.json>] [--no-commit] <run-id | prefix | latest>
      open the review UI, use terminal prompts, or import a grade from JSON
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

(define (handle-runs args)
  (define task #f)
  (define harness #f)
  (define ungraded? #f)
  (define json? #f)
  (parameterize ([current-command-line-arguments (list->vector args)])
    (command-line
     #:program "raco congame-llm-bench runs"
     #:once-each
     [("--task") id "filter by task ID" (set! task id)]
     [("--harness") id "filter by harness ID" (set! harness id)]
     [("--ungraded") "only runs without a completed grade" (set! ungraded? #t)]
     [("--json") "emit a JSON array" (set! json? #t)]
     #:args () (void)))
  (define runs (list-runs benchmark-root #:task task #:harness harness
                          #:ungraded? ungraded?))
  (cond
    [json? (write-json runs) (newline)]
    [(null? runs) (displayln "No matching runs.")]
    [else
     (displayln "RUN ID\tOUTCOME\tGRADE")
     (for ([run (in-list runs)])
       (printf "~a\t~a\t~a\n" (hash-ref run 'run_id)
               (hash-ref run 'status) (hash-ref run 'grade)))]))

(define (handle-new-task args)
  (define resume-path #f)
  (define id
    (parameterize ([current-command-line-arguments (list->vector args)])
      (command-line
       #:program "raco congame-llm-bench new-task"
       #:once-each
       [("--resume") path "resume a saved task draft" (set! resume-path path)]
       #:args ([task-id ""]) task-id)))
  (when (and resume-path (not (equal? id "")))
    (error 'new-task "use either a task ID or --resume, not both"))
  ((dynamic-require task-gui-module 'open-task-wizard) benchmark-root id #:resume resume-path)
  (void))

(define (handle-new-harness args)
  (define resume-path #f)
  (define id
    (parameterize ([current-command-line-arguments (list->vector args)])
      (command-line
       #:program "raco congame-llm-bench new-harness"
       #:once-each
       [("--resume") path "resume a saved task draft" (set! resume-path path)]
       #:args ([harness-id ""]) harness-id)))
  (when (and resume-path (not (equal? id "")))
    (error 'new-harness "use either a harness ID or --resume, not both"))
  ((dynamic-require harness-gui-module 'open-harness-wizard) benchmark-root id #:resume resume-path)
  (void))

(define (handle-cost args)
  (define selector
    (parameterize ([current-command-line-arguments (list->vector args)])
      (command-line
       #:program "raco congame-llm-bench cost"
       #:args (run) run)))
  (define ids
    (if (equal? selector "all")
        (map (lambda (run) (hash-ref run 'run_id)) (list-runs benchmark-root))
        (list (resolve-run-id benchmark-root selector))))
  (for ([id (in-list ids)])
    (define path (result-directory benchmark-root id))
    (define manifest (read-json-file (build-path path "manifest.json")))
    (write-run-cost! path (hash-ref manifest 'harness #hasheq()))
    (printf "~a: ~a\n" id (cost-summary path))))

(define (handle-grade args)
  (define commit? #t)
  (define grade-source #f)
  (define cli? #f)
  (define run-id
    (parameterize ([current-command-line-arguments (list->vector args)])
      (command-line
       #:program "raco congame-llm-bench grade"
       #:once-each
       [("--from") path "JSON grade file" (set! grade-source path)]
       [("--cli") "use terminal prompts instead of the review window" (set! cli? #t)]
       [("--no-commit") "do not commit the grade" (set! commit? #f)]
       #:args (run)
       run)))
  (define resolved-id (resolve-run-id benchmark-root run-id))
  (when (and cli? grade-source)
    (error 'grade "--cli and --from cannot be combined"))
  (cond
    [grade-source
     (record-grade! benchmark-root repository-root resolved-id grade-source #:commit? commit?)
     (printf "graded ~a\n" resolved-id)]
    [cli?
     (define grade (prompt-grade benchmark-root resolved-id))
     (define temporary (make-temporary-file "llm-grade-~a.json"))
     (dynamic-wind
       void
       (lambda ()
         (write-json-file temporary grade)
         (record-grade! benchmark-root repository-root resolved-id temporary #:commit? commit?))
       (lambda () (delete-file temporary)))
     (printf "graded ~a\n" resolved-id)]
    [else
     ((dynamic-require grade-gui-module 'open-grader)
      benchmark-root repository-root resolved-id #:commit? commit?)
     (void)]))

(define (main)
  (parameterize ([benchmark-config-root benchmark-root])
    (match (vector->list (current-command-line-arguments))
      [(list) (display-help)]
      [(cons "help" _) (display-help)]
      [(list "list") (handle-list)]
      [(cons "runs" args) (handle-runs args)]
      [(cons "run" args) (handle-run args)]
      [(cons "run-task" args) (handle-run-task args)]
      [(cons "new-task" args) (handle-new-task args)]
      [(cons "new-harness" args) (handle-new-harness args)]
      [(cons "cost" args) (handle-cost args)]
      [(cons "grade" args) (handle-grade args)]
      [(cons command _)
       (eprintf "unknown command: ~a\n\n" command)
       (display-help)
       (exit 1)])))

(module+ main
  (main))
