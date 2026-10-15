#lang racket/base

(require racket/list
         racket/path
         racket/string
         "cost.rkt"
         "git.rkt"
         "results.rkt"
         "util.rkt")

(provide
 default-reviewer
 display-run-summary
 list-runs
 prompt-grade
 resolve-run-id)

(define (default-reviewer root)
  (or (with-handlers ([exn:fail? (lambda (_) #f)])
        (define-values (_result stdout _stderr)
          (git-capture root '("config" "user.name") #:check? #f))
        (define name (string-trim stdout))
        (and (non-empty-string? name) name))
      (for/or ([variable '("USER" "LOGNAME" "USERNAME")])
        (define name (getenv variable))
        (and name (non-empty-string? (string-trim name)) (string-trim name)))
      "reviewer"))

(define (display-run-summary path)
  (define outcome (optional-json (build-path path "outcome.json")))
  (define summary-path (build-path path "checks" "summary.json"))
  (define checks
    (if (file-exists? summary-path)
        (read-json-file summary-path)
        (hash-ref outcome 'checks #f)))
  (printf "Outcome: ~a~a\n"
          (hash-ref outcome 'status "unavailable")
          (if (hash-ref outcome 'timed_out #f) " (timed out)" ""))
  (when (hash-ref outcome 'error #f)
    (printf "Error: ~a\n" (hash-ref outcome 'error)))
  (printf "Total cost: ~a\n" (cost-summary path))
  (define violations (hash-ref outcome 'scope_violations #f))
  (printf "Scope violations: ~a\n"
          (cond
            [(not violations) "unknown"]
            [(null? violations) "none"]
            [else (string-join violations ", ")]))
  (define (show-check label check log-directory)
    (define status
      (cond
        [(not (hash? check)) "NOT RUN"]
        [(hash-ref check 'timed_out #f) "TIMEOUT"]
        [(hash-ref check 'passed #f) "PASS"]
        [(number? (hash-ref check 'exit_code #f)) "FAIL"]
        [else "NOT RUN"]))
    (define command
      (if (hash? check)
          (hash-ref check 'command null)
          null))
    (printf "  ~a ~a~a\n"
            status
            label
            (if (null? command)
                ""
                (format ": ~a"
                        (string-join (for/list ([arg (in-list command)])
                                       (if (absolute-path? arg)
                                           (path->string (file-name-from-path arg))
                                           arg))
                                     " "))))
    (when (and (hash? check) (member status '("FAIL" "TIMEOUT")))
      (printf "    exit: ~a; log: ~a/~a\n"
              (hash-ref check 'exit_code "unknown")
              log-directory
              (hash-ref check 'log "unknown"))))
  (cond
    [(hash? checks)
     (show-check "upload" (hash-ref checks 'upload #f) "checks")
     (for ([kind '(visible hidden)])
       (define entries (hash-ref checks kind null))
       (when (null? entries)
         (printf "  ~a: no results\n" kind))
       (for ([check (in-list entries)]
             [index (in-naturals 1)])
         (show-check (format "~a ~a" kind index) check (format "checks/~a" kind))))]
    [else (displayln "Checks: unavailable (not run or not recorded).")]))

(define (optional-json path)
  (if (file-exists? path)
      (read-json-file path)
      #hasheq()))

(define (list-runs root #:task [task #f] #:harness [harness #f] #:ungraded? [ungraded? #f])
  (define directory (results-root root))
  (if (not (directory-exists? directory))
      null
      (for/list ([entry (in-list (sort (directory-list directory) string>? #:key path->string))]
                 #:when (file-exists? (build-path directory entry "manifest.json"))
                 #:do [(define path (build-path directory entry))
                       (define manifest (read-json-file (build-path path "manifest.json")))
                       (define outcome (optional-json (build-path path "outcome.json")))
                       (define grade (optional-json (build-path path "grade.json")))]
                 #:when (and (or (not task) (equal? task (hash-ref manifest 'task_id #f)))
                             (or (not harness) (equal? harness (hash-ref manifest 'harness_id #f)))
                             (or (not ungraded?)
                                 (not (equal? "complete" (hash-ref grade 'status #f))))))
        (hasheq
         'run_id (path->string entry)
         'task (hash-ref manifest 'task_id "unknown")
         'harness (hash-ref manifest 'harness_id "unknown")
         'status (hash-ref outcome 'status "in-progress")
         'grade (hash-ref grade 'status "pending")))))

(define (resolve-run-id root selector)
  (define ids (map (lambda (run) (hash-ref run 'run_id)) (list-runs root)))
  (define matches
    (cond
      [(equal? selector "latest") (take ids (min 1 (length ids)))]
      [(member selector ids) (list selector)]
      [else (filter (lambda (id) (string-prefix? id selector)) ids)]))
  (cond
    [(null? matches) (error 'grade "no run matches: ~a" selector)]
    [(pair? (cdr matches))
     (error 'grade "ambiguous run: ~a\n  ~a" selector (string-join matches "\n  "))]
    [else (car matches)]))

(define (prompt-line label)
  (display label)
  (flush-output)
  (define value (read-line))
  (when (eof-object? value)
    (error 'grade "input ended; grade was not saved"))
  (string-trim value))

(define (prompt-grade root run-id)
  (define path (result-directory root run-id))
  (define manifest (read-json-file (build-path path "manifest.json")))
  (define rubric-id (hash-ref (hash-ref manifest 'task) 'rubric))
  (define rubric (read-json-file (build-path root "rubrics" (string-append rubric-id ".json"))))
  (define scale (hash-ref rubric 'scale))
  (define minimum (hash-ref scale 'minimum))
  (define maximum (hash-ref scale 'maximum))
  (printf "Run: ~a\nReview: ~a\nRubric: ~a (~a–~a)\n"
          run-id
          (build-path path "review" "README.md")
          rubric-id
          minimum
          maximum)
  (display-run-summary path)
  (displayln "\nReview: TASK.md → review/files/ → stdout.log. Check results are shown above.")
  (displayln "Compare requirements with code, check results, and actual agent verification.")
  (displayln "Scale: 0 missing/wrong · 1 major gaps · 2 partial · 3 minor gaps · 4 complete.")
  (displayln
   "Notes optional. Ctrl-C cancels without saving. See llm-benchmarks/README.md for details.")
  (define grader (default-reviewer root))
  (printf "Reviewer: ~a\n" grader)
  (define axes
    (for/hasheq ([axis (in-list (hash-ref rubric 'axes))])
      (define id (hash-ref axis 'id))
      (printf "\n~a\n" (hash-ref axis 'title id))
      (displayln (hash-ref axis
                           'description
                           "Assess this criterion against the task requirements and saved evidence."))
      (define score
        (let loop ()
          (define value
            (string->number
             (prompt-line (format "Score (~a–~a, higher is better): " minimum maximum))))
          (cond
            [(and (real? value) (<= minimum value maximum)) value]
            [else
             (printf "Enter a number from ~a to ~a.\n" minimum maximum)
             (loop)])))
      (values
       (string->symbol id)
       (hasheq 'score score
               'notes (prompt-line "  Evidence or reason for this score (optional): ")))))
  (hasheq
   'status "complete"
   'rubric rubric-id
   'graded_at (timestamp)
   'grader grader
   'axes axes
   'overall_notes (prompt-line "Overall assessment, limitations, or follow-up (optional): ")))
