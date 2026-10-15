#lang racket/base

(require file/sha1
         json
         racket/format
         racket/list
         racket/string
         "results.rkt"
         "run-cli.rkt"
         "util.rkt")

(provide
 compare-runs
 display-comparison)

(define (optional-json path)
  (if (file-exists? path) (read-json-file path) #hasheq()))

(define (finite-number? value)
  (and (real? value) (< -inf.0 value +inf.0)))

(define (mean values)
  (and (pair? values) (exact->inexact (/ (apply + values) (length values)))))

;; Hash iteration order is not part of JSON. Sort keys before fingerprinting
;; configurations so IDs remain stable between invocations.
(define (canonical value)
  (cond
    [(hash? value)
     (for/list ([key (in-list (sort (hash-keys value) symbol<?))])
       (list (symbol->string key) (canonical (hash-ref value key))))]
    [(list? value) (map canonical value)]
    [else value]))

(define (fingerprint value)
  (substring (sha1 (open-input-string (jsexpr->string (canonical value)))) 0 8))

(define (grade-score grade rubric)
  (define scale (hash-ref rubric 'scale #hasheq()))
  (define minimum (hash-ref scale 'minimum #f))
  (define maximum (hash-ref scale 'maximum #f))
  (define axes (hash-ref rubric 'axes null))
  (define scores (hash-ref grade 'axes #hasheq()))
  (and (equal? (hash-ref grade 'status #f) "complete")
       (equal? (hash-ref grade 'rubric #f) (hash-ref rubric 'id #f))
       (finite-number? minimum) (finite-number? maximum) (< minimum maximum)
       (pair? axes) (hash? scores)
       (for/and ([axis (in-list axes)])
         (define entry (hash-ref scores (string->symbol (hash-ref axis 'id)) #hasheq()))
         (define score (and (hash? entry) (hash-ref entry 'score #f)))
         (define weight (hash-ref axis 'weight 1))
         (and (finite-number? score) (<= minimum score maximum)
              (finite-number? weight) (>= weight 0)))
       (let ([weight (for/sum ([axis (in-list axes)]) (hash-ref axis 'weight 1))])
         (and (positive? weight)
              (* 100.0
                 (/ (for/sum ([axis (in-list axes)])
                      (* (hash-ref axis 'weight 1)
                         (- (hash-ref (hash-ref scores (string->symbol (hash-ref axis 'id))) 'score)
                            minimum)))
                    (* weight (- maximum minimum))))))))

(define (checks-passed? outcome task)
  (define checks (hash-ref outcome 'checks #f))
  (and (hash? checks)
       (let ([upload (hash-ref checks 'upload #hasheq())])
         (and (hash-ref upload 'passed #f) (not (hash-ref upload 'timed_out #f))))
       (for/and ([kind '(visible hidden)])
         (define results (hash-ref checks kind null))
         (define declared (hash-ref task (if (eq? kind 'visible) 'visible_checks 'hidden_checks) null))
         (and (= (length results) (length declared))
              (for/and ([result (in-list results)])
                (and (hash-ref result 'passed #f) (not (hash-ref result 'timed_out #f))))))))

(define (compare-runs root #:task [task-filter #f] #:harness [harness-filter #f])
  (define groups (make-hash))
  (for ([run (in-list (list-runs root #:task task-filter #:harness harness-filter))])
    (define path (result-directory root (hash-ref run 'run_id)))
    (define manifest (read-json-file (build-path path "manifest.json")))
    (define task (hash-ref manifest 'task))
    (define rubric-id (hash-ref task 'rubric))
    (define saved-rubric (build-path path "rubric.json"))
    (define rubric
      (optional-json
       (if (file-exists? saved-rubric)
           saved-rubric
           (build-path root "rubrics" (string-append rubric-id ".json")))))
    (define key
      (list (hash-ref manifest 'task_id) (hash-ref manifest 'task_version)
            (hash-ref manifest 'prompt_sha1 #f) task rubric
            (hash-ref manifest 'harness_id) (hash-ref manifest 'harness)))
    (define grade (optional-json (build-path path "grade.json")))
    (define row
      (hasheq 'run_id (hash-ref run 'run_id)
              'outcome (optional-json (build-path path "outcome.json"))
              'grade grade 'score (grade-score grade rubric)
              'cost (optional-json (build-path path "cost.json"))
              'rubric_fallback (not (file-exists? saved-rubric))))
    (hash-update! groups key (lambda (rows) (cons row rows)) null))
  (define summaries
    (for/list ([(key rows) (in-hash groups)])
      (define task (list-ref key 3))
      (define rubric (list-ref key 4))
      (define scores (filter finite-number? (map (lambda (row) (hash-ref row 'score)) rows)))
      (define costs (map (lambda (row) (hash-ref row 'cost)) rows))
      (define (costs-with-status status)
        (for/list ([cost (in-list costs)]
                   #:when (and (equal? (hash-ref cost 'status #f) status)
                               (equal? (hash-ref cost 'currency #f) "USD")
                               (finite-number? (hash-ref cost 'total_cost_usd #f))
                               (>= (hash-ref cost 'total_cost_usd) 0)))
          (hash-ref cost 'total_cost_usd)))
      (define reported (costs-with-status "reported"))
      (define partial (costs-with-status "partial"))
      (define outcomes (map (lambda (row) (hash-ref row 'outcome)) rows))
      (define durations
        (filter finite-number? (map (lambda (outcome) (hash-ref outcome 'duration_seconds #f)) outcomes)))
      (hasheq
       'task (car key) 'task_version (cadr key)
       'task_config (fingerprint (take key 5))
       'harness (list-ref key 5) 'harness_config (fingerprint (list-ref key 6))
       'rubric (hash-ref task 'rubric)
       'runs (length rows) 'run_ids (sort (map (lambda (row) (hash-ref row 'run_id)) rows) string<?)
       'graded (length scores) 'score_percent (mean scores)
       'score_min (and (pair? scores) (apply min scores))
       'score_max (and (pair? scores) (apply max scores))
       'invalid_grades (count (lambda (row) (and (equal? (hash-ref (hash-ref row 'grade) 'status #f) "complete")
                                                (not (hash-ref row 'score)))) rows)
       'rubric_fallback_runs (count (lambda (row) (hash-ref row 'rubric_fallback)) rows)
       'axes (for/hasheq ([axis (in-list (hash-ref rubric 'axes null))])
               (define id (string->symbol (hash-ref axis 'id)))
               (values id (mean (for/list ([row (in-list rows)] #:when (hash-ref row 'score))
                                  (hash-ref (hash-ref (hash-ref (hash-ref row 'grade) 'axes) id) 'score)))))
       'checks_passed (count (lambda (outcome) (checks-passed? outcome task)) outcomes)
       'outcomes (for/fold ([counts #hasheq()]) ([outcome (in-list outcomes)])
                   (hash-update counts (string->symbol (hash-ref outcome 'status "in-progress")) add1 0))
       'mean_duration_seconds (mean durations)
       'cost_reported_runs (length reported) 'cost_partial_runs (length partial)
       'cost_unavailable_runs (- (length rows) (length reported) (length partial))
       'mean_cost_usd (mean reported)
       'reported_cost_usd (and (pair? reported) (exact->inexact (apply + reported)))
       'partial_cost_usd (and (pair? partial) (exact->inexact (apply + partial))))))
  (sort summaries string<?
        #:key (lambda (row)
                (format "~a/~a/~a/~a/~a" (hash-ref row 'task) (hash-ref row 'task_version)
                        (hash-ref row 'task_config) (hash-ref row 'harness) (hash-ref row 'harness_config)))))

(define (display-comparison rows)
  (cond
    [(null? rows) (displayln "No matching runs.")]
    [else
     (define headers '("TASK (VERSION/CONFIG)" "HARNESS (CONFIG)" "RUNS" "GRADED" "SCORE %" "CHECKS" "MEAN USD" "COST R/P/?"))
     (define cells
       (for/list ([row (in-list rows)])
         (define (number key precision)
           (define value (hash-ref row key))
           (if value (~r value #:precision (list '= precision)) "—"))
         (list (format "~a (v~a/~a)" (hash-ref row 'task) (hash-ref row 'task_version) (hash-ref row 'task_config))
               (format "~a (~a)" (hash-ref row 'harness) (hash-ref row 'harness_config))
               (~a (hash-ref row 'runs)) (~a (hash-ref row 'graded)) (number 'score_percent 1)
               (format "~a/~a" (hash-ref row 'checks_passed) (hash-ref row 'runs))
               (number 'mean_cost_usd 4)
               (format "~a/~a/~a" (hash-ref row 'cost_reported_runs)
                       (hash-ref row 'cost_partial_runs) (hash-ref row 'cost_unavailable_runs)))))
     (define widths
       (for/list ([index (in-range (length headers))])
         (apply max (map (lambda (row) (string-length (list-ref row index))) (cons headers cells)))))
     (for ([row (in-list (cons headers cells))])
       (displayln
        (string-join
         (for/list ([cell (in-list row)] [width (in-list widths)] [index (in-naturals)])
           (~a cell #:min-width width #:align (if (< index 2) 'left 'right)))
         "  ")))
     (displayln "\nScores: rubric-weighted, normalized to 0–100. Cost means exclude partial/unavailable runs.")
     (displayln "Cost R/P/?: reported / partial / unavailable. Checks include upload; completion alone is not a pass.")
     (define fallback (for/sum ([row (in-list rows)]) (hash-ref row 'rubric_fallback_runs)))
     (when (positive? fallback)
       (printf "~a older run(s) use the current rubric because no rubric snapshot was saved.\n" fallback))
     (define invalid (for/sum ([row (in-list rows)]) (hash-ref row 'invalid_grades)))
     (when (positive? invalid)
       (printf "~a invalid or incomplete completed grade(s) excluded from scores.\n" invalid))]))
