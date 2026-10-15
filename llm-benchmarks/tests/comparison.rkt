#lang racket/base

(require racket/file
         rackunit
         "../private/comparison.rkt"
         "../private/util.rkt")

(define rubric
  (hasheq 'id "test" 'scale (hasheq 'minimum 0 'maximum 4)
          'axes (list (hasheq 'id "correct" 'weight 3) (hasheq 'id "quality" 'weight 1))))
(define task (hasheq 'rubric "test" 'visible_checks (list (list "check")) 'hidden_checks null))
(define grade
  (hasheq 'status "complete" 'rubric "test"
          'axes (hasheq 'correct (hasheq 'score 4) 'quality (hasheq 'score 0))))
(define outcome
  (hasheq 'status "completed" 'duration_seconds 10
          'checks (hasheq 'upload (hasheq 'passed #t)
                          'visible (list (hasheq 'passed #t)) 'hidden null)))

(define (with-root proc)
  (define root (make-temporary-file "comparison-test-~a" 'directory))
  (dynamic-wind void (lambda () (proc root)) (lambda () (delete-directory/files root))))

(define (write-run root id #:grade [g grade] #:cost [cost #f] #:outcome [o outcome]
                   #:version [version 1] #:model [model "one"])
  (define path (build-path root "results" id))
  (write-json-file (build-path path "manifest.json")
                   (hasheq 'task_id "task" 'task_version version 'task task 'prompt_sha1 "prompt"
                           'harness_id "agent" 'harness (hasheq 'model model)))
  (write-json-file (build-path path "rubric.json") rubric)
  (write-json-file (build-path path "grade.json") g)
  (write-json-file (build-path path "outcome.json") o)
  (when cost (write-json-file (build-path path "cost.json") cost)))

(test-case "comparison excludes pending grades and incomplete costs without losing coverage"
  (with-root
   (lambda (root)
     (write-run root "a" #:cost (hasheq 'status "reported" 'currency "USD" 'total_cost_usd 0))
     (write-run root "b" #:grade (hasheq 'status "pending")
                #:cost (hasheq 'status "partial" 'currency "USD" 'total_cost_usd 2)
                #:outcome (hasheq 'status "timed_out"))
     (write-run root "c" #:grade (hash-set grade 'axes #hasheq()))
     (define row (car (compare-runs root)))
     (check-equal? (hash-ref row 'runs) 3)
     (check-equal? (hash-ref row 'graded) 1)
     (check-equal? (hash-ref row 'score_percent) 75.0)
     (check-equal? (hash-ref row 'checks_passed) 2)
     (check-equal? (hash-ref row 'mean_cost_usd) 0.0)
     (check-equal? (hash-ref row 'cost_reported_runs) 1)
     (check-equal? (hash-ref row 'cost_partial_runs) 1)
     (check-equal? (hash-ref row 'cost_unavailable_runs) 1)
     (check-equal? (hash-ref row 'invalid_grades) 1)
     (check-equal? (hash-ref (hash-ref row 'axes) 'correct) 4.0)
     (check-equal? (compare-runs root #:task "other") null))))

(test-case "task versions and harness configurations remain separate"
  (with-root
   (lambda (root)
     (write-run root "a")
     (write-run root "b" #:version 2)
     (write-run root "c" #:model "two")
     (check-equal? (length (compare-runs root)) 3))))

(test-case "missing checks and absent grades are not passes or zero scores"
  (with-root
   (lambda (root)
     (write-run root "a" #:grade (hasheq 'status "pending")
                #:outcome (hasheq 'status "completed" 'checks (hasheq 'upload (hasheq 'passed #t))))
     (define row (car (compare-runs root)))
     (check-false (hash-ref row 'score_percent))
     (check-false (hash-ref row 'mean_cost_usd))
     (check-equal? (hash-ref row 'checks_passed) 0))))
