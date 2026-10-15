#lang racket/base

(require racket/file
         racket/port
         racket/runtime-path
         rackunit
         "../private/results.rkt"
         "../private/run-cli.rkt"
         "../private/util.rkt")

(define-runtime-path study-rubric "../rubrics/study-v1.json")

(define root (make-temporary-file "run-cli-~a" 'directory))
(dynamic-wind
 void
 (lambda ()
   (test-case "empty results"
     (check-equal? (list-runs root) null)
     (check-exn #rx"no run matches" (lambda () (resolve-run-id root "latest"))))
   (for ([id '("20260101__a" "20260102__b")])
     (define path (create-result-directory! root id))
     (write-result-json! path
                         "manifest.json"
                         (hasheq 'task_id "task" 'harness_id id 'task (hasheq 'rubric "test")))
     (write-pending-grade! path))
   (write-result-json! (result-directory root "20260101__a") "grade.json" (hasheq 'status "complete"))
   (test-case "listing, filters, and selectors"
     (check-equal? (map (lambda (r) (hash-ref r 'run_id)) (list-runs root))
                   '("20260102__b" "20260101__a"))
     (check-equal? (length (list-runs root #:ungraded? #t)) 1)
     (check-equal? (list-runs root #:task "missing") null)
     (check-equal? (length (list-runs root #:harness "20260101__a")) 1)
     (check-equal? (resolve-run-id root "latest") "20260102__b")
     (check-equal? (resolve-run-id root "20260101") "20260101__a")
     (check-exn #rx"ambiguous" (lambda () (resolve-run-id root "2026")))
     (check-exn #rx"no run matches" (lambda () (resolve-run-id root "../escape"))))
   (write-json-file
    (build-path root "rubrics" "test.json")
    (hasheq 'scale (hasheq 'minimum 0 'maximum 4) 'axes (list (hasheq 'id "correctness"))))
   (test-case "interactive scores retry invalid input and record notes"
     (define grade
       (parameterize ([current-input-port (open-input-string "invalid\n5\n3\nGood\nOverall\n")]
                      [current-output-port (open-output-string)])
         (prompt-grade root "20260102__b")))
     (check-equal? (hash-ref grade 'status) "complete")
     (check-equal? (hash-ref (hash-ref (hash-ref grade 'axes) 'correctness) 'score) 3)
     (check-equal? (hash-ref grade 'overall_notes) "Overall")
     (define source (build-path root "grade.json"))
     (write-json-file source grade)
     (record-grade! root root "20260102__b" source #:commit? #f)
     (check-equal? (list-runs root #:ungraded? #t) null))
   (test-case "rubric guidance appears before interactive answers"
     (define rubric (read-json-file study-rubric))
     (write-json-file (build-path root "rubrics" "test.json") rubric)
     (define output (open-output-string))
     (parameterize ([current-input-port
                     (open-input-string
                      (string-append (apply string-append
                                            (for/list ([_axis (in-list (hash-ref rubric 'axes))])
                                              "4\n\n"))
                                     "\n"))]
                    [current-output-port output])
       (prompt-grade root "20260102__b"))
     (define transcript (get-output-string output))
     (check-regexp-match #rx"Reviewer: " transcript)
     (check-regexp-match #rx"0 missing/wrong" transcript)
     (check-regexp-match #rx"4 complete" transcript)
     (for ([axis (in-list (hash-ref rubric 'axes))])
       (check-regexp-match (regexp (regexp-quote (hash-ref axis 'description))) transcript))
     (check-regexp-match #rx"Score \\(0–4, higher is better\\)" transcript))
   (test-case "summary displays failures, timeouts, and missing evidence"
     (define path (result-directory root "20260102__b"))
     (define (render)
       (with-output-to-string (lambda () (display-run-summary path))))
     (check-regexp-match #rx"Checks: unavailable" (render))
     (write-result-json! path
                         "outcome.json"
                         (hasheq 'status "completed" 'scope_violations '("outside.rkt")))
     (write-result-json!
      path
      "checks/summary.json"
      (hasheq
       'upload
       (hasheq 'passed #t)
       'visible
       (list (hasheq 'passed #f 'exit_code 1 'command '("raco" "make" "study.rkt") 'log "1.log"))
       'hidden
       (list (hasheq 'timed_out #t 'log "1.log"))))
     (define output (render))
     (check-regexp-match #rx"PASS upload" output)
     (check-regexp-match #rx"FAIL visible 1: raco make study.rkt" output)
     (check-regexp-match #rx"TIMEOUT hidden 1" output)
     (check-regexp-match #rx"checks/visible/1.log" output)
     (check-regexp-match #rx"Scope violations: outside.rkt" output))
   (test-case "EOF aborts without changing saved grade"
     (parameterize ([current-input-port (open-input-string "")]
                    [current-output-port (open-output-string)])
       (check-exn #rx"grade was not saved" (lambda () (prompt-grade root "20260102__b"))))
     (check-equal? (list-runs root #:ungraded? #t) null)))
 (lambda () (delete-directory/files root)))
