#lang racket/base

(require racket/file
         "git.rkt"
         "util.rkt")

(provide
 create-result-directory!
 record-grade!
 result-directory
 results-root
 write-pending-grade!
 write-result-json!)

(define (results-root benchmark-root)
  (build-path benchmark-root "results"))

(define (result-directory benchmark-root run-id)
  (build-path (results-root benchmark-root) run-id))

(define (create-result-directory! benchmark-root run-id)
  (define path (result-directory benchmark-root run-id))
  (when (directory-exists? path)
    (error 'create-result-directory! "result already exists: ~a" path))
  (make-directory* path)
  path)

(define (write-result-json! result-path name value)
  (write-json-file (build-path result-path name) value))

(define (write-pending-grade! result-path)
  (write-result-json!
   result-path
   "grade.json"
   (hasheq 'status "pending" 'rubric #f 'graded_at #f 'axes #hasheq())))

(define (record-grade! benchmark-root repository-root run-id grade-source #:commit? [commit? #t])
  (define result-path (result-directory benchmark-root run-id))
  (unless (directory-exists? result-path)
    (error 'record-grade! "unknown run: ~a" run-id))
  (define grade-destination (build-path result-path "grade.json"))
  ;; Parse and re-emit to reject invalid JSON and keep formatting stable.
  (write-json-file grade-destination (read-json-file grade-source))
  (when commit?
    (commit-result! repository-root result-path
                    (format "llm-bench: grade ~a" run-id)))
  result-path)
