#lang racket/base

(require racket/file
         racket/path
         racket/string
         rackunit
         "../private/git.rkt"
         "../private/results.rkt"
         "../private/util.rkt")

(define (write-text path text)
  (make-directory* (or (path-only path) (current-directory)))
  (call-with-output-file path
    #:exists 'truncate/replace
    (lambda (out) (display text out))))

(test-case "result commits exclude unrelated staged changes"
  (define root (make-temporary-file "llm-bench-git-test-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (write-text (build-path root "tracked.txt") "baseline\n")
      (initialize-workspace-git! root)
      (write-text (build-path root "tracked.txt") "unrelated\n")
      (define-values (_stage-result _stage-out _stage-err)
        (git-capture root '("add" "tracked.txt")))
      (define result-path (build-path root "llm-benchmarks" "results" "run-1"))
      (write-text (build-path result-path "outcome.json") "{}\n")
      (commit-result! (build-path root 'same) (build-path result-path 'same)
                      "llm-bench: record run-1")
      (define-values (_show-result show-output _show-error)
        (git-capture root '("show" "--format=" "--name-only" "HEAD")))
      (check-equal? (string-trim show-output)
                    "llm-benchmarks/results/run-1/outcome.json")
      (define-values (_cached-result cached-output _cached-error)
        (git-capture root '("diff" "--cached" "--name-only")))
      (check-equal? (string-trim cached-output) "tracked.txt")
      ;; Re-import a saved grade in place after a failed commit, with the same
      ;; unsimplified runtime path used by the CLI.
      (define grade-path (build-path result-path "grade.json"))
      (define grade (hasheq 'status "complete" 'grader "Reviewer"))
      (write-json-file grade-path grade)
      (record-grade! (build-path root "llm-benchmarks" 'same)
                     root "run-1" grade-path)
      (check-equal? (read-json-file grade-path) grade)
      (define-values (_grade-result grade-output _grade-error)
        (git-capture root '("show" "--format=" "--name-only" "HEAD")))
      (check-equal? (string-trim grade-output)
                    "llm-benchmarks/results/run-1/grade.json"))
    (lambda () (delete-directory/files root))))
