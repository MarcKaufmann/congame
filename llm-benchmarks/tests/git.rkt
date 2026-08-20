#lang racket/base

(require racket/file
         racket/path
         racket/string
         rackunit
         "../private/git.rkt")

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
      (commit-result! root result-path "llm-bench: record run-1")
      (define-values (_show-result show-output _show-error)
        (git-capture root '("show" "--format=" "--name-only" "HEAD")))
      (check-equal? (string-trim show-output)
                    "llm-benchmarks/results/run-1/outcome.json")
      (define-values (_cached-result cached-output _cached-error)
        (git-capture root '("diff" "--cached" "--name-only")))
      (check-equal? (string-trim cached-output) "tracked.txt"))
    (lambda () (delete-directory/files root))))
