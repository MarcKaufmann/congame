#lang racket/base

(require racket/path
         racket/string
         "process.rkt")

(provide
 commit-result!
 git-capture
 git-head
 git-status
 initialize-workspace-git!)

(define (git-capture cwd args #:check? [check? #t])
  (define-values (result stdout stderr)
    (run-command/capture "git" args #:cwd cwd #:check? check?))
  (values result stdout stderr))

(define (git-head repository-root)
  (define-values (_result stdout _stderr)
    (git-capture repository-root '("rev-parse" "HEAD")))
  (string-trim stdout))

(define (git-status repository-root)
  (define-values (_result stdout _stderr)
    (git-capture repository-root '("status" "--short")))
  stdout)

(define (initialize-workspace-git! workspace)
  (git-capture workspace '("init" "--quiet"))
  (git-capture workspace '("config" "user.name" "Congame LLM Bench"))
  (git-capture workspace '("config" "user.email" "llm-bench@congame.local"))
  (git-capture workspace '("config" "commit.gpgsign" "false"))
  (call-with-output-file (build-path workspace ".git" "info" "exclude")
    #:exists 'append
    (lambda (out)
      (display "\ncompiled/\n**/compiled/\n" out)))
  (git-capture workspace '("add" "--all"))
  (git-capture workspace '("commit" "--quiet" "-m" "benchmark task baseline"))
  (void))

(define (commit-result! repository-root result-path message)
  (define relative-path (find-relative-path repository-root result-path))
  (git-capture repository-root
               (list "add" "--" (path->string relative-path)))
  ;; --only ensures unrelated staged changes are not included.
  (git-capture repository-root
               (list "-c" "commit.gpgsign=false"
                     "-c" "user.name=Congame LLM Bench"
                     "-c" "user.email=llm-bench@congame.local"
                     "commit" "--no-verify" "--only" "-m" message
                     "--" (path->string relative-path)))
  (void))
