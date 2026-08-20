#lang racket/base

(require racket/path
         racket/string
         "process.rkt"
         "util.rkt")

(provide run-checks!)

(define (expand-argument value task-directory repository-root)
  (define with-task
    (string-replace value "${TASK_DIR}" (path-string task-directory)))
  (string-replace with-task "${REPOSITORY_ROOT}" (path-string repository-root)))

(define (run-checks! checks workspace checks-directory environment
                     #:task-directory task-directory
                     #:repository-root repository-root
                     #:timeout-seconds [timeout-seconds 300])
  (ensure-directory! checks-directory)
  (for/list ([check (in-list checks)]
             [index (in-naturals 1)])
    (unless (and (list? check) (pair? check) (andmap string? check))
      (error 'run-checks! "check must be a non-empty array of strings: ~e" check))
    (define expanded
      (for/list ([part (in-list check)])
        (expand-argument part task-directory repository-root)))
    (define name (format "~a" index))
    (define log-path (build-path checks-directory (string-append name ".log")))
    (define result
      (call-with-output-file log-path
        #:exists 'truncate/replace
        (lambda (out)
          (run-command
           (car expanded)
           (cdr expanded)
           #:cwd workspace
           #:environment environment
           #:stdout out
           #:stderr 'stdout
           #:timeout-seconds timeout-seconds))))
    (hasheq
     'command expanded
     'exit_code (command-result-exit-code result)
     'timed_out (command-result-timed-out? result)
     'duration_seconds (command-result-duration-seconds result)
     'log (path->string (file-name-from-path log-path))
     'passed (and (not (command-result-timed-out? result))
                  (zero? (command-result-exit-code result))))))
