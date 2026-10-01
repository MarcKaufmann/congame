#lang racket/base

(require rackunit
         "../private/process.rkt")

(define racket-executable
  (path->string (find-system-path 'exec-file)))

(test-case "successful commands report their status"
  (define result
    (run-command racket-executable '("-e" "(void)")))
  (check-equal? (command-result-exit-code result) 0)
  (check-false (command-result-timed-out? result)))

(test-case "commands can stream through a synthetic output port"
  (define output (open-output-string))
  (define result
    (run-command racket-executable '("-e" "(display \"streamed\")")
                 #:stdout output))
  (check-equal? (command-result-exit-code result) 0)
  (check-equal? (get-output-string output) "streamed"))

(test-case "commands are terminated at their deadline"
  (define dev-null (open-output-file "/dev/null" #:exists 'append))
  (dynamic-wind
    void
    (lambda ()
      (define result
        (run-command racket-executable '("-e" "(sleep 5)")
                     #:stdout dev-null
                     #:stderr 'stdout
                     #:timeout-seconds 0.05
                     #:grace-seconds 0.05))
      (check-true (command-result-timed-out? result)))
    (lambda () (close-output-port dev-null))))
