#lang racket/base

(require racket/port
         rackunit
         (file "../harnesses/pi-glm-5.3-flash-openrouter-max/pi-supervisor.rkt")
         (file "../harnesses/pi-glm-5.3-flash-openrouter-max/stream-watchdog.rkt"))

(define (thinking-delta text)
  (format
   "{\"type\":\"message_update\",\"assistantMessageEvent\":{\"type\":\"thinking_delta\",\"delta\":~s}}\n"
   text))

(define stuck-sample
  (string-append (thinking-delta "\n")
                 (thinking-delta "  \n")
                 (thinking-delta "\n\n")))
(define copied (open-output-string))
(define errors (open-output-string))

(check-true
 (parameterize ([current-error-port errors])
   (guard-pi-stream (open-input-string stuck-sample)
                    copied
                    errors
                    #:limit 3)))
(check-equal? (get-output-string copied) stuck-sample)
(check-regexp-match #rx"3 consecutive whitespace-only" (get-output-string errors))

(define progressing-sample
  (string-append (thinking-delta "\n")
                 (thinking-delta "useful")
                 (thinking-delta "\n")
                 (thinking-delta "\n")))

(check-false
 (guard-pi-stream (open-input-string progressing-sample)
                  (open-output-nowhere)
                  (open-output-nowhere)
                  #:limit 3))

;; The supervisor must stop a noisy child itself rather than close its stdout
;; and make the child crash with EPIPE.
(define racket-executable (find-executable-path "racket"))
(define supervised-output (open-output-string))
(define supervised-errors (open-output-string))
(define producer-expression
  (format
   "(for ([i (in-range 1000)]) (display ~s) (flush-output))"
   (thinking-delta "\n")))

(check-equal?
 (run-guarded-command racket-executable
                      (list "-e" producer-expression)
                      (open-input-string "")
                      supervised-output
                      supervised-errors
                      #:limit 3
                      #:grace-seconds 0.1)
 86)
(check-regexp-match #rx"3 consecutive whitespace-only"
                    (get-output-string supervised-errors))
(check-false (regexp-match? #rx"EPIPE" (get-output-string supervised-errors)))
