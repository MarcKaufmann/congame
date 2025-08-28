#lang racket/base

(require net/http-easy
         threading)

(provide
 health-check)

(define (health-check port)
  (define status-code
    (~> (get (format "http://127.0.0.1:~a" port))
        (response-status-code)))
  (unless (= status-code 200)
    (error 'health-check "health check failed")))
