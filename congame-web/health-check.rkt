#lang racket/base

(require net/http-easy
         threading)

(provide
 health-check)

(define (health-check port)
  (let loop ([gas 15])
    (with-handlers* ([exn:fail?
                      (lambda (e)
                        (cond
                          [(zero? gas) (raise e)]
                          [else (sleep 1)
                                (loop (sub1 gas))]))])
      (define status-code
        (~> (get (format "http://127.0.0.1:~a" port))
            (response-status-code)))
      (unless (= status-code 200)
        (error 'health-check "health check failed")))))
