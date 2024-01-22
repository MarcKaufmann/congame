#lang racket/base

(provide
 wrap-memory-limit)

(define (((wrap-memory-limit limit) handler) req)
  (define cust (make-custodian))
  (custodian-limit-memory cust limit)
  (call-in-nested-thread
   (lambda ()
     (handler req))
   cust))
