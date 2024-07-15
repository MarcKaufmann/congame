#lang racket/base

(provide
 clear-known-var-ids!
 ensure-var-id-is-unique!)

(define known-var-ids
  (make-hasheq))

(define (clear-known-var-ids!)
  (hash-clear! known-var-ids))

(define (ensure-var-id-is-unique! who unique-id)
  (if (hash-has-key? known-var-ids unique-id)
      (error 'ensure-var-id-is-unique! "id ~s is not unique~n module: ~s" unique-id who)
      (hash-set! known-var-ids unique-id #t)))
