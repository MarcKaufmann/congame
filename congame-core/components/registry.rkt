#lang racket/base

(require
 racket/contract)

(provide
 register-study!
 get-registered-studies
 lookup-registered-study)

(define *registry*
  (make-hasheq))

(define/contract (register-study! id s)
  (-> symbol? any/c any/c)
  (when (hash-has-key? *registry* id)
    (raise-user-error 'register! "a study with id ~s is already registered" id))
  (hash-set! *registry* id s))

(define/contract (get-registered-studies)
  (-> (hash/c symbol? any/c))
  (hash-copy *registry*))

(define/contract (lookup-registered-study id [failure-thunk (lambda (id) #f)])
  (->* (symbol?)
       ((-> symbol? any/c))
       (or/c #f any/c))
  (define st
    (hash-ref *registry* id #f))
  (or st (failure-thunk id)))
