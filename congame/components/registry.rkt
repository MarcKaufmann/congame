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

(define/contract (lookup-registered-study id)
  (-> symbol? (or/c #f any/c))
  (hash-ref *registry* id #f))
