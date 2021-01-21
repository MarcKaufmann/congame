#lang racket/base

(require
 racket/contract)

(provide
 register-study!
 get-registered-studies
 lookup-registered-study

 (struct-out bot-info)
 register-bot!
 get-bot-infos-for-study)

(struct bot-info (id bot models)
  #:transparent)

(define *study-registry*
  (make-hasheq))

;; hashof study id -> (hashof bot id -> bot-info)
(define *bot-registry*
  (make-hasheq))

(define/contract (register-study! id s)
  (-> symbol? any/c void?)
  (when (hash-has-key? *study-registry* id)
    (raise-user-error 'register-study! "a study with id ~s is already registered" id))
  (hash-set! *study-registry* id s))

(define/contract (register-bot! id for-study bot models)
  (-> symbol? symbol? any/c (listof procedure?) void?)
  (hash-update! *bot-registry*
                for-study
                (lambda (bots-for-study)
                  (when (hash-has-key? bots-for-study id)
                    (raise-user-error 'register-bot! "a bot with id ~s is already registered with study ~s" id for-study))
                  (hash-set bots-for-study id (bot-info id bot (for/hash ([m (in-list models)])
                                                                 (values (object-name m) m)))))
                hasheq))

(define/contract (get-registered-studies)
  (-> (hash/c symbol? any/c))
  (hash-copy *study-registry*))

(define/contract (get-bot-infos-for-study id)
  (-> symbol? (hash/c symbol? bot-info?))
  (hash-ref *bot-registry* id hasheq))

(define/contract (lookup-registered-study id [failure-thunk (lambda (id) #f)])
  (->* (symbol?)
       ((-> symbol? any/c))
       (or/c #f any/c))
  (define st
    (hash-ref *study-registry* id #f))
  (or st (failure-thunk id)))
