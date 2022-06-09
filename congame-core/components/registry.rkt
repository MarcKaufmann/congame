#lang racket/base

(require racket/contract
         racket/promise)

(provide
 study-registry-allow-conflicts?

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

(define/contract study-registry-allow-conflicts?
  (parameter/c boolean?)
  (make-parameter #f))

(define/contract (register-study! id s)
  (-> symbol? any/c void?)
  (when (and (hash-has-key? *study-registry* id)
             (not (study-registry-allow-conflicts?)))
    (raise-user-error 'register-study! "a study with id ~s is already registered" id))
  (hash-set! *study-registry* id s))

(define/contract (register-bot! id for-study info)
  (-> symbol? symbol? (promise/c bot-info?) void?)
  (hash-update! *bot-registry*
                for-study
                (lambda (bots-for-study)
                  (when (and (hash-has-key? bots-for-study id)
                             (not (study-registry-allow-conflicts?)))
                    (raise-user-error 'register-bot! "a bot with id ~s is already registered with study ~s" id for-study))
                  (hash-set bots-for-study id info))
                hasheq))

(define/contract (get-registered-studies)
  (-> (listof symbol?))
  (hash-keys *study-registry*))

(define/contract (get-bot-infos-for-study id)
  (-> symbol? (hash/c symbol? bot-info?))
  (for/hasheq ([(k p) (in-hash (hash-ref *bot-registry* id hasheq))])
    (values k (force p))))

(define/contract (lookup-registered-study id [failure-thunk (lambda (_id) #f)])
  (->* (symbol?)
       ((-> symbol? any/c))
       (or/c #f any/c))
  (define st
    (hash-ref *study-registry* id #f))
  (if st
      (force st)
      (failure-thunk id)))
