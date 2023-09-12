#lang racket/base

(require racket/contract
         racket/list
         "struct.rkt")

(provide
 study-registry-allow-conflicts?

 register-study-mod!
 registered-study-mod-paths
 registered-study-mod-path-indexes
 study-mod-require

 register-study!
 get-registered-studies
 get-registered-bots
 lookup-registered-study

 (struct-out bot-info)
 register-bot!
 get-bot-infos-for-study)

(struct bot-info (id bot models)
  #:transparent)

(define *study-mod-registry*
  (make-hasheq))

(define/contract (registered-study-mod-paths)
  (-> (listof symbol?))
  (hash-keys *study-mod-registry*))

(define/contract (registered-study-mod-path-indexes)
  (-> (listof module-path-index?))
  (remove-duplicates (hash-values *study-mod-registry*)))

(define/contract (study-mod-require mod-path id)
  (-> symbol? symbol? any/c)
  (define (fail-mod)
    (error 'import (format "module not found: ~a" mod-path)))
  (define (fail-id)
    (error 'import (format "module ~a does not provide ~a" mod-path id)))
  (dynamic-require (hash-ref *study-mod-registry* mod-path fail-mod) id fail-id))

(define *study-registry*
  (make-hasheq))

;; hashof study id -> (hashof bot id -> bot-info)
(define *bot-registry*
  (make-hasheq))

(define/contract study-registry-allow-conflicts?
  (parameter/c boolean?)
  (make-parameter #f))

(define/contract (register-study-mod! quoted-mod-path mod-path)
  (-> symbol? module-path-index? void?)
  (hash-set! *study-mod-registry* quoted-mod-path mod-path))

(define/contract (register-study! id s)
  (-> symbol? study? void?)
  (when (and (hash-has-key? *study-registry* id)
             (not (study-registry-allow-conflicts?)))
    (raise-user-error 'register-study! "a study with id ~s is already registered" id))
  (hash-set! *study-registry* id s))

(define/contract (register-bot! id for-study bot models)
  (-> symbol? symbol? any/c (listof procedure?) void?)
  (hash-update! *bot-registry*
                for-study
                (lambda (bots-for-study)
                  (when (and (hash-has-key? bots-for-study id)
                             (not (study-registry-allow-conflicts?)))
                    (raise-user-error 'register-bot! "a bot with id ~s is already registered with study ~s" id for-study))
                  (hash-set bots-for-study id (bot-info id bot (for/hash ([m (in-list models)])
                                                                 (values (object-name m) m)))))
                hasheq))

(define/contract (get-registered-studies)
  (-> (hash/c symbol? any/c))
  (hash-copy *study-registry*))

(define/contract (get-registered-bots)
  (-> (hash/c symbol? any/c))
  (hash-copy *bot-registry*))

(define/contract (get-bot-infos-for-study id)
  (-> symbol? (hash/c symbol? bot-info?))
  (hash-ref *bot-registry* id hasheq))

(define/contract (lookup-registered-study id [failure-thunk (lambda (_id) #f)])
  (->* (symbol?)
       ((-> symbol? any/c))
       (or/c #f any/c))
  (define st
    (hash-ref *study-registry* id #f))
  (or st (failure-thunk id)))
