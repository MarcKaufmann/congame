#lang racket/base

(require buid
         congame/components/study
         koyo/database
         racket/contract/base
         racket/match)

(provide
 (contract-out
  [make-matchmaker
   (->* [exact-positive-integer?]
        [(-> buid/c boolean?)]
        procedure?)]
  [get-ready-groups (-> (hash/c buid/c (listof id/c)))]
  [get-current-group (-> (or/c #f buid/c))]
  [get-pending-groups (-> (hash/c buid/c (listof id/c)))]
  [reset-current-group (-> void?)]
  
  [current-group-members (->* () (#:include-self? any/c) (or/c #f (listof id/c)))]
  
  [store-my-result-in-group! (-> any/c any/c void?)]
  [get-my-result-in-group (-> any/c any/c)]
  [current-group-results-count (->* (any/c) (#:include-self? any/c) exact-nonnegative-integer?)]
  [current-group-member-results (->* (any/c) (#:include-ids? any/c
                                              #:include-self? any/c)
                                     (or/c (listof (cons/c id/c any/c))
                                           (listof any/c)))]))

;; TODO: Namespace these vars?

;; These are study-scoped in order for the parent and the child to be
;; able to do their own matchmaking. If a parent wants to share the
;; current group with a child, it needs to store it in a separate var*
;; and share that with the child and vice-versa.
(defvar/instance pending-groups)
(defvar/instance ready-groups)
(defvar current-group)

(define (get-ready-groups)
  (if-undefined ready-groups (hash)))

(define (get-current-group)
  (if-undefined current-group #f))

(define (reset-current-group)
  (set! current-group #f))

(define (current-group-members #:include-self? [include-self? #f])
  (define my-group-id (get-current-group))
  (define all-group-members
    (and my-group-id
         (hash-ref (get-ready-groups) my-group-id #f)))
  (define others
    (and all-group-members
         (or (and include-self? all-group-members)
             (filter (λ (v) (not (equal? v (current-participant-id)))) all-group-members))))
  (or others null))

(define ((make-matchmaker group-size [group-ok? values]) page-proc)
  (if (hash-has-key? (get-ready-groups) current-group)
      (skip)
      (call-with-study-transaction
       (lambda ()
         (cond
           [(not (or (undefined? current-group) (equal? #f current-group)))
            (void)]
           [(get-pending-group group-ok?)
            => (lambda (pending-group)
                 (match-define (cons pending-group-id participant-ids)
                   pending-group)
                 (set! current-group pending-group-id)
                 (cond
                   [((group-size . - . (length participant-ids)) . = . 1)
                    (define participant-ids* (cons (current-participant-id) participant-ids))
                    (set! ready-groups (hash-set (get-ready-groups) pending-group-id participant-ids*))
                    (set! pending-groups (hash-remove pending-groups pending-group-id))]
                   [else
                    (set! pending-groups (hash-update
                                          #;ht pending-groups
                                          #;key pending-group-id
                                          #;updater (lambda (pending-group) ;; noqa
                                                      (cons (current-participant-id) pending-group))
                                          #;failure-result null))]))]
           [else
            (set! current-group (buid))
            (set! pending-groups (hash-update
                                  #;ht (get-pending-groups)
                                  #;key current-group
                                  #;updater (lambda (pending-group)
                                              (cons (current-participant-id) pending-group))
                                  #;failure-result null))])
         (page-proc)))))

;; group-id -> (listof participant-id)
(define (get-pending-groups)
  (if-undefined pending-groups (hash)))

(define (get-pending-group group-ok?)
  (for/first ([(group-id participant-ids) (in-hash (get-pending-groups))]
              #:when (group-ok? group-id))
    (cons group-id participant-ids)))

;;================================================
;; Storing/retrieving results within groups
(require data/monocle)

;; (hash group-id . (hash participant-id . (hash key . val)))
(defvar/instance _group-results)

(define (results) (if-undefined _group-results (hash)))

(define (&group-member-result participant-id key)
  (parameterize ([current-hash-maker hash])
    (&opt-hash-ref*
     (get-current-group)
     participant-id
     key)))

(define (store-my-result-in-group! key val)
  (when (get-current-group)
    (with-study-transaction
        (set! _group-results
              ((&group-member-result (current-participant-id) key) (results) val)))))

(define (get-my-result-in-group key)
  (when (get-current-group)
    ((&group-member-result (current-participant-id) key) (results))))

(define (current-group-member-results key
                                      #:include-ids? [ids #f]
                                      #:include-self? [include-self? #f])
  (for/list ([member (in-list (current-group-members #:include-self? include-self?))])
    (define result ((&group-member-result member key) (results)))
    (if ids (cons member result) result)))

(define (current-group-results-count key #:include-self? [include-self? #f])
  (length (filter values (current-group-member-results key #:include-self? include-self?))))