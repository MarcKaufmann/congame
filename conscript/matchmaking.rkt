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
  [reset-current-group (-> void?)]))

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
