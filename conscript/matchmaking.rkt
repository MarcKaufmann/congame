#lang racket/base

(require buid
         congame/components/study
         racket/contract/base
         racket/match)

(provide
 (contract-out
  [make-matchmaker
   (->* [exact-positive-integer?]
        [(-> buid/c boolean?)]
        procedure?)]
  [get-ready-groups (-> (listof buid/c))]
  [get-current-group (-> (or/c #f buid/c))]
  [reset-current-group (-> void?)]))

;; These are study-scoped in order for the parent and the child to be
;; able to do their own matchmaking. If a parent wants to share the
;; current group with a child, it needs to store it in a separate var*
;; and share that with the child and vice-versa.
(defvar/instance pending-groups)
(defvar/instance ready-groups)
(defvar current-group)

(define (get-ready-groups)
  (if-undefined ready-groups null))

(define (get-current-group)
  (if-undefined current-group #f))

(define (reset-current-group)
  (set! current-group #f))

(define ((make-matchmaker group-size [group-ok? values]) page-proc)
  (if (member current-group (if-undefined ready-groups null))
      (skip)
      (call-with-study-transaction
       (lambda ()
         (cond
           [(not (or (undefined? current-group) (equal? #f current-group)))
            (void)]
           [(get-pending-group group-ok?)
            => (lambda (pending-group)
                 (match-define (cons pending-group-id remaining)
                   pending-group)
                 (set! current-group pending-group-id)
                 (cond
                   [(= remaining 1)
                    (set! ready-groups (cons pending-group-id (if-undefined ready-groups null)))
                    (set! pending-groups (hash-remove pending-groups pending-group-id))]
                   [else
                    (set! pending-groups (hash-update pending-groups pending-group-id sub1))]))]
           [else
            (set! current-group (buid))
            (set! pending-groups (hash-set (get-pending-groups) current-group (sub1 group-size)))])
         (page-proc)))))

(define (get-pending-groups)
  (if-undefined pending-groups (hash)))

(define (get-pending-group group-ok?)
  (for/first ([(group-id remaining) (in-hash (get-pending-groups))]
              #:when (group-ok? group-id))
    (cons group-id remaining)))
