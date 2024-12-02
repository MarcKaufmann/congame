#lang racket/base

(require buid
         congame/components/study
         racket/contract/base
         racket/match)

(provide
 (contract-out
  [make-matchmaker (-> exact-positive-integer? procedure?)]
  [get-ready-groups (-> (listof buid/c))]
  [get-current-group (-> (or/c #f buid/c))]
  [set!-current-group (-> buid/c void?)]
  [reset-current-group (-> void?)]))

;; These are study-scoped in order for the parent and the child to be
;; able to do their own matchmaking. If a parent wants to share the
;; current group with a child, it needs to store it in a separate var*
;; and share that with the child and vice-versa.
(defvar/instance pending-group)
(defvar/instance ready-groups)
(defvar current-group)

(define (get-ready-groups)
  (if-undefined ready-groups null))

(define (get-current-group)
  (if-undefined current-group #f))

(define (set!-current-group g)
  (set! current-group g))

(define (reset-current-group)
  (set! current-group #f))

(define ((make-matchmaker group-size) page-proc)
  (if (member current-group (if-undefined ready-groups null))
      (skip)
      (call-with-study-transaction
       (lambda ()
         (cond
           [(not (undefined? current-group))
            (void)]
           [(if-undefined pending-group #f)
            (match-define (cons pending-group-id remaining)
              pending-group)
            (set! current-group pending-group-id)
            (cond
              [(= remaining 1)
               (set! ready-groups (cons pending-group-id (if-undefined ready-groups null)))
               (set! pending-group #f)]
              [else
               (set! pending-group (cons pending-group-id (sub1 remaining)))])]
           [else
            (set! current-group (buid))
            (set! pending-group (cons current-group (sub1 group-size)))])
         (page-proc)))))
