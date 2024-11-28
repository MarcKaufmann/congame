#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         buid
         racket/match
         racket/unit
         "congame-sig.rkt"
         "matchmaking-sig.rkt")

(provide
 matchmaking@)

(define-unit matchmaking@
  (import congame^)
  (export matchmaking^)

  ;; These are study-scoped in order for the parent and the child to be
  ;; able to do their own matchmaking. If a parent wants to share the
  ;; current group with a child, it needs to store it in a separate var*
  ;; and share that with the child and vice-versa.
  (defvar/instance pending-group)
  (defvar/instance ready-groups)
  (defvar current-group)

  (define-syntax (if-undefined stx)
    (syntax-parse stx
      [(_ val-expr default-expr)
       #'(let ([tmp val-expr])
           (if (undefined? tmp)
               default-expr
               tmp))]))

  (define (get-ready-groups)
    (if-undefined ready-groups null))

  (define (get-current-group)
    (if-undefined current-group #f))

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
           (page-proc))))))
