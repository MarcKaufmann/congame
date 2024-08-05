#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         buid
         racket/match
         racket/unit
         "matchmaking-sig.rkt")

(provide
 congame^
 matchmaking@)

(define-signature congame^
  ((define-syntaxes (defvar*)
     (lambda (stx)
       (syntax-parse stx
         [(_ id:id unique-id:id)
          #'(begin
              (define-syntax id
                (make-set!-transformer
                 (lambda (stx)
                   (syntax-case stx (set!)
                     [(set! id v) #'(put-var* 'unique-id 'id v)]
                     [id (identifier? #'id) #'(get-var* 'unique-id 'id)])))))])))
   get-var* put-var*
   (define-syntaxes (defvar*/instance)
     (lambda (stx)
       (syntax-parse stx
         [(_ id:id unique-id:id)
          #'(begin
              (define-syntax id
                (make-set!-transformer
                 (lambda (stx)
                   (syntax-case stx (set!)
                     [(set! id v) #'(put-var*/instance 'unique-id 'id v)]
                     [id (identifier? #'id) #'(get-var*/instance 'unique-id 'id)])))))])))
   get-var*/instance put-var*/instance
   undefined? skip call-with-study-transaction))

(define-unit matchmaking@
  (import congame^)
  (export matchmaking^)

  (defvar*/instance pending-group conscript/matchmaking/pending-group)
  (defvar*/instance ready-groups conscript/matchmaking/ready-groups)
  (defvar* current-group conscript/matchmaking/current-group)

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
