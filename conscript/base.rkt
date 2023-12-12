#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         (prefix-in congame: congame/components/study)
         (except-in congame/components/study button form)
         congame/components/transition-graph
         "form.rkt"
         "html.rkt")

;; kernel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 ;; Racket Syntax
 #%app
 #%datum
 #%module-begin
 #%top
 #%top-interaction
 define
 quote
 quasiquote
 unquote
 provide

 ;; Racket Runtime
 lambda λ
 void
 list list? null null? cons pair? car cdr
 display displayln print println printf eprintf write writeln

 ;; Congame Syntax
 --> goto

 ;; Congame Runtime
 done
 )

;; syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (all-from-out "form.rkt")
 (all-from-out "html.rkt")
 defstep
 defstudy)

(begin-for-syntax
  (define-splicing-syntax-class function-arg
    {pattern arg:id}
    {pattern [arg:id default-expr:expr]}
    {pattern {~seq kwd:keyword arg:id}}
    {pattern {~seq kwd:keyword [arg:id default-expr:expr]}})
  (define-syntax-class function-header
    {pattern (nested:function-header arg:function-arg ...)}
    {pattern (name:id arg:function-arg ...)}
    {pattern (name:id . args-id:id)}))

(define-syntax (defstep stx)
  (syntax-parse stx
    [(_ head:function-header . body)
     #'(define head . body)]))

(begin-for-syntax
  ;; Extends the regular transition graph syntax with support for
  ;; binders in the middle of a transition. Rewrites binders to drop the
  ;; rhs before passing the stx to `transition-graph' (via `tg-e').
  (define-splicing-syntax-class transition-arrow
    #:literals (--> unquote)
    {pattern (unquote e)
             #:with (tg-e ...) #'((unquote e))
             #:with ((step-id step-e) ...) #'()}
    {pattern step:id
             #:with (tg-e ...) #'(step)
             #:with ((step-id step-e) ...) #'((step step))}
    {pattern [step:id step-expr:id]
             #:with (tg-e ...) #'(step)
             #:with ((step-id step-e) ...) #'((step step-expr))}
    {pattern {~seq lhs:transition-arrow --> rhs:transition-arrow}
             #:with (tg-e ...) #'{lhs.tg-e ... --> rhs.tg-e ...}
             #:with ((step-id step-e) ...) #'((lhs.step-id lhs.step-e) ... (rhs.step-id rhs.step-e) ...)})

  (define-syntax-class transition
    {pattern [arrow:transition-arrow]
             #:with tg-e #'(arrow.tg-e ...)
             #:with ((step-id step-e) ...) #'((arrow.step-id arrow.step-e) ...)}))

(define-syntax (defstudy stx)
  (syntax-parse stx
    [(_ (id:id) transition-e:transition ...+)
     #:with ((step-id step-expr) ...)
     (for/fold ([stxs null]
                [seen (hash)]
                #:result (reverse stxs))
               ([step-id-stx (in-list (syntax-e #'(transition-e.step-id ... ...)))]
                [step-expr-stx (in-list (syntax-e #'(transition-e.step-e ... ...)))])
       (define step-id
         (syntax->datum step-id-stx))
       (if (hash-has-key? seen step-id)
           (values stxs seen)
           (values (cons (list step-id-stx step-expr-stx) stxs)
                   (hash-set seen step-id #t))))
     #'(define id
         (make-study
          (symbol->string 'id)
          #:transitions
          (transition-graph
           transition-e.tg-e ...)
          (list (make-step* 'step-id step-expr) ...)))]))

(define (make-step* id v)
  (if (study? v)
      (make-step/study id v)
      (make-step id v)))


;; widgets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 button)

(define button
  (case-lambda
    [(label) (congame:button void label)]
    [args (apply congame:button args)]))
