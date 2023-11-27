#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/parse)
         congame/components/study
         congame/components/transition-graph
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
 void
 list list? null null? cons pair? car cdr

 ;; Congame Syntax
 -->

 ;; Congame Runtime
 button done
 )

;; syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
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
  (define-splicing-syntax-class transition-arrow
    #:literals (--> unquote)
    {pattern (unquote e)
             #:with (step-id ...) #'()}
    {pattern step:id
             #:with (step-id ...) #'(step)}
    {pattern {~seq step:id --> e:transition-arrow}
             #:with (step-id ...) #'(step e.step-id ...)})

  (define-syntax-class transition
    {pattern [arrow:transition-arrow]
             #:with (step-id ...) #'(arrow.step-id ...)}))

(define-syntax (defstudy stx)
  (syntax-parse stx
    [(_ (id:id) transition-e:transition ...+)
     #:with (step-id ...)
     (for/fold ([stxs null]
                [seen (hash)]
                #:result (reverse stxs))
               ([step-id-stx (in-list (syntax-e #'(transition-e.step-id ... ...)))])
       (define step-id
         (syntax->datum step-id-stx))
       (if (hash-has-key? seen step-id)
           (values stxs seen)
           (values (cons step-id-stx stxs)
                   (hash-set seen step-id #t))))
     #'(define id
         (make-study
          (symbol->string 'id)
          #:transitions
          (transition-graph
           transition-e ...)
          (list (make-step* 'step-id step-id) ...)))]))

(define (make-step* id v)
  (if (study? v)
      (make-step/study id v)
      (make-step id v)))
