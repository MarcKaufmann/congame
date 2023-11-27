#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         congame/components/struct
         "html.rkt")

;; kernel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 #%app
 #%datum
 #%module-begin
 #%top
 #%top-interaction)

;; syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (all-from-out "html.rkt")
 defstep)

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
