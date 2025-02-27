#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         (only-in congame/components/study form)
         (prefix-in dyn: forms)
         (except-in forms form)
         koyo/haml
         racket/match)

(provide
 form
 form+submit
 dyn:form
 (all-from-out forms)

 input-text
 input-number)

(define-syntax (form+submit stx)
  (syntax-parse stx
    [(_ [id:id formlet:expr] ...+)
     #:with (tmp ...) (generate-temporaries #'(id ...))
     #'(let ([f (form* ([id formlet] ...)
                  (list id ...))])
         (values f (lambda (data)
                     (match-define (list tmp ...) data)
                     (set! id tmp) ...)))]))

(define (((make-input-widget widget) [label #f]) name value errors)
  (haml
   (.field-group
    (:label
     (or label (string-titlecase name))
     ((widget) name value errors)
     ,@((widget-errors) name value errors)))))

(define input-text (make-input-widget widget-text))
(define input-number (make-input-widget widget-number))
