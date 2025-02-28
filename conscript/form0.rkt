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

 checkbox
 input-text
 input-number
 radios

 required-unless)

(define-syntax (form+submit stx)
  (syntax-parse stx
    [(_ [id:id formlet:expr] ...+)
     #:with (tmp ...) (generate-temporaries #'(id ...))
     #'(let ([f (form* ([id formlet] ...)
                  (list id ...))])
         (values f (lambda (data)
                     (match-define (list tmp ...) data)
                     (set! id tmp) ...)))]))

(define (((make-input-widget widget)
          [label #f]
          #:attributes [attributes null])
         name value errors)
  (haml
   (.field-group
    (:label
     (or label (string-titlecase name))
     ((widget #:attributes attributes) name value errors)
     ,@((widget-errors) name value errors)))))

(define checkbox (make-input-widget widget-checkbox))
(define input-text (make-input-widget widget-text))
(define input-number (make-input-widget widget-number))

(define ((radios options
                [label #f]
                #:attributes [attributes null])
         name value errors)
  (haml
   (.group
    (:label.radio-group
     label
     ((widget-radio-group options #:attributes attributes) name value errors))
    ,@((widget-errors) name value errors))))

(define ((required-unless pred) v)
  (cond [(pred) (ok v)]
        [((required) v)]))
