#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         (only-in congame/components/study form)
         (prefix-in dyn: forms)
         (except-in forms form)
         koyo/haml
         racket/format
         racket/match)

(provide
 (rename-out [form+combine form])
 form+submit
 dyn:form
 (all-from-out forms)

 checkbox
 input-text
 input-number
 select
 radios
 checkboxes

 required-unless)

(define form+combine
  (make-keyword-procedure
   (lambda (kws kw-args f action render . args)
     (keyword-apply
      form kws kw-args f action render args
      #:combine (lambda (_k v1 v2)
                  (if (pair? v1)
                      (append v1 (list v2))
                      (list v1 v2)))))))

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

(define ((select options label) name value errors)
  (haml
   (.field-group
    (:label
     (or label (string-titlecase name)))
    ((widget-select options) name value errors)
    ,@((widget-errors) name value errors))))

(define ((radios options [label #f] #:attributes [attributes null]) name value errors)
  (haml
   (.group
    (:label.radio-group
     (or label (string-titlecase name))
     ((widget-radio-group options #:attributes attributes) name value errors))
    ,@((widget-errors) name value errors))))

(define ((checkboxes options) name value errors)
  (haml
   (.group
    ((widget-list
      (lambda (re)
        (haml
         (.div
          ,@(for/list ([opt (in-list options)])
              (match-define (cons value label) opt)
              (haml
               (:label
                (re (widget-checkbox #:attributes `((value ,(~a value)))))
                label)))))))
     name value errors))))

(define ((required-unless pred) v)
  (cond [(pred) (ok v)]
        [((required) v)]
        [else (error 'required-unless "unreachable")]))
