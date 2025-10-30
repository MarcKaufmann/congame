#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         (prefix-in congame: congame/components/formular)
         (prefix-in congame: (submod congame/components/formular tools))
         (only-in congame/components/study form when-bot)
         (prefix-in dyn: forms)
         (except-in forms form)
         koyo/haml
         racket/format
         racket/match
         racket/port
         web-server/http)

(provide
 (rename-out
  [form+combine form]
  [congame:formular-autofill bot:autofill]
  [congame:submit-button submit-button])
 form+submit
 dyn:form
 (all-from-out forms) ;; all except form (exported as dyn:form avove)
 make-autofill

 checkbox
 checkboxes
 input-date
 input-datetime
 input-email
 input-file
 input-number
 input-range
 input-text
 input-time
 radios
 select
 textarea

 required-unless
 at-least
 number-in-range
 list-longer-than

 make-autofill-meta)

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

(define (make-autofill bots)
  (haml
   (:meta
    ([:name "formular-autofill"]
     [:content (when-bot
                (call-with-output-string
                 (lambda (out)
                   (write bots out))))]))))

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

(define (make-typed-input-widget type)
  (make-input-widget
   (lambda (#:attributes [attributes null])
     (widget-input
      #:type type
      #:attributes attributes))))

(define checkbox (make-input-widget widget-checkbox))
(define input-date (make-typed-input-widget "date"))
(define input-datetime (make-typed-input-widget "datetime"))
(define input-email (make-input-widget widget-email))
(define input-text (make-input-widget widget-text))
(define input-time (make-typed-input-widget "time"))
(define input-number (make-input-widget widget-number))
(define input-range (make-typed-input-widget "range"))
(define textarea (make-input-widget widget-textarea))

(define ((input-file label) name value errors)
  (haml
   (.field-group
    (:label (or label (string-titlecase name))))
   ((widget-file) name value errors)
   ,@((widget-errors) name value errors)))

(define ((select options [label #f]) name value errors)
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
              (match-define (cons (app ~a value) label) opt)
              (haml
               (:label
                (re
                 (lambda (name bindings _errors) ;; noqa
                   (define checked?
                     (and bindings
                          (for/first ([bind (in-list bindings)]
                                      #:do [(define v (bytes->string/utf-8 (binding:form-value bind)))]
                                      #:when (equal? v value))
                            #t)))
                   `(input
                     ([type "checkbox"]
                      [name ,name]
                      [value ,value]
                      ,@(if checked? '([checked ""]) null))))
                 #;idx #f)
                label)))))))
     name value errors))))

(define ((required-unless pred) v)
  (cond [(pred) (ok v)]
        [((required) v)]
        [else (error 'required-unless "unreachable")]))

(define ((at-least n) v)
  (cond
    [(not v) (ok #f)]
    [(>= v n) (ok v)]
    [else (err (format "Must be greater than or equal to ~s." n))]))

(define ((number-in-range lo hi) v)
  (cond
    [(not v) (ok #f)]
    [(and (>= v lo)
          (<= v hi))
     (ok v)]
    [else (err (format "Must be in the range [~a, ~a]." lo hi))]))

(define ((list-longer-than n) v)
  (if (and v (pair? v) (>= (length v) n))
      (ok v)
      (err (format "Expected at least ~a items." n))))

(define (make-autofill-meta ht)
  `(meta
    ([name "formular-autofill"]
     [content ,(call-with-output-string
                (lambda (out)
                  (write ht out)))])))
