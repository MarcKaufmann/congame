#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         (prefix-in congame: congame/components/formular)
         (prefix-in congame: (submod congame/components/formular tools)))

(provide
 (rename-out
  [congame:~error ~error]
  [congame:~errors ~errors]
  [congame:~all-errors ~all-errors]
  [congame:checkbox checkbox]
  [congame:formular-autofill bot:autofill]
  [congame:input-date input-date]
  [congame:input-file input-file]
  [congame:input-number input-number]
  [congame:input-range input-range]
  [congame:input-text input-text]
  [congame:input-time input-time]
  [congame:submit-button submit-button]
  [congame:submit-button/label submit-button/label]
  [congame:textarea textarea]
  [congame:make-checkboxes make-checkboxes]
  [congame:make-radios make-radios]
  [congame:make-radios-with-other make-radios-with-other])
 form
 radios
 select
 binding)

(define (splice xexpr)
  (if (null? xexpr) "" (car xexpr)))

;; Flips the `options` and `label` arguments of the given procedure to
;; make it so that the `label` is last and can be used inside curly
;; braces in Scribble.
(define (make-flipped-procedure proc)
  (make-keyword-procedure
   (lambda (kws kw-args options label . args)
     (keyword-apply proc kws kw-args label options args))))

(define radios (make-flipped-procedure congame:radios))
(define select (make-flipped-procedure congame:select))

(define-syntax (binding stx)
  (raise-syntax-error 'binding "may only be used inside a form block" stx))

(begin-for-syntax
  (define-literal-set error-bindings
    (congame:~error congame:~errors congame:~all-errors))

  (define error-binding?
    (literal-set->predicate error-bindings))

  (define-literal-set widget-bindings
    (congame:checkbox
     congame:input-date
     congame:input-file
     congame:input-number
     congame:input-range
     congame:input-text
     congame:input-time
     congame:textarea
     radios
     select))

  (define widget-binding?
    (literal-set->predicate widget-bindings))

  (define-syntax-class form-expr
    #:literal-sets (error-bindings widget-bindings)
    #:literals (binding form)
    {pattern (form . _)
             #:do [(raise-syntax-error 'form "cannot nest forms" this-syntax)]
             #:with form-e this-syntax}
    {pattern (binding name:keyword arg)
             #:with form-e #'(name arg)}
    {pattern (id:id name:keyword ...)
             #:when (error-binding? #'id)
             #:with form-e #'(splice (congame:~error name ...))}
    {pattern (id:id name:keyword args ...)
             #:when (widget-binding? #'id)
             #:with form-e #'(name (id args ...))}
    {pattern (rator:form-expr rand:form-expr ...)
             #:with form-e #'(rator.form-e rand.form-e ...)}
    {pattern e
             #:with form-e this-syntax}))

(define-syntax (form stx)
  (syntax-parse stx
    [(_ {~alt
         {~optional {~seq #:action action:expr}}
         {~optional {~seq #:bot bot}}
         {~optional {~seq #:fields fields}}} ...
        body:form-expr ...+)
     #'(congame:formular
        {~? {~@ #:bot bot}}
        {~? {~@ #:fields fields}}
        `(div () ,body.form-e ...)
        {~? {~@ action}})]))
