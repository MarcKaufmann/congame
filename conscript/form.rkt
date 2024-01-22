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
  [congame:input-date input-date]
  [congame:input-file input-file]
  [congame:input-number input-number]
  [congame:input-range input-range]
  [congame:input-text input-text]
  [congame:input-time input-time]
  [congame:radios radios]
  [congame:select select]
  [congame:submit-button submit-button]
  [congame:textarea textarea])
 form)

(define (splice xexpr)
  (if (null? xexpr) "" (car xexpr)))

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
     congame:radios
     congame:select
     congame:textarea))

  (define widget-binding?
    (literal-set->predicate widget-bindings))

  (define-syntax-class form-expr
    #:literal-sets (error-bindings widget-bindings)
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
    [(_ #:action action:expr body:form-expr ...+)
     #'(congame:formular `(div () ,body.form-e ...) action)]
    [(_ body:form-expr ...+)
     #'(congame:formular `(div () ,body.form-e ...))]))
