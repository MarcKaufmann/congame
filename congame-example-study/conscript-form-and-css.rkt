#lang conscript

(require conscript/form0)

(provide
 conscript-form-and-css)

(define-static-resource css
  "conscript-css-resource.css")

(defstep (start)
  @md{# Start

      @button{Continue}})

(defvar n)

(defstep (a-form)
  (define-values (f on-submit)
    (form+submit
     [n (ensure binding/number (required))]))
  (define (render rw)
    @md{@rw["n" @input-number{Enter a number:}]
        @|submit-button|})
  @md{# Form

      @form[f on-submit render]})

(defstudy conscript-form-and-css
  #:wrapper @add-css-resource[css]
  [start --> a-form --> a-form])
