#lang conscript/with-require

(require (prefix-in congame: congame/components/study)
         forms
         koyo/haml
         racket/match)

(provide
 conscript-forms-lib)

(defstep (intro)
  @md{# Forms

      This study shows how to use `forms-lib` with conscript.

      @button{Continue}})

(defvar name)
(defvar age)

(defstep (survey)
  (define survey-form
    (form* ([name (ensure binding/text (required))]
            [age (ensure binding/number (required))])
      (list name age)))

  (define (on-submit data)
    (match-define (list n a) data)
    (set! name n)
    (set! age a))

  (define (render rw)
    (haml
     (:div
      (:div
       (:label
        "Name: "
        (rw "name" (widget-text))
        ,@(rw "name" (widget-errors))))
      (:div
       (:label
        "Age: "
        (rw "age" (widget-number))
        ,@(rw "age" (widget-errors))))
      (:button
       ([:type "submit"])
       "Submit"))))

  @md{# Survey

      @congame:form[survey-form on-submit render]})

(defstep (display-info)
  @md{# Thanks

      Name: @name
      Age: @~a[age]})

(defstudy conscript-forms-lib
  [intro --> survey --> display-info --> ,(λ () done)])
