#lang conscript

(require conscript/form0)

(provide
 conscript-forms-lib)

(defstep (intro)
  @md{# Forms

      This study shows how to use `forms-lib` with conscript.

      @button{Continue}})

(defvar name)
(defvar age)

(defstep (survey)
  (define-values (survey-form on-submit)
    (form+submit
     [name (ensure binding/text (required))]
     [age (ensure binding/number (required))]))

  (define (render rw)
    @md*{@rw["name" (input-text)]
         @rw["age" (input-number)]
         @|submit-button|})

  @md{# Survey

      @form[survey-form on-submit render]
      })

(defstep (display-info)
  @md{# Thanks

      Name: @name
      Age: @~a[age]})

(defstudy conscript-forms-lib
  [intro --> survey --> display-info --> ,(λ () done)])
