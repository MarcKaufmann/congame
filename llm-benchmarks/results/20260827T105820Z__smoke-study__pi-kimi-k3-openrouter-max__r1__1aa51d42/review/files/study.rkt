#lang conscript

(require conscript/form0)

(provide
 benchmark-study)

(defvar name)

(defstep (intro)
  @html{@h1{Welcome}
        Welcome to this short study. It only takes a minute.

        @button{Continue}})

(defstep (ask-name)
  (define-values (name-form on-submit)
    (form+submit
     ;; binding/text produces #f for an empty input, so (required)
     ;; rejects empty submissions.
     [name (ensure binding/text (required))]))

  (define (render rw)
    @md*{@rw["name" @input-text{Display name:}]
         @|submit-button|})

  @md{# Your Display Name

      Please enter a display name (it may not be empty).

      @form[name-form on-submit render]})

(defstep (done)
  @md{# Thank You, @|name|!

      You registered the display name: **@|name|**.})

(defstudy benchmark-study
  [intro --> ask-name --> done --> done])
