#lang conscript

(require conscript/form0)

(provide
 benchmark-study)

(defvar display-name)

(define-values (name-form on-submit)
  (form+submit
    [display-name (ensure binding/text
                          (required)
                          (matches #rx"^.+$" #:message "Please enter a display name."))]))

(defstep (intro)
  @md{# Welcome

      Welcome to this short study. Click Continue to begin.

      @button{Continue}})

(defstep (ask-name)
  (define (render rw)
    @md{# What is your name?

        @rw["display-name" (input-text "Display name:")]

        @|submit-button|})

  @md{@form[name-form on-submit render]})

(defstep (final)
  @md{# Thank You

      Thank you, @display-name, for participating in this study.})

(defstudy benchmark-study
  [intro --> ask-name --> final]
  [final --> final])
