#lang conscript

(provide
 consent
 name)

(defvar name 0CxAvPwBJ3tgSUaZhAZeuH)

(defstep (get-consent)
  @html{@h1{Consent}
        Welcome to the study, @|name|!
        @button{Continue}})

(defstudy consent
  [get-consent --> ,(λ () done)])
