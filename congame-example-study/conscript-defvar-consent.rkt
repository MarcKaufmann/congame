#lang conscript

;; Shows that global vars can be reused across modules.

(provide
 consent
 name)

(with-namespace xyz.trichotomy.conscript.conscript-defvar-consent
  (defvar* name))

(defstep (get-consent)
  @html{@h1{Consent}
        Welcome to the study, @|name|!
        @button{Continue}})

(defstudy consent
  [get-consent --> ,(λ () done)])
