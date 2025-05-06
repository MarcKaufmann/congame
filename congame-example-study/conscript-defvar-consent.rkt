#lang conscript

;; Shows that global vars can be reused across modules. Every use refers
;; to the same global. See conscript/matchmaking for an example that
;; uses units to have multiple matchmaking states.

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
