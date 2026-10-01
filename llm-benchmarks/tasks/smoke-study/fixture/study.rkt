#lang conscript

#|review: ignore|#
;; This intentionally incomplete task fixture also triggers generated-binding
;; warnings in the current Conscript review extension.

(provide
 benchmark-study)

;; Replace this placeholder with the study described in TASK.md.
(defstep (placeholder)
  @html{@h1{Not implemented}
        @button{Continue}})

(defstudy benchmark-study
  [placeholder --> placeholder])
