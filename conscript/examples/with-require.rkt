#lang conscript/with-require

(require racket/base)

(provide
 conscript-example)

(defstep (info)
  (exit 1))

(defstep (consent)
  @html{@h1{Consent}
        Do you consent to join the study?
        @button{Give Consent}})

(defstep (done)
  @html{@h1{Done}
        You're done.})

(defstudy (conscript-example)
  [info --> consent --> done]
  [done --> done])
