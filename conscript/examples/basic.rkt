#lang conscript

(provide
 conscript-example)

(defstep (info)
  @html{@h1{Hello!}
        Welcome to the study.
        @button{Continue}})

(defstep (consent)
  @html{@h1{Consent}
        Do you consent to join the study?
        @button{Give Consent}})

(defstep (done)
  @html{@h1{Done}
        You're done.})

(defstudy conscript-example
  [info --> {consent1 consent}
        --> {consent2 consent}
        --> done]
  [done --> done])
