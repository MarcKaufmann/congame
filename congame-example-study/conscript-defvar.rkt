#lang conscript/with-require

(require "conscript-defvar-consent.rkt")

(provide
 conscript-defvar-example)

(defstep (info)
  @html{@h1{Hello!}
        Welcome to the study.
        @form{
          @label{Name: @set![name (input-text)]}
          @submit-button}})

(defstep (done)
  @html{@h1{Done}
        You're done.})

(defstudy conscript-defvar-example
  [info --> consent --> done]
  [done --> done])
