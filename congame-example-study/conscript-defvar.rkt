#lang conscript/with-require

(require "conscript-defvar-consent.rkt")

(provide
 conscript-defvar-example)

(defstep (info)
  (define (on-submit #:name n)
    (set! name n))
  @html{@h1{Hello!}
        Welcome to the study.
        @form[#:action on-submit]{
          @label{Name: @input-text[#:name]}
          @submit-button}})

(defstep (done)
  @html{@h1{Done}
        You're done.})

(defstudy conscript-defvar-example
  [info --> consent --> done]
  [done --> done])
