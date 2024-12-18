#lang conscript

(require conscript/admin)

(provide
 conscript-with-admin)

(defstep (intro)
  @md{# Welcome

      Welcome to the study.

      @button{Continue...}})

(defstep (end)
  @md{You've reached the end.})

(defstudy study
  [intro
   --> [end (with-bot end bot:completer)]
   --> ,(λ () done)])

(define (default-bot-model _k proc)
  (proc))

(define conscript-with-admin
  (make-admin-study
   #:models `((default . ,default-bot-model))
   study))
