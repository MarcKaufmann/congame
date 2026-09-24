#lang conscript

(provide
 conscript-runtime-step-id-study)

(defstep (hello)
  @md{# Hello

      @button{Continue}})

(defstep (goodbye)
  @md{# Goodbye

      @button{Continue}})

(define (get-hello-id)
  'some-step)

(defstudy conscript-runtime-step-id-study
  [{,(get-hello-id) hello} --> goodbye]
  [goodbye --> ,(λ () (get-hello-id))])
