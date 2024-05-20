#lang conscript/local

(defvar*/instance counter the-counter)

(defstep (hello)
  @md{# Welcome to the study

      @button{Continue}})

(defstep (increment)
  (when (undefined? counter)
    (set! counter 0))
  (define (do-increment)
    (with-study-transaction
      (set! counter (+ counter 1))))
  @md{# Press the Button to Increment the Counter

      Counter: @format["~a" counter]

      @button[do-increment]{Continue}})

(defstep (end)
  @md{# You're done})

(defstudy the-study
  [hello --> increment --> end]
  [end --> ,(lambda () done)])
