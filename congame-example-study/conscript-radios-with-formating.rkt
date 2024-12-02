#lang conscript

(provide
 test-radios)

(defvar strategy)

(define a "A")
(define b "B")

(defstep (pick-strategies)
  @md{# Radios

      @form{
            @(set! strategy
                   (radios
                    `((,a . ,(format "Strategy ~a" a))
                      (,b . ,(format "Strategy ~a" b)))
                    "Pick your strategy"))
             @submit-button}})

(defstep (the-end)
  @md{# Your choice

      You picked @(~a strategy).})

(defstudy test-radios
  [pick-strategies --> the-end --> the-end])