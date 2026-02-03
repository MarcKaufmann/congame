#lang conscript

(provide
 cheating-lying-randomizer)

(defvar lying-randomizer)
(defvar cheating-randomizer)

; First way of randomizing
(defstep (lying)
  (set! lying-randomizer (random-ref '(gold taylor)))
  (if (equal? lying-randomizer 'gold)
      @md{# Lying Control Version

          @button{Next}}
      @md{# Lying Competition Version

          @button{Next}}))

(defstep (cheating)
  (set! cheating-randomizer (random-ref '(control competition)))
  (skip))

; Second way of randomizing
; Better, since more extensible.
(defstep (cheating-control)
  @md{# Cheating Control

      @button{Next}})

(defstep (cheating-competition)
  @md{# Cheating Competition

      @button{Next}})

(defstep (the-end)
  @md{# The End})

(defstudy cheating-lying-randomizer
  [lying --> cheating
         --> ,(lambda ()
                (case cheating-randomizer
                  [(control) 'cheating-control]
                  [(competition) 'cheating-competition]))]
  [cheating-control --> the-end]
  [cheating-competition --> the-end]
  [the-end --> the-end])