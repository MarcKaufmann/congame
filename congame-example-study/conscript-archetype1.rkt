#lang conscript

(provide
 hya54)

(require racket/random)

;; HYA54

(defstep (welcome)
  @md{# Welcome

      @button{Continue}})

(defstep (final-page)
  @md{# Final Page})

(defvar* treatment unique-treatment)

(defstep (randomize-treatments)
  (set! treatment
    (random-ref '(control treatment)))
  (skip))

(defstep (instructions-control)
  @md{# Instructions

      Here is how the control treatment goes.

      @button{Continue}})

(defstep (instructions-treatment)
  @md{# Instructions

      Here is how the treatment treatment goes.

      @button{Continue}})

; TODO:
; - Attention checks

(defvar when-paid?)
(defvar some-other-var)

(defstep (comprehension-control)
  @md{# Comprehension Test
      @form{
            @(set!
              when-paid?
              (radios
               '(("high-number" . "If the number I report is higher than 4.")
                 ("matching-number" . "If the number I report matches the one on the screen."))
               "What is the answer to the comprehension test?"))

            @(set!
              some-other-var
              (input-text "Please provide some text."))
            @submit-button}})

(defstep (display-comprehension-test)
  @md{# Your answer

      You answered @(~a when-paid?).

      @button{Continue}})

(defstudy hya54
  [welcome --> randomize-treatments
           --> ,(lambda ()
                  (case treatment
                    [(control) 'instructions-control]
                    [(treatment) 'instructions-treatment]))]

  [instructions-control --> comprehension-control
                        --> display-comprehension-test
                        --> final-page]

  [instructions-treatment --> (comprehension-treatment comprehension-control) ; temporary so we don't have to define
                          --> final-page]

  #;[welcome --> randomize-treatments
           --> instructions
           --> comprehension-check
           --> cheating-game
           --> report-number
           --> compensation
           --> final-page]
  [final-page --> final-page])
