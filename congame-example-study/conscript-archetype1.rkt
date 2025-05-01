#lang conscript

(require conscript/form0
         racket/match
         racket/random)

(provide hya54)

;; Code from the many designs study: HYA54
;; Huber et al. 2023

(defstep (welcome)
  @md{# Welcome

      @button{Continue}})

(defstep (final-page)
  @md{# Final Page

      When paid: @when-paid?
      Some other var: @some-other-var
      })

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
  (define choices
    '(("high-number" . "If the number I report is higher than 4.")
      ("matching-number" . "If the number I report matches the one on the screen.")))

  (define-values (comprehension-form on-submit)
    (form+submit
     [when-paid?
      (ensure
       binding/text
       (required)
       (one-of (for/list ([choice (in-list choices)])
                 (match-define (cons value _) choice)
                 (cons value value))))]
     [some-other-var
      (ensure
       binding/text
       (required))]))

  (define (render rw)
    @md*{@rw["when-paid?" (radios choices "")]
         @rw["some-other-var" @input-text{Please provide some text.}]
         @|submit-button|})

  @md{# Comprehension Test

      @form[comprehension-form on-submit render]})

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
