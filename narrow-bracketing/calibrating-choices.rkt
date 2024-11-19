#lang conscript

(require conscript/survey-tools)

(provide
 nb-calibration)

(defvar consent?)
(defvar throw-away)

(define n 20)
(define completion-fee 1.5)

(defstep (welcome)
  @md{# Welcome

      Thank you for participating in this study. In this study, you will be asked several choices about decoding sequences for money. This page explains the overall structure of the study, on the next pages we will cover the following:

      1. Illustration of the decisions and payments based on your decisions
      2. Illustration of the decoding task
      3. Comprehension test required to continue with the study
      4. Make @(~a n) decisions
      5. Decode sequences based on **one randomly chosen decision**
      6. Receive completion code

      You will receive a completion fee of @(~pound completion-fee) from completing the study in addition to the payment for the decoded sequences (explained later).

      There are no expected risks from participating in this study.

      You can withdraw at any time, in which case you forfeit the payments.

      @form{
            @(set! consent?
                   (map-result
                    (radios
                    '(("yes" . "I agree to participate in this study")
                      ("no"  . "I do not agree to participate in this study"))
                    "Please select whether you want to participate in this study or prefer to leave at this stage.")
                    (lambda (r)
                      (string=? r "yes"))))
            @submit-button}})

(define (~decode n p)
  (format "Decode ~a sequences for ~a"
          (if (= n 0) "no" n)
          (~pound p)))

(define (radios/choices opts)
  (radios
   (for/list ([opt opts]
              [i (in-range (length opts))])
     (cons (number->string i) (apply ~decode opt)))
   "Choose one of these options"))

(defstep (illustrate-decisions)
  (define illustration-decision
    '((20 1.00) (0 0.20) (5 0)))
  @md{# Illustrating Decisions and Payments

      You will later make @(~a n) decisions on @(~a n) consecutive pages. In each decision, you are offered 2 or 3 options, such as *@(format "Decode 20 sequences for ~a" (~pound 1))*.

      ## An Example of a Single Decision

      Here is an example decision (you can select, but this choice will not matter):

      @form{
            @(set! throw-away (radios/choices illustration-decision))}

      Selecting the first option would mean that you would have to decode @(~a (first (first illustration-decision))) sequences and you would receive an extra bonus of @(~pound (second (first illustration-decision))).

      ## Determining the Decision That Counts

      You will make @(~a n) decisions, each on a separate page. After you made all your decisions, we will randomly select one of these decisions as the decision that counts: that is, you will have to do the work of the option you chose and receive the bonus for that work.

      The other decisions you made will not be taken into account.

      ## Total Payment

      In order to complete the study, you have to do the work of the decision that counts. If you do not, then you do not receive the extra bonus, nor the completion fee.

      @button{Continue}})

(defstep (illustrate-decoding-task)
  @md{# Illustrate Decoding Task

      @button{Continue}})

(defstep (no-consent)
  @md{# You did not agree to participate

      Since you decided not to participate in the study, you are done. You can close this window.})

(defstudy nb-calibration
  [welcome --> ,(lambda ()
                  (if consent? 'illustrate-decisions 'no-consent))]
  [illustrate-decisions --> illustrate-decoding-task
                        --> illustrate-decoding-task
                        ;--> comprehension-test
                        ;--> decisions
                        ;--> pick-decision-that-counts
                        ;--> do-work-that-counts
                        ;--> final-page
                        ]
  [no-consent --> no-consent])
