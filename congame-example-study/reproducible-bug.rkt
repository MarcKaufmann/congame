#lang racket/base

(require congame/components/study
         congame/components/formular
         congame/components/transition-graph
         koyo/haml)

(provide
 reproducible-bug-study)

(define (stake-distributes)
  (page
   (haml
    (.container
     (formular
      (haml
       (:div
        (#:dist (input-number "number"))
        (:button.button.next-button ((:type "submit")) "Next")))
      (lambda (#:dist dist)
        (put 'dist dist)))))))

(define (make-investment)
  (page
   (haml
    (.container
     (formular
      (haml
       (:div
        (#:inv (radios
                "Which alternative do you prefer?"
                '(
                  ("Safe"  . "A: You receive the safe alternative (see above) with certainty (100%).")
                  ("Risky" . "B: You receive 800 with 50% chance and in the other 50% of cases you receive 0.")
       )))
        (:button.button.next-button ((:type "submit")) "Next")))
      (lambda (#:inv inv)
        (put 'inv inv)))))))

(define (update-data-distribution)
  (void))

(define (save-dist-choice)
  (page
   (haml
    (.container
     (button update-data-distribution "Continue")))))

(define (set-outcome)
  (skip))

(define (save-inv-choice)
  (skip))

(define stakeholder-invests
  (make-study
   "stakeholder-invests"
   #:transitions
   (transition-graph
    (make-investment --> set-outcome
                      --> save-inv-choice
                      --> ,(lambda () done)))
   (list
    (make-step 'make-investment make-investment)
    (make-step 'set-outcome set-outcome)
    (make-step 'save-inv-choice save-inv-choice))))

(define stakeholder-distributes
  (make-study
   "stakeholder-distributes"
   #:transitions
   (transition-graph
    (stake-distributes --> save-dist-choice --> ,(lambda () done)))
   (list
    (make-step 'stake-distributes stake-distributes)
    (make-step 'save-dist-choice save-dist-choice))))

(define (end)
  (page
   (haml
    (.container
     (:h1 "The end")))))

(define reproducible-bug-study
  (make-study
   "reproducible-bug"

   #:transitions
   (transition-graph
    [si1 --> si2 --> si3 --> si4 --> sd1]
    [sd1 --> sd2 --> sd3 --> sd4 --> sd5]
    [sd5 --> sd6 --> sd7 --> sd8 ;--> sd9]
    ;[sd9 --> sd10 --> sd11 --> sd12
                 --> end]
    [end --> end])

   (list
    (make-step/study 'si1 stakeholder-invests)
    (make-step/study 'si2 stakeholder-invests)
    (make-step/study 'si3 stakeholder-invests)
    (make-step/study 'si4 stakeholder-invests)
    (make-step/study 'sd1 stakeholder-distributes)
    (make-step/study 'sd2 stakeholder-distributes)
    (make-step/study 'sd3 stakeholder-distributes)
    (make-step/study 'sd4 stakeholder-distributes)
    (make-step/study 'sd5 stakeholder-distributes)
    (make-step/study 'sd6 stakeholder-distributes)
    (make-step/study 'sd7 stakeholder-distributes)
    (make-step/study 'sd8 stakeholder-distributes)
    ;(make-step/study 'sd9 stakeholder-distributes)
    ;(make-step/study 'sd10 stakeholder-distributes)
    ;(make-step/study 'sd11 stakeholder-distributes)
    ;(make-step/study 'sd12 stakeholder-distributes)
    (make-step 'end end))))
