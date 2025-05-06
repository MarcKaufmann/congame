#lang racket/base

;;; Notes
;;;
;;; This example relies on
;;; - formular
;;; - get and put (which can almost surely be removed)
;;; - transition-graph
;;; - skip
;;;
;;; We should probably boil it down to something that requires as few of these
;;; features as possible, to rule out / in that it is an interaction with one of
;;; those features that causes it. My guess is that all it needs is that we
;;; define some variable(s) somewhere (in the first set of substudies?), that we
;;; have skip, and then that we run another substudy. What is weird is that this
;;; problem does not occur when I only repeat studies of the final type. Somehow
;;; it requires the earlier studies too, which I don't get.
;;;
;;; Worth trying out whether the first substudy by itself causes all the problems, as it does not have an empty step that displays the page before the `skip`.

(require congame/components/study
         congame/components/formular
         congame/components/transition-graph
         koyo/haml)

(provide
 reproducible-bug-study)

(define (stake-distributes)
  (haml
   (.container
    (formular
     (haml
      (:div
       (#:dist (input-number "number"))
       (:button.button.next-button ((:type "submit")) "Next")))))))

(define (make-investment)
  (haml
   (.container
    (formular
     (haml
      (:div
       (#:inv (input-number "number"))
       (:button.button.next-button ((:type "submit")) "Next")))))))

(define (update-data-distribution)
  (void))

(define (save-dist-choice)
  (haml
   (.container
    (button update-data-distribution "Continue"))))

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
  (haml
   (.container
    (:h1 "The end"))))

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
