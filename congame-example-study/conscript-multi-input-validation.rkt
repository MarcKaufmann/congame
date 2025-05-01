#lang conscript

;; TODO: Port to form0.

(provide
 sum-percentages)

(defvar beliefs)

(define (input-percent label)
  (input-number label #:min 0 #:max 100))

(defstep (distribute-100)
  @md{# Distribute 100

      One of A, B, or C must happen, so the probabilities must sum to 100.

      @form{

            @div{
                 @set![beliefs
                       (map-validator
                        (input-list
                         (list
                          (input-percent "How likely is A?")
                          (input-percent "How likely is B?")
                          (input-percent "How likely is C?")))
                        (lambda (loa)
                          (define total (apply + loa))
                          (if (= total 100)
                              `(ok loa)
                              `(err ,@(for/list ([_ (in-list loa)])
                                        "Your answers must sum to 100.")))))]}
            @submit-button}})


(defstep (display-answers)
  (define belief-a (first beliefs))
  (define belief-b (second beliefs))
  (define belief-c (third beliefs))
  (define belief-total (+ belief-a belief-b belief-c))

  @md{# Answers

      In total we get @(~a belief-a) + @(~a belief-b) + @(~a belief-c) = @(~a belief-total).})

(defstudy sum-percentages
  [distribute-100 --> display-answers --> display-answers])
