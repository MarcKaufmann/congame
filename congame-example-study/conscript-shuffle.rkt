#lang conscript

(provide
 shuffle-study)

(defvar steps)

(defstep (start)
  (set! steps (shuffle (hash-keys the-steps)))
  @md{# Welcome to the study
      @button{Continue}})

(define (make-shuffled-study)
  (for/study ([id (in-list steps)])
    (define the-step (hash-ref the-steps id))
    (the-step) ;; need to call the step here!
    ))

;; Make-shuffled-study is not a step, or a study, but a study maker that
;; defers study creation until "runtime" (i.e. when the user visits the
;; step).
(defstep/study shuffled-study
  #:study make-shuffled-study)

(defstep (apple-step)
  @md{# Apple
      @button{Continue}})

(defstep (banana-step)
  @md{# Banana
      @button{Continue}})

(defstep (kumquat-step)
  @md{# Kumquat
      @button{Continue}})

(defstep (display-order)
  @md{# Order
      You went through the steps in this order: @string-join[(map ~a steps) ", "]
      @button{Continue}})

(define the-steps
  (hasheq
   'apple apple-step
   'banana banana-step
   'kumquat kumquat-step))

(defstudy shuffle-study
  [start --> shuffled-study --> display-order --> start])
