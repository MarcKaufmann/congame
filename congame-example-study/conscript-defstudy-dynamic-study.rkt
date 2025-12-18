#lang conscript

(provide
 test-study)

(define ((make-start i))
  @md{# Start

      Study @~a[i]

      @button{Go}})

(defstep start
  (make-start -1))

(define (make-sub)
  (for/study ([i (in-range 3)])
    ((make-start i))))

(defstep/study my-sub
  #:study make-sub)

(defstudy test-study
  [start --> my-sub --> start])
