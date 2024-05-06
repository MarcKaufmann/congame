#lang conscript

(provide
 conscript-for-study-example)

(defvar* n i-swear-this-is-unique)

(defstep (start)
  (define (set-n #:n the-n)
    (set! n the-n))
  @md{# Start

      @form[#:action set-n]{
        @input-number[#:n #:min 1 #:max 5]{How many steps?}
        @submit-button
      }})

(defstep (say-hi i)
  @md{# Hello

      This is step @number->string[i].

      @button{Continue}})

(define (make-substudy)
  (for/study ([i (in-range n)])
    (say-hi i)))

(defstudy conscript-for-study-example
  [start --> [s (make-step/study 's make-substudy)] --> start])
