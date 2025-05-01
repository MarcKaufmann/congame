#lang conscript

(require conscript/form0)

(provide
 conscript-for-study-example)

(with-namespace xyz.trichotomy.congame-example-study.conscript-for-study
  (defvar* n))

(defstep (start)
  (define-values (f on-submit)
    (form+submit
     [n (ensure
         binding/number
         (required)
         (range/inclusive 1 5))]))

  (define (render rw)
    @md*{@rw["n" @input-number[#:attributes `([min "1"] [max "5"])]{How many steps?}]
         @|submit-button|})

  @md{# Start

      @form[f on-submit render]})

(defstep (say-hi i)
  @md{# Hello

      This is step @number->string[i].

      @button{Continue}})

(define (make-substudy)
  (for/study ([i (in-range n)])
    (say-hi i)))

(defstudy conscript-for-study-example
  [start --> [s (make-step/study 's make-substudy)] --> start])
