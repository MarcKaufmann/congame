#lang conscript/with-require

(require "fee-sig.rkt"
         "study-sig.rkt")

(provide
 cde-study@)

(define cde-study@
  (unit
    (import fee^)
    (export study^)

    (defstep (start)
      @md{# Start CDE

          Fee: @~a[(get-fee)]

          @button{Continue}})

    (defstudy cde
      [start --> ,(λ () done)])

    (defstudy study
      [cde --> ,(λ () done)])))
