#lang conscript/with-require

(require "abc.rkt"
         "cde.rkt"
         "fee-sig.rkt"
         "fgh.rkt"
         "payment-sig.rkt"
         "study-sig.rkt")

(provide
 many-designs)

(with-namespace xyz.trichotomy.many-designs
  (defvar* fee))

(define-values (abc-study compute-abc-study-payment)
  (let ()
    (define (get-fee) fee)
    (define-values/invoke-unit abc-study@
      (import fee^)
      (export payment^ study^))
    (values study compute-payment)))

(define cde-study
  (let ()
    (define (get-fee) (* fee 2))
    (define-values/invoke-unit cde-study@
      (import fee^)
      (export study^))
    study))

(defstep (the-beginning)
  (set! fee 42)
  @md{# The Beginning

      @button{Continue}})

(defstep (the-end)
  @md{# The End

      ABC payment: @~a[(compute-abc-study-payment)]
      })

(defstudy many-designs
  [the-beginning --> abc-study --> cde-study --> fgh-study --> the-end]
  [the-end --> the-end])
