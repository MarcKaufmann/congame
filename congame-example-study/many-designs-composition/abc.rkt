#lang conscript/with-require

(require conscript/form0
         "fee-sig.rkt"
         "payment-sig.rkt"
         "study-sig.rkt")

(provide
 abc-study@)

(define abc-study@
  (unit
    (import fee^)
    (export payment^ study^)

    (with-namespace xyz.trichotomy.many-designs.abc
      (defvar* payment))

    (define (compute-payment)
      (- payment (get-fee)))

    (defstep (start)
      @md{# Start ABC

          @button{Continue}})

    (defstep (ask-payment)
      (define-values (payment-form on-submit)
        (form+submit
         [payment (ensure binding/number (required) (range/inclusive (get-fee) 100))]))

      (define (render rw)
        @md*{@rw["payment" @input-number{How much money do you want?}]
             @|submit-button|})

      @md{# Payment

          @form[payment-form on-submit render]})

    (defstudy study
      [start --> ask-payment --> ,(λ () done)])))
