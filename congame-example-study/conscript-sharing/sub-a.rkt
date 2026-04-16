#lang conscript/with-require

(require racket/unit
         "result-sig.rkt"
         "sub-sig.rkt")

(provide
 sub-a@)

(define sub-a@
  (unit
    (import result^)
    (export sub^)

    (defstep (start)
      (store-treatment 'a-treatment)
      @md{# Start

          This is substudy a.

          @button{Continie}})

    (defstep (end)
      (store-result 1)
      @md{# End

          You're done with substudy a.

          @button{Continue}})

    (defstudy sub-study
      [start --> end --> ,(λ () done)])))
