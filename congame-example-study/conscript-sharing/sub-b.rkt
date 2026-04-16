#lang conscript/with-require

(require racket/unit
         "result-sig.rkt"
         "sub-sig.rkt")

(provide
 sub-b@)

(define sub-b@
  (unit
    (import result^)
    (export sub^)

    (defstep (start)
      (store-treatment 'b-treatment)
      @md{# Start

          This is substudy b.

          @button{Continie}})

    (defstep (end)
      (store-result 0)
      @md{# End

          You're done with substudy b.

          @button{Continue}})

    (defstudy sub-study
      [start --> end --> ,(λ () done)])))
