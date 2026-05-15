#lang conscript

(provide
 fgh-study)

(defstep (start)
  @md{# Start FGH

      @button{Continue}})

(defstudy fgh-study
  [start --> ,(λ () done)])
