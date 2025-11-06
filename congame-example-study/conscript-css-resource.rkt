#lang conscript

(provide
 css-study
 css-study-parent2)

(define-static-resource css
  "conscript-css-resource.css")

(defstep (hello)
  @md{# Hi
      @p{Some description}
      @button{Continue}})

(defstep (goodbye)
  @md{# Goodbye

      @button{Continue}})

(defstudy css-study
  [hello --> goodbye --> hello])

(defstudy css-study-parent2
  #:wrapper @add-css-resource[css]
  [css-study --> ,(λ () done)])
