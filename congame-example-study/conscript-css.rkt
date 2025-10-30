#lang conscript

(provide
 css-study
 css-study-parent)

(defstep (hello)
  @md{# Hi
      @p{Some description}
      @button{Continue}})

(defstep (goodbye)
  @md{# Goodbye

      @button{Continue}})

(defstudy css-study
  #:wrapper @add-css{h1 { color: red }}
  [hello --> goodbye --> hello])

(defstudy css-study-parent
  #:wrapper @add-css{p { color: blue }}
  [css-study --> ,(λ () done)])
