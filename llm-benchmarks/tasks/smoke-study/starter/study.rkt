#lang conscript

#|review: ignore|#
;; The Conscript review extension currently reports generated bindings in its
;; own implementation when it analyzes a Conscript module.

(provide
 benchmark-study)

(defstep (placeholder)
  @html{@h1{Benchmark study}
        @p{The task workspace has not been uploaded yet.}
        @button{Continue}})

(defstudy benchmark-study
  [placeholder --> placeholder])
