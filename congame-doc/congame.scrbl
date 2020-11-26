#lang scribble/manual

@(require (for-label racket/contract
                     congame/components/study))

@title{Congame}
@author[(author+email "Marc Kaufmann" "marc@trichotomy.xyz")]

@defmodule[congame/components/study]

@include-section["introduction.scrbl"]

@section{Examples}

@defproc[(make-step/study [id symbol?]
                          [s study?]
                          [transition transition/c (lambda () next)]
                          [#:require-bindings require-bindings (listof binding/c) null]
                          [#:provide-bindings provide-bindings (listof binding/c) null]) step?]{
  Runs the study @racket[s] as a sub-study of the step @racket[id]. @racket[#:require-bindings] maps identifiers required by @racket[s] to identifiers available in the current study context if the name is different -- otherwise it assumes that required identifers share names and attempts to set them accordingly. @racket[#:provide-bindings] maps identifiers in the current study that should be mapped to some subset of the identifiers provided by @racket[s] upon completion. If @racket[#:provide-bindings] is @racket[null?], then no values are assigned.
}

For example:

@racketblock[
(make-step/study
  'required-tasks
  task-study
  #:require-bindings '([n task-treatment])
  #:provide-bindings '([root-success? success?]))
]

In this case, @racket[n] in @racket[task-study] will take on the value of @racket[task-treatment], and after running, @racket[root-success?] will be assigned the value of @racket[success?].
