#lang scribble/manual

@(require (for-label racket/contract
                     congame/components/study))

@title{Congame}
@author[(author+email "Marc Kaufmann" "marc@trichotomy.xyz")]

@include-section["introduction.scrbl"]

@defmodule[congame/components/study]

@defproc[(run-study [s study?]
                    [req request? (current-request)]
                    [#:bindings bindings (hash/c symbol? any/c) (hasheq)]) any/c]{

  Runs the study @racket[s] under @racket[req] with @racket[bindings].
}

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

@section{How Tos}

@subsection{How to add a new study}

The simplest case is when you add a new study from a package that is not yet installed. In that case, define the new study and provide it: say the file that provides it is @racket[package/new-studies.rkt] and the name of the new study is @racket[test-study]. Then in the @racket[info.rkt] file for the package, include the following:

@racketblock[
(define congame-studies
  '((the-package/new-studies test-study)))
]

You then should install this new package, and upon the next launch, congame should pick up this new study.

If this does not work, you may have to remove the @racket[compiled] folder in @racket[congame-web/] so that the cache (which contains the installed studies) gets refreshed.

To add a new study from an already installed package, you should only have to update the @racket[info.rkt] file of the package, as well as refresh the cache.

@subsection{How to pass values from a substudy to its caller}

When running a study with @racket[make-step/study], then the values @racket[put] can not be retrieved anywhere with @racket[get] except in this substudy. To pass some variables up to the calling study, we can use the @racket[#:provides] mechanism. For example, consider the following block of code from @racket[congame-example-study/multi-review]:

@racketblock[
(define (submit-research-ideas [n 2])
  ; ...
  (make-study
   "research-ideas-study"
   #:requires '()
   #:provides '(research-ideas)
   (list
    (make-step 'initialize initialize next-or-done/transition)
    (make-step 'submit-research-idea submit-research-idea next-or-done/transition))))

(define (review-study)
  (make-study
   "review-study"
   #:requires '()
   #:provides '()
   (list
    (make-step/study
     'submit
     (submit-research-ideas)
     #:provide-bindings '([submission research-ideas]))
    (make-step 'update-submissions update-submissions)
    (make-step 'lobby lobby)
    (make-step 'review-1 review)
    (make-step 'review-2 review)
    (make-step 'final final))))
]

The study @racket[submit-research-ideas] @racket[put]s a value with the key @racket['research-ideas], which it provides via @racket[#:provides '(research-ideas)]. This means that this value can be used by the parent study, if so desired. The parent study turns the study into a substudy via @racket[(make-step/study 'submit submit-research-ideas) #:provide-bindings '([submission research-ideas])]. The magic happens in @racket[#:provide-bindings '([submissions research-ideas])], which means that the value of key @racket['research-ideas] for the substudy should be made available (via @racket[put]) to the parent study under the key @racket['submission]. If the parent study did not want to use the provided value, then it can do so by providing an empty list of @racket[#:provide-bindings].

@section{Studies}

@include-section["multi-review.scrbl"]

@include-section["veneer.scrbl"]
