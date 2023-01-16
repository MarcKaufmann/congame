#lang scribble/manual

@(require (for-label congame/components/study
                     (only-in forms form? widget-renderer/c)
                     racket/base
                     racket/contract
                     web-server/http
                     xml))

@title{Reference}
@defmodule[congame/components/study]

@section{Studies}

A @deftech{study} is a series of @tech{steps} and a transition graph
that controls which steps lead into which.

@defproc[(study? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a study.
}

@defproc[(make-study [name string?]
                     [steps (listof step?)]
                     [#:requires requires (listof symbol?) null]
                     [#:provides provides (listof symbol?) null]
                     [#:transitions transitions (or/c #f (hash/c symbol? any/c)) #f]
                     [#:view-handler view-handler (or/c #f (-> request? response?)) #f]
                     [#:failure-handler failure-handler (or/c #f (-> step? any/c step-id/c)) #f]) study?]{

}


@defproc[(run-study [s study?]
                    [req request? (current-request)]
                    [#:bindings bindings (hash/c symbol? any/c) (hasheq)]) any/c]{

  Runs the study @racket[s] under @racket[req] with @racket[bindings].
}

@section{Steps}

A @deftech{step} is any page that can be used to relay information to
a participant or collect information from them (or both).

@defproc[(step? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @tech{step}.
}

@defproc[(make-step [id symbol?]
                    [handler (-> step-page?)]
                    [transition transition/c (lambda () next)]
                    [#:view-handler view-handler (or/c #f (-> request? response?)) #f]
                    [#:for-bot bot-handler (or/c #f procedure?) #f]) step?]{

}

@defproc[(make-step/study [id symbol?]
                          [s study?]
                          [transition transition/c (lambda () next)]
                          [#:require-bindings require-bindings (listof binding/c) null]
                          [#:provide-bindings provide-bindings (listof binding/c) null]) step?]{

  Creates a @tech{step} that executes @racket[s] when reached.

  The @racket[#:require-bindings] argument maps identifiers required
  by @racket[s] to identifiers available in the current study context
  if the name is different -- otherwise it assumes that required
  identifers share names and attempts to set them
  accordingly.

  The @racket[#:provide-bindings] argument maps identifiers in the
  current study that should be mapped to some subset of the
  identifiers provided by @racket[s] upon completion.  When
  @racket[#:provide-bindings] is @racket[null?], no values are
  assigned.

  For example:

  @racketblock[
  (make-step/study
    'required-tasks
    task-study
    #:require-bindings '([n task-treatment])
    #:provide-bindings '([root-success? success?]))
  ]

  Here, @racket[n] in @racket[task-study] will take on the value of
  @racket[task-treatment], and after running, @racket[root-success?]
  will be assigned the value of @racket[success?] in the parent.
}

@defproc[(step-page? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a step page.
}

@defform[(page maybe-validator expr ...+)
         #:grammar
         [(maybe-validator (code:line)
                           (code:line #:validator validator-expr))]
         #:contracts
         [(validator-expr (-> any/c xexpr?))]]{

  Within a @tech{step}, this form delimits where the setup actions of
  a step end.  Once execution of a step reaches this form, the next
  time the step is executed, the setup actions will be skipped.

  For example:

  @codeblock|{
    (define (step-with-setup)
      (perform-a-once)
      (perform-b-once)
      (page
       (haml
        (:p "Some text..."))))
  }|

  When the user lands on @racket[step-with-setup] the first time,
  @racket[perform-a-once] and @racket[perform-b-once] will be called.
  Subsequently, if the user reloads the page, it will be as if only
  the expressions under the @racket[page] form make up the step.  If
  control never reaches the @racket[page] form, the setup actions will
  get run again.
}

@deftogether[(
  @defthing[#:kind "canary" next any/c]
  @defthing[#:kind "canary" done any/c]
)]{
  Special values that can be used as transition results to cause a
  study to transition to the next step or to the end of the study,
  respectively.
}

@subsection{Step Widgets}

@defproc[(button [action (-> void?)]
                 [label xexpr?]
                 [#:id id string? ""]
                 [#:to-step-id to-step-id (or/c #f symbol?) #f]) xexpr?]{

  Renders a button with the given @racket[label] that executes
  @racket[action] when pressed.  After the @racket[action] is
  executed, moves the participant to the step named by
  @racket[to-step-id] or the next step if @racket[to-step-id] is
  @racket[#f].

  The @racket[#:id] argument is useful for identifying the button
  within @tech{bot handlers}.
}

@defproc[(form [f form?]
               [action (-> void?)]
               [render (-> (widget-renderer/c) xexpr?)]
               [#:id id string? ""]
               [#:enctype enctype string? "multipart/form-data"]) xexpr?]{

  Renders the form represented by @racket[f] using @racket[render] and
  executes @racket[action] on successful submission, then continues to
  the next step in the study.

  The @racket[#:id] argument is the same as for @racket[button].
}

@defproc[(skip [to-step-id symbol? #f]) void?]{
  Skips to the step named by @racket[to-step-id] or the next step in
  the study if @racket[to-step-id] is @racket[#f].
}

@section{Data Storage & Retrieval}

A @deftech{step scope} represents the region of the database where
data for a study is stored and retrieved from.  Step scope is
determined by the combination of the current participant, the study
stack and optional round and group information.  @deftech{Instance
scope} is shared between participants to a @tech{study instance}.

@defproc[(get [k symbol?]
              [default (or/c any/c (-> any/c)) (λ () (error 'get "value not found for key ~.s" k))]
              [#:round round-name string? (current-round-name)]
              [#:group group-name string? (current-group-name)]) any/c]{

  Retrieves the value stored under the symbol @racket[k] for the
  current @tech{step scope}.  If no such value exists,
  @racket[default] is called if it is a procedure, or returned if it
  is a value.
}

@defproc[(get/instance [k symbol?]
                       [default (or/c any/c procedure?)]) any/c]{

  Like @racket[get], but retrieves data from @tech{instance scope}.
}

@defproc[(put [k symbol?]
              [v any/c]
              [#:round round-name string? (current-round-name)]
              [#:group group-name string? (current-group-name)]) void?]{

  Stores @racket[v] under the symbol @racket[k] for the current
  @tech{step scope}.
}

@defproc[(put/instance [k symbol?]
                       [v any/c]) void?]{

  Like @racket[put], but stores data in @tech{instance scope}.
}

@defparam[current-round-name round-name string?]{
  Controls the current round for participants in a study.
}

@defparam[current-group-name group-name string?]{
  Controls the current group for participants in a study.
}
