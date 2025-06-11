#lang scribble/manual

@(require (for-label (except-in forms form)
                     marionette
                     racket/base
                     racket/contract
                     congame-web/components/study-bot
                     congame/components/for-study
                     congame/components/bot
                     (submod congame/components/bot actions)
                     congame/components/bot-maker
                     congame/components/formular
                     congame/components/study
                     congame/components/transition-graph
                     koyo/haml
                     (only-in xml xexpr?)))

@(require "doc-util.rkt")

@title[#:style 'toc]{Congame Reference}

@local-table-of-contents[]


@;===============================================

@section[#:style 'quiet]{Studies and Steps}

@defmodule[congame/components/study]

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

  Creates a @tech{study} that can be run by @racket[run-study]. The
  @racket[name] argument is used in debugging and in the admin interface. Any
  step that is part of the study has to be listed in @racket[steps].

  The @racket[#:transitions] argument has to be a @racket[transition-graph] that
  provides all the transitions between the steps. See @racket[transition-graph]
  for details.
}


@defproc[(run-study [s study?]
                    [req request? (current-request)]
                    [#:bindings bindings (hash/c symbol? any/c) (hasheq)]) any/c]{

  Runs the study @racket[s] under @racket[req] with @racket[bindings].
}

@;------------------------------------------------

@subsection{Study variables}

@deftogether[(

@defform[(defvar id)]

@defform[(defvar* id global-id)])]{

Defines a study variable with @tech{participant scope} bound to @racket[_id]. The study variable can
be accessed inside the study steps using @racket[_id] and updated with @racket[(set! _id _expr)]. 

The value of the study variable will be stored in the Congame server database under the current
study → instance → participant. 

Study variables created with @racket[defvar*] will additionally be visible to any child studies (see
@racket[defstep/study]).

@bold{Important:} when using @racket[defvar*], you must provide a second identifier
@racket[_global-id] and manually ensure it is distinct from any identifiers that may be used in
child studies. This prevents child studies that may be using the same identifier names from
accidentally overwriting your parent study’s variable.

}

@deftogether[(

@defform[(defvar/instance ...)]

@defform[(defvar*/instance ...)])]{

Like @racket[defvar] and @racket[defvar*], but for creating study variables with @tech{instance
scope} --- that is, the stored value is shared by all participants in the study instance.

}


@;------------------------------------------------

@subsection{Steps}

@defproc[(step? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @tech{step}.
}

@defproc[(make-step [id symbol?]
                    [handler (-> xexpr?)]
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

@deftogether[(
  @defthing[#:kind "canary" next any/c]
  @defthing[#:kind "canary" done any/c]
)]{
  Special values that can be used as transition results to cause a
  study to transition to the next step or to the end of the study,
  respectively.
}

@;------------------------------------------------

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
               [#:enctype enctype string? "multipart/form-data"]
               [#:combine combine-proc (-> any/c any/c any/c any/c) (λ (k v1 v2) v2)]
               [#:defaults defaults hash? (hash)]) xexpr?]{

  Renders the form represented by @racket[f] using @racket[render] and
  executes @racket[action] on successful submission, then continues to
  the next step in the study.

  The @racket[#:id] argument is useful for identifying the button
  within @tech{bot handlers}.
}

@defproc[(skip [to-step-id symbol? #f]) void?]{
  Skips to the step named by @racket[to-step-id] or the next step in
  the study if @racket[to-step-id] is @racket[#f].
}

@defform[(when-bot expr)]{

When the value of @racket[current-user-bot?] is not @racket[#f], returns the result of @racket[expr]
converted to a string (in @racket[display] mode), otherwise returns @racket[""].

}

@;------------------------------------------------

@subsection{Study loops}

@defmodule[congame/components/for-study]

@defform[(for/study [#:substudies]
                    [#:requires requires]
                    [#:provides provides]
                    (for-clause ...)
                    body-or-break ... body)
         #:contracts
         ([requires (listof symbol?)]
          [provides (listof symbol?)])

]{

Iterates like @racket[for] but each result of the last @racket[_body] accumulated into a list of
@tech{steps}, which are passed to @racket[make-study] to produce a @tech{study}.

Use @racket[for/study] for quickly building studies with many steps that differ in only a few
places.

@racketblock[
(for/study ([phase (in-list '("Setup" "Activation" "Tear-down"))])
  (page
    (haml
      (:h1 phase " Phase")
      (:p "...")
      (button void "Next"))))
]

}

@;================================================

@section[#:style 'quiet]{Formular: forms and fields}

@defmodule[congame/components/formular]

A @deftech{field} is a special value that, when used inside a study form, renders as an input element.
When the user submits the form, the field yields whatever value the user has entered as a Racket value.

@defproc[(formular-field? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[_v] is a @tech{field}, @racket[#f] otherwise.
  
}

@defproc[(formular-autofill [bot-id any/c]) void?]{

@tktk{Autofills elements on the page for the bot with @racket[_bot-id].}

}

@defproc[(input-list [fields (listof formular-field?)]) formular-field?]{

Returns a field that is a collection of @racket[_fields], and which will provide all values
in @racket[_fields] as a single @racket[list] when the form is submitted.

For example, the following code creates two @racket[input-number] fields:

@racketblock[

(set! myvals (input-list (list (input-number) (input-number))))

]

When the user enters two numeric values (say, @racket[-3] and @racket[9]) and submits the form, 
@racketidfont{myvals} will contain @racket['(-3 9)].

}

@deftogether[(
@defproc[(input-number [label (or/c #f string?) #f]
                       [#:min min real? -inf.0]
                       [#:max max real? +inf.0]
                       [#:step step integer? 1]
                       [#:required? req (or/c string? boolean?) #t]
                       [#:validators validators '()]
                       [#:attributes attribs (listof (list/c symbol? string?)) '()])
          formular-field?]
@defproc[(input-range [label (or/c #f string?) #f]
                      [#:min min real? -inf.0]
                      [#:max max real? +inf.0]
                      [#:step step integer? 1]
                      [#:required? req (or/c string? boolean?) #t]
                      [#:validators validators '()]
                      [#:attributes attribs (listof (list/c symbol? string?)) '()])
          formular-field?])]{

Returns a field that ensures its input is numeric and falls between @racket[_min] and @racket[_max]
(inclusive). Use @racket[input-number] field to get a textbox with spinner arrow buttons
for incrementing/decrementing the value, or @racket[input-range] to get a horizontal slider.

If @racket[_step] is provided, then using the stepper arrows or slider will increase or decrease the
value by that amount (though the value is not guaranteed to be a multiple of @racket[_step]).

If @racket[required?] is not @racket[#f], the field must contain data before the form can be
submitted. 

}


@defproc[(checkbox [label (or/c #f string?)]
                   [#:required? required? any/c #t]
                   [#:attributes attrs (listof (list/c symbol? string?)) null])
         formular-field?]{

Returns a @tech{field} that renders as a single checkbox. If @racket[required?] is not @racket[#f],
the checkbox must be selected before the form can be submitted. Any @racket[_attrs] will be used as
@tech{HTML} attributes in the checkbox’s @racketresultfont{<input>} tag.

}

@defproc[(input-text [label (or/c #f string?) #f]
                     [#:required? req (or/c string? boolean?) #t]
                     [#:validators validators '()]
                     [#:attributes attrs (listof (list/c symbol? string?)) '()])
          formular-field?]{

Returns a field that renders as a single-line text input. If @racket[required?] is not @racket[#f],
the checkbox must be selected before the form can be submitted. Any @racket[_attrs] will be used as
@tech{HTML} attributes in the text box’s @racketresultfont{<input>} tag.
 

}

@defproc[(textarea [label (or/c #f string?)]
                   [#:required? req (or/c string? boolean?) #t]
                   [#:validators validators '()]
                   [#:attributes attrs (listof (list/c symbol? string?)) '()])
          formular-field?]{

Returns a field that renders as a multi-line text input area. If @racket[required?] is not
@racket[#f], the text box must contain text before the form can be submitted. Any @racket[_attrs]
will be used as @tech{HTML} attributes in the field’s @racketresultfont{<input>} tag.

Use @racket[_attrs] to specify the size of the text box in rows and columns:

@racketblock[
(textarea "Label" #:attributes '((rows "5") (cols "33")))
]

}

@deftogether[(
@defproc[(input-date [label (or/c #f string?)]
                     [#:required? req (or/c string? boolean?) #t]
                     [#:validators validators '()]
                     [#:attributes attrs (listof (list/c symbol? string?)) '()])
          formular-field?]
@defproc[(input-time [label (or/c #f string?)]
                     [#:required? req (or/c string? boolean?) #t]
                     [#:validators validators '()]
                     [#:attributes attrs (listof (list/c symbol? string?)) '()])
          formular-field?])]{

Return fields for entering date and time values, respectively. 

Date values are returned in strings of the form @racket{yyyy-mm-dd}. Time values are returned as
strings of the form @racket{hh:mm} (24-hour format).

If @racket[required?] is not @racket[#f], the field must contain text before the form can be
submitted. Any @racket[_attrs] will be used as @tech{HTML} attributes in the field’s
@racketresultfont{<input>} tag.

}

@defform[(input-file arg)
         #:contracts ([arg any/c])]{

input-file form

}

@defform[(make-checkboxes arg)
         #:contracts ([arg any/c])]{

make-checkboxes form

}

@defform[(make-radios arg)
         #:contracts ([arg any/c])]{

make-radios form

}

@defform[(make-radios-with-other arg)
         #:contracts ([arg any/c])]{

make-radios-with-other form

}

@defform[(make-sliders arg)
         #:contracts ([arg any/c])]{

make-sliders form

}


@defform[(select/inline arg)
         #:contracts ([arg any/c])]{

select/inline form

}


@defproc[(map-result [arg any/c]) any/c]{

map-result proc

}

@defform[(map-result* arg)
         #:contracts ([arg any/c])]{

map-result* form

}

}
@;------------------------------------------------

@subsection{Form tools}

@defmodule[(submod congame/components/formular tools)]

@deftogether[(@defthing[submit-button xexpr?]
              @defproc[(submit-button/label [label string?]) xexpr?])]{

Returns a representation of an @tech{HTML} button that submits a form. The @racket[_label] argument
is used as the button’s label.

}



@;===============================================

@section[#:style 'quiet]{Transition Graphs}

@defmodule[congame/components/transition-graph]

@defform[#:literals (unquote lambda --> goto)
         (transition-graph transition-clause ...+)
         #:grammar
         [(transition-clause (code:line [id transition-entry ...+]))

          (transition-entry (code:line --> transition-target))

          (transition-target (code:line id)
                             (code:line (unquote transition-lambda)))

          (transition-lambda (code:line (lambda () transition-expr ...+))
                             (code:line (lambda name:id () transition-expr ...+)))

          (transition-expr (code:line (done))
                           (code:line (fail expr))
                           (code:line (goto id:id))
                           (code:line expr))]
         ]{

  A @racket[transition-graph] consists of one or more transition-entries. For
  example:

  @racketblock[
  (make-study
    "some study"
    #:transitions
    (transition-graph
      [a --> b --> ,(lambda ()
                      (if succes-step-b?
                        (goto bad-ending)
                        (goto good-ending)))]
      [fail-ending --> fail-ending]
      [good-ending --> good-ending])
    (list
      (make-step 'a a)
      (make-step 'b b)
      (make-step 'c c)))
  ]
}

@deftogether[(@defidform[-->]
              @defform[(goto step-id)])]{

Forms used in @racket[transition-graph]s to define transitions between study steps. Use of these
forms in places where a transition graph is not being defined will result in a syntax error.

}

@;===============================================

@section[#:style 'quiet]{Bots}

@defmodule[congame/components/bot]

@deftech{Bots} are scriptable automatons that traverse a study according
to a @deftech{bot model}. A bot is made up of one or more @tech{bot
steppers} and a @tech{bot model} is a mapping from locations in a study
to bot behaviors. A @deftech{bot stepper} is an arbitrary procedure
associated with a step that determines what the bot does when it reaches
that step.

@tktk{Need definition for @deftech{bot handlers}.}

@defproc[(bot? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @tech{bot}.
}

@defparam[current-user-bot? v any/c #:auto-value]{
  A parameter set to a non-@racket[#f] value when the current user
  is a bot, @racket[#f] otherwise.
}

@defproc[(make-bot [step (or/c bot? bot-stepper?)] ...+) bot?]{
  Returns a @tech{bot} with the given steppers.

  This is a primitive procedure used to implement @racket[study->bot].
  You should use @racket[study->bot] unless you really know what you're
  doing.
}

@defproc[(bot-stepper? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @tech{bot stepper}.
}

@defproc[(make-bot-stepper [step-id symbol?]
                           [action (-> any/c)]) bot-stepper?]{

  Returns a @tech{bot stepper} for the given @racket[step-id] and
  @racket[action] combination.

  As with @racket[make-bot], you shouldn't normally need this
  procedure.
}

@;------------------------------------------------

@subsection{Making Bots}

@defmodule[congame/components/bot-maker]

@defthing[model/c (-> (listof symbol?) procedure? any)]{
  The contract for @tech{bot models}.
}

@defproc[(study->bot [s study?]) (-> model/c bot?)]{
  Returns a procedure that generates a bot suitable for running
  against @racket[s]. The returned procedure takes a @tech{bot model}
  as input.

  The model is called every time the bot lands on a step with the path
  to that step, represented as a list of symbols, and the default bot
  action for that step. The model can then decide whether to call the
  bot action or perform its own actions, or both.

  At every step, the model can access instance and participant data
  through any study variables that it has access to. Within the dynamic
  extent of a model invocation, the current participant is the bot
  currently running the model.
}

@;------------------------------------------------

@subsection{Bot Actions}

@defmodule[(submod congame/components/bot actions)]

@defproc[(run-bot [b bot?]
                  [#:study-url url string?]
                  [#:username username string?]
                  [#:password password string?]
                  [#:delay delay real? 0]
                  [#:browser browser (or/c #f browser?) #f]
                  [#:headless? headless? boolean? #t]
                  [#:port port (or/c #f (integer-in 0 65535)) #f]) void?]{

  Runs bot @racket[b] against the study at @racket[url] with the given
  @racket[username] and @racket[password].
}

@defparam[current-page page (or/c #f page?)]{
  Within a bot action, this represents the current page.
}

@defproc[(completer) void?]{
  A bot action that stops the bot when run.
}

@defproc[(continuer) void?]{
  A bot action that continues to the next step when run.
}

@defproc[(click [id symbol?]) void?]{
  A bot action that clicks on the widget @racket[id] when run.
}

@defproc[(click-all [elts (listof element?)]) void?]{
  Clicks every element in @racket[elts].
}

@defproc[(type-all [elts&text (hash/c element? string?)]) void?]{
  Types into every element key of @racket[elts&text] the associated string.
}

@defproc[(wait-for [selector string?]) void?]{
  Waits until an element with the given @racket[selector] appears on the page.
}

@defproc[(show [selector string?]) void?]{
  Sets the @tt{display} style of the first element that matches
  @racket[selector] to @tt{block}.
}

@defproc[(find [selector string?]) (or/c #f element?)]{
  Returns the first element that matches @racket[selector].
}

@defproc[(find-all [selector string?]) (listof element?)]{
  Returns all the elements that match @racket[selector].
}

@defproc[(element-find [elt element?]
                       [selector string?]) (or/c #f element?)]{

  Returns the first child of @racket[elt] that matches @racket[selector].
}

@defproc[(element-find-all [elt element?]
                           [selector string?]) (listof element?)]{

  Returns all the children of @racket[elt] that match @racket[selector].
}

@defproc[(find-attribute [attr string?]) (or/c string? #f)]{

  Returns the value of the attribute @racket[_attr] in the first element found on the current page
  with that attribute, or @racket[#f] if no matching element/attribute is found.
}

@;------------------------------------------------

@subsection{Running Bots From Studies}

@defmodule[congame-web/components/study-bot]

@defproc[(spawn-bot [b bot?]) void?]{
  Creates a new user, adds it to the current study instance as a
  participant and launches the @tech{bot} @racket[b] in the background.
  Does not wait for the bot to finish before returning.

  Raises an exception if called outside of a step.
}
