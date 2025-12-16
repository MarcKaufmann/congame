#lang scribble/manual

@(require (for-label (except-in forms form)
                     marionette
                     racket/base
                     racket/contract
                     congame-web/components/study-bot
                     congame-web/components/uploaded-file
                     congame/components/for-study
                     congame/components/bot
                     (submod congame/components/bot actions)
                     congame/components/bot-maker
                     congame/components/formular
                     congame/components/study
                     congame/components/transition-graph
                     (except-in conscript/base require button study? step?)
                     koyo/haml
                     web-server/http
                     (only-in xml xexpr?))
          scribble/examples)

@(require "doc-util.rkt")

@title[#:style 'toc]{Congame Reference}

@local-table-of-contents[]

@(define e (make-base-eval #:lang 'racket/base))
@(e '(require congame/components/study))

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

  The @racket[#:requires] and @racket[#:provides] arguments are deprecated and included for
  compatibility. Use @racket[defvar*] and @racket[defvar*/instance] to share study variables between
  parent and child studies.
}


@defproc[(run-study [s study?]
                    [req request? (current-request)]
                    [#:bindings bindings (hash/c symbol? any/c) (hasheq)]) any/c]{

  Runs the study @racket[s] under @racket[req] with @racket[bindings].
}

@defproc[(current-participant-id) integer?]{

Returns the database ID of the current participant in the current study @tech{instance}.

}

@defproc[(current-participant-owner?) boolean?]{

Returns @racket[#t] if the current participant is also the owner of the current study
@tech{instance}, @racket[#f] otherwise.

This is useful for conditionally displaying admin-only content or enabling special controls for
study owners.

}

@defproc[(current-participant-identity-user?) (or/c string? #f)]{

Returns the identity server URL if the current participant enrolled in the study through an
identity server, @racket[#f] otherwise.

Identity servers are used to decouple participant identity from their study responses, allowing
researchers to pay participants without knowing their specific answers.

}

@;------------------------------------------------

@subsection{Study variables}

A @deftech{study variable} is a variable whose value is recorded in the study server database.

@itemlist[

 @item{A variable with @tech{participant scope} will store/reference a separate value for each
 participant in each separate @tech{instance}.}

 @item{A variable with @tech{instance scope} will store/reference a separate value for each study
 @tech{instance}, but the value is shared by all participants in a given study instance.}

 ]

@defform[(defvar id)]{

Defines a @tech{study variable} with @tech{participant scope} bound to @racket[_id]. The study
variable can be accessed inside the study steps using @racket[_id] and updated with
@racket[(set! _id _expr)].

When set, the value of the study variable will be stored in the Congame server database under the
current study, instance, and participant.

@inline-note{Study variables have no default value (see @racket[undefined?]). This is intentional,
because until you store a value using @racket[set!], no value has been stored in the study
database.}

}

@defform*[((defvar* id)
          (defvar* id global-id))]{

Like @racket[defvar], but creates a variable that is additionally visible to any child studies (see
@racket[make-step/study]).

The single-argument form @racket[(defvar* _id)] must be used inside a @racket[with-namespace] block,
which automatically generates a unique global identifier. This is the recommended usage:

@racketblock[
(with-namespace my-study.variables
  (defvar* score))
]

The two-argument form @racket[(defvar* _id _global-id)] can be used outside of
@racket[with-namespace], but you must manually ensure @racket[_global-id] is distinct from any
identifiers that may be used in child studies.

}

@defform[(defvar/instance id)]{

Like @racket[defvar], but creates a variable with @tech{instance scope} --- that is, the stored
value is shared by all participants in the study instance.

}

@defform*[((defvar*/instance id)
          (defvar*/instance id global-id))]{

Like @racket[defvar*], but creates a variable with @tech{instance scope} --- that is, the stored
value is shared by all participants in the study instance, and is also visible to child studies.

The single-argument form @racket[(defvar*/instance _id)] must be used inside a
@racket[with-namespace] block. The two-argument form can be used outside of @racket[with-namespace]
but requires manually ensuring the global identifier is unique.

}

@defthing[undefined undefined?]{

A special value used internally to represent @tech{study variables} that have been created with
@racket[defvar] but not yet assigned a value.

You typically don't need to use @racket[undefined] directly. Instead, use @racket[undefined?] to
check if a variable has been set, or @racket[if-undefined] to provide a fallback value.

}

@defproc[(undefined? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{study variable} which has been created but not yet given
any value, @racket[#f] otherwise.

}

@defform[(if-undefined study-var alt)]{

Returns the current value of @racket[study-var] if it has been given a value, or @racket[alt] if
@racket[study-var] is @racket[undefined?].

}

@defform[(with-namespace namespace body ...+)]{

Wraps one or more @racket[defvar*] or @racket[defvar*/instance] definitions so that each variable
automatically receives a globally unique identifier based on @racket[_namespace].

When using @racket[defvar*] (which creates variables visible to child studies), there is a risk
that a child study could accidentally use the same variable name and overwrite the parent's data.
The @racket[with-namespace] form prevents this by prefixing each variable's internal identifier
with the namespace.

@racketblock[
(with-namespace my-study.variables
  (defvar* score)
  (defvar*/instance group-results))
]

In the example above, @racketidfont{score} will internally be stored as
@racketidfont{my-study.variables:score}, preventing collisions with a child study that might also
define a @racketidfont{score} variable.

@inline-note{Always use @racket[with-namespace] when using @racket[defvar*] or
@racket[defvar*/instance] to prevent accidental variable overwrites by child studies.}

}

@defform[(with-root root-id body ...+)]{

Wraps @racket[defvar] and @racket[defvar/instance] definitions to store them under a custom root
identifier @racket[_root-id] instead of the default @racketidfont{*root*}.

This is primarily used internally by Congame (for example, by the matchmaking system) to create
isolated variable storage namespaces. Most study authors do not need this form.

@inline-note{This form only affects variables defined with @racket[defvar] and
@racket[defvar/instance], not those defined with @racket[defvar*] or @racket[defvar*/instance].}

}

@deftogether[(

@defproc[(call-with-study-transaction [proc (-> any/c)]) any/c]

@defform[(with-study-transaction expr ...)])]{

Calls @racket[proc] (or evaluates @racket[expr ...] in such a way as to prevent any study variables
from being updated by other participants until completed. The result of @racket[proc] (or 
@racket[expr ...]) becomes the result of the expression.

@bold{Important:} You should use one of these forms when you’re doing multiple operations that
depend on each other and which involve instance-scoped variables (that is, variables with
@tech{instance scope}, created with @racket[defvar/instance] or @racket[defvar*/instance]).
Variables with instance scope can be updated by other participants at any time, so it is possible
that the variable could change between the time when its value is read and when it is updated.

Example:

@racketblock[

(defvar/instance group-score)

(code:comment2 @#,elem{...})

(define (bad-update)
  (when (code:hilite (> score 100))
    (code:comment2 @#,elem{Bad: another participant could change the score before the next line runs!})
    (set! score (code:hilite (+ score 50))))) 

(define (good-update)
  (with-study-transaction (code:comment @#,elem{Prevents changes by anyone else during this code})
    (when (code:hilite (> score 100))
      (set! score (code:hilite (+ score 50))))))

]

Note that each highlighted expression above is a separate reference to the @racketvarfont{score}
variable; without the use of a transaction, other study participants could change its value at any
point between those expressions.

You don’t need @racket[with-study-transaction] for single operations; a single read, or a single
write of a literal value, is safe. But the moment you have a sequence of operations where later
steps depend on earlier ones, and those steps touch instance variables, use
@racket[with-study-transaction] to keep everything atomic and consistent.

In concrete technical terms, @racket[call-with-study-transaction] enters a database transaction with
an isolation level of @racket['serializable].

}

@defproc[(get/linked/instance [pseudonym symbol?]
                              [key symbol?]
                              [default any/c (lambda () (error ...))]
                              [#:root root-id symbol? '*root*])
         any/c]{

Retrieves an instance-scoped @tech{study variable} from a @emph{linked} study instance identified
by @racket[_pseudonym].

This is an advanced feature used when multiple study instances are linked together (for example,
when one study needs to access aggregate data from another). The @racket[_pseudonym] identifies
the linked instance, and @racket[_key] specifies which variable to retrieve.

If the variable is not found and @racket[_default] is a procedure, it is called and its result
returned. Otherwise, @racket[_default] is returned directly.

@inline-note{Study instance linking is typically configured at the server level and is used for
cross-study data sharing. Most studies do not need this function.}

}

@;------------------------------------------------

@subsection{Low-level data access}

A @deftech{step scope} represents the region of the database where data for a study step is
stored and retrieved. Step scope is determined by the combination of the current participant,
the study stack, and optional round and group information. @tech{Instance scope} (used by
@racket[defvar/instance]) is shared between all participants in a study @tech{instance}.

The functions below provide direct access to the underlying data storage. In most cases, you
should use @racket[defvar] and related forms instead, which provide a more convenient interface.

@defproc[(put [key symbol?]
              [value any/c]
              [#:root root-id symbol? '*root*]
              [#:round round-stack (listof string?) (list "")]
              [#:group group-stack (listof string?) (list "")]
              [#:participant-id participant-id integer? (current-participant-id)])
         void?]{

Stores @racket[_value] under the symbol @racket[_key] in the current @tech{step scope}.

@inline-note{This is a low-level function. Use @racket[defvar] and @racket[set!] instead for
most use cases.}

The optional keyword arguments allow storing data in different scopes:

@itemlist[
@item{@racket[#:root] specifies the root namespace for storage (default @racket['*root*])}
@item{@racket[#:round] and @racket[#:group] specify round and group context for the data}
@item{@racket[#:participant-id] allows storing data for a different participant (must be in the
same study instance)}
]

}

@defproc[(get [key symbol?]
              [default (or/c any/c (-> any/c))
                       (lambda () (error 'get "value not found for key ~.s" key))]
              [#:root root-id symbol? '*root*]
              [#:round round-stack (listof string?) (list "")]
              [#:group group-stack (listof string?) (list "")]
              [#:participant-id participant-id integer? (current-participant-id)])
         any/c]{

Retrieves the value stored under the symbol @racket[_key] from the current @tech{step scope}.

If no value exists for @racket[_key], @racket[_default] is called if it is a procedure, or
returned directly otherwise.

@inline-note{This is a low-level function. Use @racket[defvar] instead for most use cases.}

The optional keyword arguments mirror those of @racket[put] and allow retrieving data from
different scopes.

}

@;------------------------------------------------

@subsection{Participant groups}

Congame supports organizing participants into named groups within a study. This is useful for
implementing group-based activities such as games or collaborative tasks. Group membership is
tracked per participant.

@deftogether[(

@defproc[(get-current-group-name) string?]

@defproc[(put-current-group-name [group-name string?]
                                 [#:participant-id participant-id integer? (current-participant-id)])
         void?])]{

@racket[get-current-group-name] returns the current participant's group name. If the participant
has not been assigned to a group, returns an empty string @racket[""].

@racket[put-current-group-name] assigns the current participant (or the participant specified by
@racket[_participant-id]) to the group named @racket[_group-name].

These functions are typically used with matchmaking logic to pair participants. For example:

@racketblock[
(with-study-transaction
  (when (string=? (get-current-group-name) "")
    (code:comment @#,elem{Participant not yet in a group, assign them})
    (put-current-group-name "group-1")))
]

@inline-note{For most use cases involving participant matching, consider using the higher-level
matchmaking functions from @racketmodname[conscript/matchmaking] instead of these low-level
primitives.}

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

Creates a @tech{step} for use in a study.

The @racket[_id] argument is a symbol that uniquely identifies this step within the study.

The @racket[_handler] argument is a procedure that takes no arguments and returns an
@tech[#:doc '(lib "xml/xml.scrbl")]{X-expression} representing the step's page content.

The @racket[_transition] argument is a procedure that returns the next step to transition to
after this step completes. It can return @racket[next] to proceed to the next step in the
list, @racket[done] to end the study, or a symbol naming a specific step.

The @racket[#:view-handler] argument, if provided, specifies a @tech{view handler} for this step.

The @racket[#:for-bot] argument, if provided, specifies a @tech{bot handler} that determines
what a @tech{bot} should do when it reaches this step. If not provided and the step has a
custom transition, an error will be raised when a bot reaches the step.

@racketblock[
(make-step 'greeting
           (lambda ()
             (haml
              (:div
               (:h1 "Welcome!")
               (button void "Continue")))))
]

}

@defproc[(make-step/study [id symbol?]
                          [s (or/c study? (-> study?))]
                          [transition transition/c (lambda () next)]
                          [#:require-bindings require-bindings (listof binding/c) null]
                          [#:provide-bindings provide-bindings (listof binding/c) null]) step?]{

  Creates a @tech{step} that executes the child study @racket[s] when reached.

  The @racket[s] argument can be either a study value or a procedure that returns a study. When
  @racket[s] is a procedure, it is called when the step is reached, allowing the study structure to
  depend on runtime values (such as participant responses from earlier steps). This is essential for
  dynamically generated studies using @racket[for/study].

  The @racket[#:require-bindings] and @racket[#:provide-bindings] arguments are deprecated and
  included for compatibility. Use @racket[defvar*] and @racket[defvar*/instance] to share study
  variables between parent and child studies.

  Embedding a dynamically generated study:

  @racketblock[
  (define (make-substudy)
    (for/study ([i (in-range n)])
      (question-step i)))

  (make-step/study 'questions make-substudy)
  ]

  Here, @racket[make-substudy] is called when the step is reached, after @racket[n] has been set by
  a previous step.
}

@defproc[(map-step [s step?]
                   [proc (-> handler/c handler/c)])
         step?]{

Transforms the step @racket[_s] by applying @racket[_proc] to its handler.

The @racket[_proc] argument receives the step's current handler (a procedure that returns an
X-expression) and should return a new handler. This allows wrapping or modifying step behavior
without changing the original step definition.

If @racket[_s] is a @racket[make-step/study] step containing a child study, @racket[map-step]
recursively applies the transformation to all steps in that child study.

See also @racket[map-study].

}

@defproc[(map-study [s study?]
                    [proc (-> handler/c handler/c)])
         study?]{

Transforms every step in the study @racket[_s] by applying @racket[_proc] to each step's handler.

This is useful for adding consistent behavior across all steps in a study, such as wrapping each
page with common styling, adding logging, or injecting validation.

@racketblock[
(define (add-border handler)
  (lambda ()
    (haml
     (:div ([:style "border: 1px solid black;"])
       (handler)))))

(define bordered-study
  (map-study my-study add-border))
]

The transformation is applied recursively to any child studies contained within
@racket[make-step/study] steps.

}

@deftogether[(
  @defthing[#:kind "canary" next next?]
  @defproc[(next? [v any/c]) boolean?]
)]{
  A special value that can be used as a transition result to cause a
  study to transition to the next step, whatever step that may be.

  The predicate @racket[next?] returns @racket[#t] if @racket[_v] is
  identical to @racket[next], @racket[#f] otherwise.

}

@deftogether[(
  @defthing[#:kind "canary" done done?]
  @defproc[(done? [v any/c]) boolean?]
)]{
  A special value that can be used as a transition result to cause a
  transition to the end of the study.

  The predicate @racket[done?] returns @racket[#t] if @racket[_v] is
  identical to @racket[done], @racket[#f] otherwise.

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

@subsection[#:tag "congame-step-timings"]{Step Timings}

Congame automatically tracks timing information for each step in a study. This timing data
measures how long participants spend on each page, including both total elapsed time and
the time the page was actively in focus (visible to the participant).

@defparam[current-step-timings timings (cons/c (or/c #f number?) (or/c #f number?))]{

Returns a pair @racket[(cons _total-time _focus-time)] containing timing information for the current
step, where both values are measured in milliseconds.

The @racket[_total-time] is the total elapsed time since the participant first loaded the page,
including time spent with the page in the background (for example, if they switched to another
browser tab).

The @racket[_focus-time] is the total time the page was actually visible and in focus. This excludes
time when the page was in a background tab or the browser window was minimized.

Both values will be @racket[#f] if no timing data is available (for example, on the very first page
load of a study).

In @hash-lang[] @racketmodname[conscript], this function is available as @racket[get-step-timings]. 

@inline-note[#:type 'warning]{@bold{Important:} This parameter should @italic{only} be accessed
within a widget's action procedure or during a step transition. Accessing
@racket[current-step-timings] directly within a step’s handler (inside @racket[defstep]) will
produce undefined behavior and unreliable values. The timing data is only properly set when
processing user actions like button clicks or form submissions.}

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

@defform[#:literals (set! ~error ~errors ~all-errors)
         (formular maybe-bot maybe-fields form-body maybe-action)
         #:grammar
         [(maybe-bot (code:line)
                     (code:line #:bot ([bot-id (field-kwd bot-value) ...] ...)))
          (maybe-fields (code:line)
                        (code:line #:fields ([field-id field-expr] ...)))
          (form-body (code:line xexpr-with-fields))
          (maybe-action (code:line)
                        (code:line action-expr))]]{

Creates a form from an X-expression template containing embedded @tech{field} declarations.

Fields can be declared in two ways within @racket[_form-body]:

@itemlist[

@item{@bold{Keyword syntax}: @racket[(#:field-name _field-expr)] where @racket[_field-expr]
produces a @tech{field}. The field's value will be passed to @racket[_action-expr] as a keyword
argument.}

@item{@bold{Set! syntax}: @racket[(set! _var-id _field-expr)] where @racket[_var-id] is a
@tech{study variable}. When the form is submitted, the variable is automatically updated with
the field's value. This syntax cannot be combined with a custom @racket[_action-expr].}

]

Optional default values can be specified using @racket[(#:field-name _field-expr {#:default _value})].

The @racket[#:bot] clause defines autofill values for @tech{bots}. Each @racket[_bot-id] names
a bot configuration, and the keyword/value pairs specify what values to fill in for each field.
Use @racket[formular-autofill] in the step's bot handler to trigger the autofill.

The @racket[#:fields] clause allows defining fields dynamically, useful when field order needs
to be randomized or fields are computed at runtime.

Within @racket[_form-body], you can use these special forms to display validation errors:

@itemlist[

@item{@racket[(~error _field-id)] or @racket[(~error #:field-name)] — displays errors for a
specific field}

@item{@racket[(~errors _field-id ...)] — displays errors for multiple fields}

@item{@racket[(~all-errors)] — displays all validation errors}

]

@racketblock[
(formular
 #:bot
 ([ok (#:name "Alice")
      (#:age 30)])
 (haml
  (:div
   (:p "Name: " (#:name (input-text)))
   (:p "Age: " (#:age (input-number #:min 0 #:max 120)))
   ,@(~all-errors)
   (:button ([:type "submit"]) "Submit"))))
]

Using set! syntax with study variables:

@racketblock[
(defvar participant-name)
(defvar participant-age)

(formular
 (haml
  (:div
   (:p "Name: " (set! participant-name (input-text)))
   (:p "Age: " (set! participant-age (input-number)))
   (:button ([:type "submit"]) "Submit"))))
]

}

@defproc[(formular-field? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[_v] is a @tech{field}, @racket[#f] otherwise.

}

@defproc[(formular-autofill [bot-id symbol?]) void?]{

Automatically fills in and submits a form created with @racket[formular] when running as a
@tech{bot}.

The @racket[_bot-id] argument must match one of the bot identifiers declared in the
@racket[#:bot] clause of the @racket[formular] macro. The function reads autofill metadata
embedded in the page by @racket[formular], fills in each form field with the corresponding
value, and clicks the submit button.

For example, given a form defined with:

@racketblock[
(formular
 #:bot
 ([ok (#:emissions 42)
      (#:location "Cluj-Napoca")])
 (haml
  (:div
   (#:emissions (input-number))
   (#:location (input-text))
   (:button ([:type "submit"]) "Submit"))))
]

The bot handler can use @racket[(formular-autofill 'ok)] to fill in @racket[42] for the
@racketidfont{emissions} field, @racket["Cluj-Napoca"] for the @racketidfont{location} field,
and submit the form.

This function handles different input types appropriately: text and number fields are filled
by typing, checkboxes and radio buttons are clicked, and select dropdowns have their values
set directly.

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

@defproc[(input-file [label (or/c #f string?) #f]
                      [#:required? required? (or/c string? boolean?) #t]
                      [#:validators validators (listof procedure?) null]
                      [#:attributes attributes (listof (list/c symbol? string?)) null])
         formular-field?]{

Returns a @tech{field} that renders as a file upload input.

When the form is submitted, the field's value is a @racket[binding:file] struct containing the
uploaded file's name, headers, and content.

If @racket[_label] is provided, the file input is wrapped in a label element displaying
@racket[_label]. If @racket[_required?] is not @racket[#f], the user must select a file before
the form can be submitted.

}

@defproc[(make-checkboxes [options (listof (cons/c symbol? any/c))]
                           [render-proc (-> (listof (cons/c symbol? any/c))
                                            (-> symbol? string? xexpr?)
                                            xexpr?)]
                           [#:n n exact-nonnegative-integer? 0]
                           [#:message message (or/c #f string?) #f]
                           [#:validators validators (listof procedure?) null]
                           [#:attributes attributes (listof (list/c symbol? string?)) null])
         formular-field?]{

Returns a @tech{field} containing multiple checkboxes with custom rendering.

The @racket[_options] argument is a list of pairs, where each pair contains a symbol (the
checkbox value) and associated data (typically a label string, but can be any value your
render procedure uses).

The @racket[_render-proc] argument is a procedure that controls how the checkboxes are
rendered. It receives the @racket[_options] list and a @racketidfont{make-checkbox} function.
The @racketidfont{make-checkbox} function takes a symbol (the checkbox value) and an optional
label string, and returns an X-expression for a labeled checkbox input.

When @racket[_n] is greater than 0, the user must check at least @racket[_n] boxes before the
form can be submitted. If @racket[_message] is provided, it is shown when this validation
fails.

@racketblock[
(make-checkboxes
 '((apple . "Apple")
   (banana . "Banana")
   (cherry . "Cherry"))
 (lambda (options make-checkbox)
   (haml
    (:ul
     ,@(for/list ([opt (in-list options)])
         (haml (:li (make-checkbox (car opt) (cdr opt))))))))
 #:n 1
 #:message "Please select at least one fruit.")
]

@inline-note{For simpler use cases where you don't need custom rendering, consider using
@racket[make-multiple-checkboxes] from @racketmodname[conscript/survey-tools], which provides
a default list-style rendering.}

}

@defproc[(make-radios [options (listof (cons/c symbol? any/c))]
                       [render-proc (-> (listof (cons/c symbol? any/c))
                                        (-> symbol? string? xexpr?)
                                        xexpr?)]
                       [#:required? required? (or/c string? boolean?) #t]
                       [#:validators validators (listof procedure?) null]
                       [#:attributes attributes (listof (list/c symbol? string?)) null])
         formular-field?]{

Returns a @tech{field} containing radio buttons with custom rendering.

The @racket[_options] argument is a list of pairs, where each pair contains a symbol (the
radio button value) and associated data (which can be any value your render procedure uses).

The @racket[_render-proc] argument is a procedure that controls how the radio buttons are
rendered. It receives the @racket[_options] list and a @racketidfont{make-radio} function.
The @racketidfont{make-radio} function takes a symbol (the radio value) and an optional
label string, and returns an X-expression for a labeled radio input.

If @racket[_required?] is not @racket[#f], the user must select one of the radio options
before the form can be submitted.

This example renders radio buttons in a table format:

@racketblock[
(make-radios
 '((mac1 . ("Apple Mac" "White"))
   (mac2 . ("Apple Mac" "Gray"))
   (dell1 . ("Dell" "Blue")))
 (lambda (options make-radio)
   (haml
    (:table
     (:thead
      (:tr (:th "") (:th "Brand") (:th "Color")))
     (:tbody
      ,@(for/list ([opt (in-list options)])
          (haml
           (:tr
            (:td (make-radio (car opt)))
            (:td (car (cdr opt)))
            (:td (cadr (cdr opt)))))))))))
]

@inline-note{For standard vertical or horizontal radio layouts, consider using the simpler
@racket[radios] field instead.}

}

@defproc[(make-radios-with-other [options (listof (cons/c symbol? string?))]
                                  [#:required? required? (or/c string? boolean?) #t]
                                  [#:validators validators (listof procedure?) null])
         formular-field?]{

Returns a @tech{field} containing radio buttons with an additional "Other" option that includes
a text input field.

The @racket[_options] argument is a list of pairs where each pair contains a symbol (the radio
value) and a string (the displayed label).

When the user selects one of the predefined options, the field's value is that option's symbol
(as a string). When the user types text in the "Other" field, the field's value is whatever
they typed.

If @racket[_required?] is not @racket[#f], the user must either select one of the radio options
or provide text in the "Other" field before the form can be submitted.

@racketblock[
(make-radios-with-other
 '((often . "Often")
   (sometimes . "Sometimes")
   (rarely . "Rarely")))
]

}

@defproc[(make-sliders [n exact-positive-integer?]
                        [render-proc (-> exact-nonnegative-integer? string? (or/c number? #f) xexpr?)]
                        [#:message message (or/c #f string?) #f]
                        [#:validators validators (listof procedure?) null])
         formular-field?]{

Returns a @tech{field} containing @racket[_n] slider inputs with custom rendering.

The @racket[_render-proc] argument is a procedure that renders a single slider. It receives
three arguments: the slider's index (starting from 0), the field name (a string), and the
current value (a number, or @racket[#f] if no value is set). It should return an X-expression
containing an @tt{<input>} element with @tt{type="range"} and @tt{name} set to the provided
field name.

When the form is submitted, the field's value is a list of numbers corresponding to each
slider's value.

@racketblock[
(make-sliders
 3
 (lambda (idx name current-value)
   (haml
    (:div
     (:label (format "Slider ~a: " (add1 idx))
             (:input ([:type "range"]
                      [:name name]
                      [:min "0"]
                      [:max "100"]
                      [:value (or current-value "50")])))))))
]

@inline-note{For simpler slider usage in Conscript, consider the @racket[make-sliders] macro
from @racketmodname[conscript/survey-tools], which provides a more convenient syntax.}

}


@defproc[(select/inline [options (listof (cons/c string? string?))]
                         [#:required? required? (or/c string? boolean?) #t]
                         [#:validators validators (listof procedure?) null]
                         [#:attributes attributes (listof (list/c symbol? string?)) null])
         formular-field?]{

Returns a @tech{field} that renders as a dropdown select element without a label or wrapper.

This is similar to @racket[select] but renders only the @tt{<select>} element itself, making
it suitable for embedding inline within text or other custom layouts.

The @racket[_options] argument is a list of pairs where each pair contains a value string and
a display label string.

@racketblock[
(haml
 (:p "I prefer "
     (#:color (select/inline '(("red" . "Red")
                               ("blue" . "Blue")
                               ("green" . "Green"))))
     " as my favorite color."))
]

}


@defproc[(map-result [field formular-field?]
                      [proc (-> any/c any/c)])
         formular-field?]{

Transforms the value of @racket[_field] by applying @racket[_proc] after validation succeeds.

When the form is submitted and @racket[_field] passes validation, @racket[_proc] is called
with the validated value. The result of @racket[_proc] becomes the field's final value.

@racketblock[
(code:comment @#,elem{Convert a text input to a symbol})
(map-result (input-text) string->symbol)

(code:comment @#,elem{Square a number input})
(map-result (input-number) (λ (n) (* n n)))
]

}

@defproc[(map-result* [field formular-field?]
                       [proc (-> any/c any/c)]
                       [#:exn-predicate exn-predicate (-> any/c boolean?) exn:fail?]
                       [#:exn-handler exn-handler (-> exn? (cons/c 'err string?))
                                      (λ (e) (cons 'err (exn-message e)))])
         formular-field?]{

Like @racket[map-result], but catches exceptions raised by @racket[_proc] and converts them
to validation errors.

If @racket[_proc] raises an exception matching @racket[_exn-predicate], the exception is caught
and @racket[_exn-handler] is called. The handler should return a pair @racket[(cons 'err _message)]
where @racket[_message] is the error string to display to the user.

This is useful when the transformation might fail on certain inputs:

@racketblock[
(code:comment @#,elem{Parse a number, showing a friendly error on failure})
(map-result* (input-text)
             string->number
             #:exn-handler (λ (_) (cons 'err "Please enter a valid number")))
]

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

@section[#:style 'quiet]{File Uploads}

@defmodule[congame-web/components/uploaded-file]

This module provides functions for handling file uploads in studies running on the Congame server.

@defproc[(upload-file! [binding binding:file?]
                       [#:prefix prefix (or/c #f string?) #f])
         uploaded-file?]{

Saves an uploaded file to the server and returns an @racket[uploaded-file] record containing
metadata about the stored file.

The @racket[_binding] argument should be a file binding from a form submission (typically
obtained from an @racket[input-file] field).

If @racket[_prefix] is provided, it is prepended to the original filename with a hyphen
separator. This is useful for organizing uploaded files or adding participant identifiers.

}

@defstruct*[uploaded-file ([key string?]
                           [filename string?]
                           [content-type string?])]{

A structure representing an uploaded file's metadata.

The @racket[_key] field is a unique identifier used internally to retrieve the file.
The @racket[_filename] field contains the original filename (potentially with a prefix added).
The @racket[_content-type] field contains the MIME type of the uploaded file.

}

@defproc[(uploaded-file-attachment [file uploaded-file?]
                                    [label string?])
         xexpr?]{

Creates an attachment X-expression from an uploaded file record.

The @racket[_label] argument specifies the display text for the attachment link.

}

@defproc[(valid-pdf? [binding binding:file?])
         (or/c (cons/c 'ok binding:file?)
               (cons/c 'err string?))]{

A validator function that checks if an uploaded file is a PDF.

Returns @racket[(cons 'ok _binding)] if the file's content-type is @racket{application/pdf},
or @racket[(cons 'err "the file must be a PDF")] otherwise.

Use this with the @racket[#:validators] argument of @racket[input-file] to restrict uploads
to PDF files only.

}


@;===============================================

@section[#:style 'quiet]{Transition Graphs}

@defmodule[congame/components/transition-graph]

@defform[#:literals (unquote lambda --> goto quote)
         (transition-graph transition-clause ...+)
         #:grammar
         [(transition-clause (code:line [id transition-entry ...+ maybe-expr]))

          (transition-entry (code:line --> id))

          (maybe-expr (code:line)
                      (code:line ,(lambda () transition-expr ...+))
                      (code:line ,(lambda name:id () transition-expr ...+)))

          (transition-expr (code:line '(done))
                           (code:line '(fail _))
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
                      (if (not success-step-b?)
                          (goto bad-ending)
                          (goto good-ending)))]
      [fail-ending --> ,(lambda () '(fail 0))]
      [good-ending --> good-ending])
    (list
      (make-step 'a a)
      (make-step 'b b)
      (make-step 'good-ending good)
      (make-step 'fail-ending failed)))
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

A @deftech{bot handler} is a procedure associated with a step that defines what a @tech{bot}
should do when it reaches that step. Bot handlers typically interact with the page (clicking
buttons, filling forms) and then trigger the transition to the next step.

Bot handlers are specified using the @racket[#:for-bot] argument of @racket[make-step] or
the @racket[#:bot] clause of @racket[formular]. Common bot actions include @racket[continuer]
(to proceed to the next step), @racket[click] (to click a specific widget), and
@racket[formular-autofill] (to fill in form fields automatically).

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
