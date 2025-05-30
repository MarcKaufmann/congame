#lang scribble/manual

@(require [for-label buid
                     racket/contract
                     congame/components/bot-maker
                     ;(except-in congame/components/study button)
                     (only-in congame/components/formular formular-field?)
                     conscript/base
                     conscript/markdown
                     (only-in racket/base string?)
                     xml]
          scribble/examples
          "doc-util.rkt")

@title[#:style 'toc]{Conscript Reference}

@local-table-of-contents[]

@(define e (make-base-eval #:lang 'racket/base))
@(e '(require (except-in conscript/base require) conscript/survey-tools))

@;===============================================

@section[#:style 'quiet]{Core}

@defmodule[conscript/base #:use-sources (conscript/var-box)]

The bindings provided by this module are also provided by @code{#lang conscript}.

@defform[(defstep (step-id) step-body)]{

@margin-note{For examples of @racket[defstep] in use, see @secref["intro"].}

Defines a study @tech{step}. The @racket[_step-body] must be an expression that evaluates to a
@racket[study-page] — usually @racket[md] or @racket[html].

}

@defform[(defstep/study step-id #:study child-study-expr 
                        maybe-require-bindings
                        maybe-provide-bindings)
         #:grammar
         [(maybe-require-bindings (code:line)
                                  (code:line #:require-bindings ([child-id parent-id] ...)))
          (maybe-provide-bindings (code:line)
                                  (code:line #:provide-bindings ([parent-id child-id] ...)))]]{
    

Defines a study step that runs @racket[_child-study-expr] when reached. 

The step @racket[_step-id] is said to be part of the “parent” study, of which
@racket[_child-study-expr] becomes a “child” study.

@;{Review below -- may be deprecated}

The @racket[#:require-bindings] argument is used to map identifiers required by the child study to
identifiers available in the current study context if their names differ. Any identifiers required
by the child study that are not mapped here are assumed to be named identically to identifiers in
the parent study context, and @racket[defstep/study] will attempt to map them accordingly.

The @racket[#:provide-bindings] argument can be used to map identifiers in the parent study to
particular identifiers provided by @racket[_child-study-expr] upon completion. When
@racket[#:provide-bindings] is not specified, no values are assigned.

}

@defform[(defview ...)]{

@tktk{...}

}

@defform[#:literals (-->)
         (defstudy study-id
                   maybe-requires
                   maybe-provides 
                   step-transition ...)
         #:grammar
         [(maybe-requires (code:line)
                          (code:line #:requires (value-id-sym ...)))
          (maybe-provides (code:line)
                          (code:line #:provides (value-id-sym ...)))
          (step-transition [step --> step maybe-more-steps])
          (maybe-more-steps (code:line)
                            (code:line --> step ...))
         ]
         #:contracts
         ([value-id-sym symbol?]
          [step step?])]{

Defines a study in terms of steps joined by transitions. The transitions follow the same grammar as
@racket[transition-graph].

For any “final” step (that is, a step that marks the study’s end or one of the study’s endings) you
must include a separate @racket[_step-transition] with the step at both ends.

Example:

@racketblock[
(defstudy college-try
  [attempt --> ,(lambda () (if success
                               (goto good-ending)
                               (goto bad-ending)))]
  [bad-ending --> bad-ending]
  [good-ending --> good-ending])
]

}

@defproc[(put/identity [key symbol?] [value any/c]) void?]{

If the user enrolled in the study via an identity server, stores @racket[_value] under @racket[_key]
within their enrollment on that server; if the user did not enroll through an identity server, it
stores the value under @racket[_key] directly on the Congame server.

The point of enrolling via an identity server is to prevent the study author from connecting a
participant’s answers with their identity --- but there are cases where a well-designed study will
still need to attach some data to their identity. For example, at the end of a study you might call
@racket[(put/identity 'payment-due pay-amount)] so that, despite not knowing the participant’s
answers, you know how much you need to pay them.

}

@defproc[(~current-view-uri) string?]{

Returns the URI of the current view handler page.

}

@defproc[(button [action-proc (-> any/c) void]
                 [label string?]
                 [#:id id string? ""]
                 [#:to-step-id to-step-id (or/c identifier? #f) #f]
                 [#:up-target up-target string? ".step"]
                 [#:up-transition up-transition string? "none"]) xexpr?]{

Returns a representation of an HTML @html-tag{a} element styled as a button that navigates to the
next step in the study when clicked.

If @racket[_action-proc] is provided, it will be called when the button is clicked, just before the
next step in the study is loaded. @mark{Rather: before the transition is run.}

The @tt{href} attribute
is dynamically set to the URL of a continuation that first calls @racket[_action] with no arguments,
then returns the page provided by @racket[_to-step-id] (or that provided by the next step in the
study if @racket[to-step-id] is @racket[#f]).

If @racket[#:id] is provided, it sets the value of the @tt{data-widget-id} attribute for the
element.


}

@defform[(with-bot step-expr bot-expr)]{
  Wraps @racket[step-expr] so that its default bot is @racket[bot-expr].

  @racketblock[
    (defstep (hello)
      (button "Continue..."))
    (defstudy s
      [[hello (with-bot hello bot:continuer)] --> ,(lambda () done)])
  ]
}

@;------------------------------------------------

@subsection{Boxes}


@defform[(defbox id)]{

Defines two functions, @racketkeywordfont{get-}@racket[_id] (which takes no arguments and returns
the value of @racket[_id]) and @racketkeywordfont{set!-}@racket[_id] (which takes one argument and
updates the value of @racket[_id]).

@examples[#:eval e
(define flux 81)
(defbox flux)
(get-flux)
(set!-flux "new")
(get-flux)
flux

]

}

@defform[(define-var-box id var)]{

Binds @racket[_id] to a function that can take zero arguments or one argument. If given no
arguments, the function returns the current value of @racket[_var] (which must already be defined
elsewhere); if given a single argument, the function @racket[set!]s the value of @racket[_var] to
that value.

The function @racket[_id] can be passed to other functions, allowing them to access or update a
local variable.

@examples[#:eval e
(define flux #f)
(define-var-box get-or-set flux)

(get-or-set)
(get-or-set 'foo)
(get-or-set)
flux
]

}



@;------------------------------------------------

@subsection{Logging}

@deftogether[(
@defform*[[(log-conscript-debug string-expr)
           (log-conscript-debug format-string-expr v ...)]]
@defform*[[(log-conscript-info string-expr)
           (log-conscript-info format-string-expr v ...)]]
@defform*[[(log-conscript-warning string-expr)
           (log-conscript-warning format-string-expr v ...)]]
@defform*[[(log-conscript-error string-expr)
           (log-conscript-error format-string-expr v ...)]]
@defform*[[(log-conscript-fatal string-expr)
           (log-conscript-fatal format-string-expr v ...)]])]{

@margin-note{See @secref["logging" #:doc '(lib "scribblings/reference/reference.scrbl")] in the 
@italic{Racket Reference} for more information on logging.}

Logs an event with the Conscript logger, evaluating @racket[string-expr] or @racket[(format
format-string-expr v ...)]. These forms are listed above in order of severity.

Logging functions are useful for debugging: for example, when you want to capture and surface
state information in the middle of a process, or extra context about possible causes of a problem.

Logged events are printed on the console (@tt{stderr}) of a running Congame server.

Note that since the result of a @racketkeywordfont{log-conscript-}@racket[_level] form is
@|void-const|, you can't use it directly inside a study step. Instead, wrap it in a 
@racket[begin] expression that returns an empty string:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (age-name-survey)
  @md{
    # Survey
 
    @form{
      What is your first name? @(set! first-name (input-text))
 
      @(begin
        (log-conscript-info "Hello")
        "")
      @submit-button
  }})
}|

}

@screenshot{ref-log-example-output.png}

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@; @subsection{Local Testing}

@; @defmodulelang[conscript/local]

@; @defproc[(preview [study study?]) void?]{

@; @tktk{Run @racket[_study] in your browser.}

@; }

@;===============================================

@section[#:style 'quiet]{Page Content}

A study @deftech{page} is an @tech[#:doc '(lib "xml/xml.scrbl")]{X-expression} representing a
complete page of HTML content.

Any definition of a study @tech{step} must be a function that produces an x-expression (or
another study, but we won’t go into that here). But you should never need to create these values
directly; rather, you’ll typically use helper functions like @racket[md] and @racket[html] that do
this for you.

@examples[#:eval e
  (define x (md "# Heading"))
  x
  ]

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Markdown}

@defmodule[conscript/markdown]

The bindings provided by this module are also provided by @racketmodname[conscript/base].

@deftogether[(

@defform[(md content-str ...) #:contracts ([content-str string?])]
@defform[(md* content-str ...) #:contracts ([content-str string?])])]{

Parses its contents as Markdown and produces a representation of a complete @tech{page}
containing the resulting HTML content (@racket[md]) or of a fragment of HTML suitable for use
within another page (@racket[md*]).

@examples[#:eval e
  (md "# Heading")
  (md* "# Heading")]

}

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{HTML}

@defmodule[conscript/html #:use-sources (conscript/html-element)]

The bindings in this module are also provided by @racketmodname[conscript/base].

@deftogether[(

@defform[(html element ...) #:contracts ([element xexpr?])]
@defform[(html* element ...) #:contracts ([element xexpr?])])]{

Returns a complete @tech{page} of HTML content (@racket[html]) or a representation of a fragment of
HTML suitable for use within another page (@racket[html*]).

@examples[#:eval e
  (html (div "content..."))
  (html* (div "content..."))]

}

@deftogether[(

@defform[(a #:href href content ...)]
@defform[(aside content ...)]
@defform[(audio #:src src content ...)]
@defform[(blockquote content ...)]
@defform[(br)]
@defform[(dd content ...)]
@defform[(div content ...)]
@defform[(dt content ...)]
@defform[(em content ...)]
@defform[(h1 content ...)]
@defform[(h2 content ...)]
@defform[(h3 content ...)]
@defform[(h4 content ...)]
@defform[(h5 content ...)]
@defform[(h6 content ...)]
@defform[(img #:alt alt #:src src content ...)]
@defform[(label content ...)]
@defform[(li content ...)]
@defform[(ol content ...)]
@defform[(output content ...)]
@defform[(p content ...)]
@defform[(pre content ...)]
@defform[(script content ...)]
@defform[(section content ...)]
@defform[(span content ...)]
@defform[(strong content ...)]
@defform[(style content ...)]
@defform[(table content ...)]
@defform[(tbody content ...)]
@defform[(td content ...)]
@defform[(th content ...)]
@defform[(thead content ...)]
@defform[(tr content ...)]
@defform[(u content ...)]
@defform[(ul content ...)]
@defform[(video #:src src content ...) #:contracts ([content xexpr?])]
)]{

Return representations (X-expressions) of @tech{HTML} tags of the same names.

Any keyword arguments supplied are converted attribute names/values in the resulting HTML
representation. Keyword arguments specifically noted above are required for their respective
forms.

@examples[#:eval e
  (a #:href "/" "a link")
  (eval:error (a "/" "a link"))
  (a #:class "my-style" #:href "/" "styled link")]

}

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Forms}

@defmodule[conscript/form]

In addition to the bindings documented here, this module also reprovides most of the bindings in
@racketmodname[congame/components/formular].

The bindings provided by this module are also provided by @racketmodname[conscript/base].

@defproc[(bot:autofill [arg any/c]) any/c]{

@tktk{bot:autofill proc}

}


@defproc[(radios [arg any/c]) any/c]{

radios proc

}

@defproc[(select [arg any/c]) any/c]{

select proc

}

@defform[(binding arg)
         #:contracts ([arg any/c])]{

binding form

}

@defform[(form arg)
         #:contracts ([arg any/c])]{

form form

}

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Resources}

@defmodule[conscript/resource]

@tktk{A way to access static resources that aren’t stored in the database. The files get uploaded
automatically as long as they're linked using `define-static-resource`. (Or you can upload a zipped
folder as long as the study is contained/provided from study.rkt}

@; ==============================================

@section[#:style 'quiet]{Survey Tools}

@defmodule[conscript/survey-tools]

The bindings in this module are also provided by @racketmodname[conscript/base].

@defproc[(refresh-every [n-seconds exact-positive-integer?]) xexpr?]{

Returns a representation of an @tech{HTML} @html-tag{script} element that causes the browser to
reload the current page every @racket[_n-seconds].

}

@deftogether[(@defproc[(~$ [n rational?]) string?]
              @defproc[(~euro [n rational?]) string?]
              @defproc[(~pound [n rational?]) string?])]{

Returns a string representing @racket[_n] to two decimal places and prefixed with a currency symbol.

@tktk{Should include a way to use something other than `.` for the decimal separator}

@examples[#:eval e

(define price 1.045)
(~$ price)
(~euro price)
(~pound price)
]

}

@defproc[(diceroll-js [arg any/c]) any/c]{

@tktk{diceroll-js proc}

}

@defproc[(questions [arg any/c]) any/c]{

@tktk{questions proc}

}

@defproc[(slider-js [arg any/c]) any/c]{

@tktk{slider-js proc}

}

@defproc[(timer [arg any/c]) any/c]{

@tktk{timer proc}

}

@defform[(assigning-treatments arg)
         #:contracts ([arg any/c])]{

@tktk{assigning-treatments form --- probably deprecated (related to matchmaking)}

}

@defform[(is-equal arg)
         #:contracts ([arg any/c])]{

@tktk{is-equal form}

}

@;Not documenting the optional render-proc argument because it's complicated, its default value
@;is a binding that's not provided outside the module, and it appears to be for internal use only.
@defproc[(make-multiple-checkboxes [options (listof (cons/c symbol? string?))]
                                   [#:n num-required exact-nonnegative-integer? 0]
                                   [#:message message (or/c #f string?)])
          formular-field?]{ 

@margin-note{See @secref["How_to_have_a_form_input_with_multiple_checkboxes"] for more examples of
this function in use.}

Returns a @tech{field} containing multiple checkboxes defined by the @racket[_options] list. The
@racket[_num-required] argument specifies the minimum number of checkboxes the user must check
before they can submit the form. If @racket[_message] is not @racket[#f], it will be shown to
the participant @mark{if they don’t check at least @racket[_num-required] boxes.}

}

@defform[(make-sliders arg)
         #:contracts ([arg any/c])]{

make-sliders form

}

@defform[(toggleable-xexpr arg)
         #:contracts ([arg any/c])]{

toggleable-xexpr form

}


@;===============================================

@section[#:style 'quiet]{Matchmaking}

@defmodule[conscript/matchmaking]

The bindings in this module are also provided by @racketmodname[conscript/base].

@defproc[(make-matchmaker [group-size exact-positive-integer]) (-> (-> study-page?) any/c)]{

Returns a function that accepts one argument (a study step function) and which adds the current
participant to the current pending group (creating a new group if all other groups are full
already), and then either skips to the next step in the study (if the current group has
@racket[_group-size] members) or loads the step @tech{page} provided by the argument.

}

@defproc[(get-current-group) (or/c buid/c #f)]{

Returns the @seclink["Spec" #:doc '(lib "buid/buid.scrbl")]{BUID} of the group the current
participant is assigned to, or @racket[#f] if not currently assigned to any group.

}


@;===============================================
@section[#:style 'quiet]{Admin}

@defmodule[conscript/admin]

This module provides a combinator for wrapping a study in an admin
section. The resulting study displays an admin area to the study owner
and the passed-in study to other participants.

@defproc[(make-admin-study [s study?]
                           [#:models models (listof (cons/c symbol? model/c))]) study?]{

@tktk{Remove?}

  Returns a new study by wrapping @racket[s] with admin functionality.

  The @racket[#:models] argument represents a list of bot models that
  can be run from the admin area.
}


