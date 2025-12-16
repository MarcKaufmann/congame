#lang scribble/manual

@(require [for-label buid
                     congame/components/bot-maker
                     conscript/admin
                     (only-in conscript/base
                              add-css
                              add-css-resource
                              defbox
                              define-var-box
                              defstep
                              defstep/study
                              defstudy
                              defview
                              button
                              get-step-timings
                              put/identity
                              require
                              with-bot
                              ~current-view-uri
                              log-conscript-debug
                              log-conscript-error
                              log-conscript-fatal
                              log-conscript-info
                              log-conscript-warning)
                     (except-in congame/components/study button form)
                     congame/components/transition-graph
                     conscript/form0
                     conscript/html
                     (except-in forms form)
                     conscript/markdown
                     conscript/matchmaking
                     conscript/resource
                     conscript/survey-tools
                     (only-in congame/components/formular formular-field?)
                     (except-in racket/base require)
                     (rename-in racket/base (require rkt:require))
                     racket/contract
                     web-server/servlet
                     xml]
          scribble/examples
          (only-in conscript/base %whitelist)
          "doc-util.rkt")

@title[#:style 'toc]{Conscript Reference}

@local-table-of-contents[]

@(define e (make-base-eval #:lang 'racket/base))
@(e '(require (except-in conscript/base require) conscript/survey-tools))

@;===============================================

@section[#:style 'quiet]{Core}

@declare-exporting[conscript/base]
@defmodule[conscript/base #:no-declare]

The bindings provided by this module are also provided by @code{#lang conscript}.

@defform[(defstep (step-id) step-body)]{

@margin-note{For examples of @racket[defstep] in use, see @secref["intro"].}

Defines a study @tech{step}. The @racket[_step-body] must be an expression that evaluates to a
study @tech{page} — usually @racket[md] or @racket[html].

}

@defform[(defstep/study step-id #:study child-study-expr
                        maybe-require-bindings
                        maybe-provide-bindings)
         #:grammar
         [(maybe-require-bindings (code:line)
                                  (code:line #:require-bindings ([child-id parent-id] ...)))
          (maybe-provide-bindings (code:line)
                                  (code:line #:provide-bindings ([parent-id child-id] ...)))]]{

Defines a study step named @racket[_step-id] that runs @racket[_child-study-expr] when reached.

The @racket[_child-study-expr] can be either a study value or a procedure that returns a study. When
it is a procedure, it is called when the step is reached, allowing the study structure to depend on
runtime values (such as participant responses from earlier steps). This is essential for dynamically
generated studies using @racket[for/study].

The step @racket[_step-id] is said to be part of the "parent" study, of which
@racket[_child-study-expr] becomes a "child" study.

The @racket[#:require-bindings] and @racket[#:provide-bindings] arguments are deprecated and
included for compatibility. Use @racket[defvar*] and @racket[defvar*/instance] to share study
variables between parent and child studies.

}


@defform[#:literals (--> lambda unquote done goto quote)
         (defstudy study-id
                   maybe-wrapper
                   maybe-requires
                   maybe-provides
                   transition-clause ...+)
         #:grammar
         [(maybe-wrapper (code:line)
                         (code:line #:wrapper wrapper-expr))
          (maybe-requires (code:line)
                          (code:line #:requires (value-id-sym ...)))
          (maybe-provides (code:line)
                          (code:line #:provides (value-id-sym ...)))
          (transition-clause [step --> transition ... maybe-lambda])
          (transition (code:line --> step)
                      (code:line --> {step-id step}))
          (maybe-lambda (code:line)
                        (code:line --> ,(lambda () transition-expr)))
          (transition-expr (code:line done)
                           (code:line (goto step))
                           (code:line '(fail _))
                           (code:line 'step-id)
@;{Returning 'step-id from unquoted lambda also seems to work, but this appears redunant to goto}
                           (code:line expr))
          ]]{

Defines a study in terms of steps joined by transitions. Each @racket[step] should be a step defined
with @racket[defstep] or @racket[defstep/study].

If @racket[#:wrapper] is supplied, @racket[wrapper-expr] must produce a function that takes a study
as its only argument and returns a study. (See, for example, @racket[add-css] and
@racket[add-css-resource].)

An unquoted lambda at the end of a @racket[transition-clause] can include arbitrary @racket[expr],
but this but must evaluate to one of the other possible @racket[transition-expr]s:

@racketblock[
(defstudy college-try
  [attempt --> ,(lambda () (if success
                               (goto good-ending)
                               (goto bad-ending)))]
  [bad-ending --> ,(lambda () '(fail 0))]
  [good-ending --> good-ending])
]

For any “final” step (that is, a step that, once reached, should prevent the participant from taking
any further step) you need to include a separate @racket[_transition-clause] with the step at both
ends, or a @racket[maybe-lambda] expression that returns @racket[done]:

@racketblock[
(defstudy mystudy
  [intro --> question --> final]
  [final --> final])
(code:comment @#,elem{OR:})
(defstudy mystudy
  [intro --> question --> final --> ,(lambda () done)])
]

You can reuse the same step function as separate steps with the @racket[--> {_step-id _step}] form of
@racket[_transition]:

@racketblock[
(defstudy repeating-step-study
  [intro --> {message1 message}
         --> {message2 message}
         --> final]
  [final --> final])
]

The use of @racket[#:requires] and @racket[#:provides] arguments is deprecated and included for
compatibility. Use @racket[defvar*] and @racket[defvar*/instance] to share study variables between
parent/child studies.

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

@(define racketrequire @racketlink[rkt:require]{@racketidfont{require}})

@defform[(require require-spec ...)]{

Conscript provides its own binding for @racketrequire that only allows modules from the whitelist
below. This prevents unsafe code from running on Congame servers.

@(keyword-apply itemlist '(#:style) '(compact)
  (for/list ([modname (in-list (%whitelist))])
    @item{@racketmodname[#,modname]})) @; font[(symbol->string modname)]}))

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
next transition to the next step in the study is run.

The @tt{href} attribute
is dynamically set to the URL of a continuation that first calls @racket[_action] with no arguments,
then returns the page provided by @racket[_to-step-id] (or that provided by the next step in the
study if @racket[to-step-id] is @racket[#f]).

If @racket[#:id] is provided, it sets the value of the @tt{data-widget-id} attribute for the
element.

@inline-note[#:type 'warning]{You must call @racket[button] from inside a function definition. If
you call it from the “top level” of a study or module you will get an error.}

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

@subsection{Views}

A @deftech{view} is an additional set of content/functionality that can be associated with a
@tech{step}. Views can be used to provide additional instructions for a step, or to give admins a
customized display of study outcomes.

The content of a @tech{view} is provided by a @deftech{view handler}, which is a function which
takes a single @racket[request] argument (which can be inspected if the view needs to vary depending
on the HTTP request) and returns a @racket[response]. This function will be called by the Congame
server when the view is accessed.

@defform[(defview (id req) body ...)
         #:contracts ([req request?])]{

Defines a @tech{view handler}. When the @tech{view} is accessed, Congame will call the view handler,
passing the HTTP @racket[request] as the @racket[_req] argument.

As with @racket[defstep], the @racket[_body] of a @racket[defview] expression should evaluate to a
study @tech{page}, usually using @racket[md] or @racket[html]. The @racket[defview] form takes care
of converting the page to an HTTP @racket[response].

Example:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defview (instructions-popup _request)
  @md{#Instructions

      More detail.....})
}|

}

@defproc[(~current-view-uri) string?]{

Returns the URI of the view handler page associated with the current step. Intended for use inside
@racket[defstep].

}

@;------------------------------------------------

@subsection{CSS}

@defproc[(add-css [css-string string?]) (-> study? study?)]{

Returns a procedure, suitable for use with the @racket[#:wrapper] argument of @racket[defstudy],
which adds the custom CSS contained in @racket[css-string] to each step in the study.

}

@defproc[(add-css-resource [css-res resource?]) (-> study? study?)]{

Returns a procedure, suitable for use with the @racket[#:wrapper] argument of @racket[defstudy],
which adds the custom CSS @tech{resource} @racket[css-res] to each step in the study.

@inline-note{See @github-link{congame-example-study/conscript-css-resource.rkt} for an example of
how to use this function.}

}

@;------------------------------------------------

@subsection[#:tag "conscript-step-timings"]{Step Timings}

Congame automatically tracks timing information for each step in a study. This timing data measures
how long participants spend on each page, including both total elapsed time and the time the page
was actively in focus (visible to the participant).

@defproc[(get-step-timings) (cons/c (or/c #f number?) (or/c #f number?))]{

Returns a pair @racket[(cons _total-time _focus-time)] containing timing information for the current
step, where both values are measured in milliseconds.

The @racket[_total-time] is the total elapsed time since the participant loaded the page, including
time spent with the page in the background (for example, if they switched to another browser tab).

The @racket[_focus-time] is the total time the page was actually visible and in focus. This excludes
time when the page was in a background tab or the browser window was minimized.

Both values will be @racket[#f] if no timing data is available (for example, on the very first page
load of a study).

See @secref["cookbook-step-timings"] for an example showing how to use @racket[get-step-timings].

@inline-note[#:type 'warning]{@bold{Important:} This function should @italic{only} be called within
a @racket[button]'s action procedure or within code that runs after a form is submitted. Calling
@racket[get-step-timings] directly within the body of a @racket[defstep] (outside of an action
procedure) will return @racket[(cons #f #f)] because the timing data is only available when
processing user actions like button clicks or form submissions.}

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

@subsubsection{Additional box functions}

@declare-exporting[conscript/var-box conscript/base]
@defmodule[conscript/var-box #:no-declare]

The bindings from this module are also provided by @racketmodname[conscript/base].

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


@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@; @subsection{Local Testing}

@; @defmodulelang[conscript/local]

@; @defproc[(preview [study study?]) void?]{

@; @tktk{Run @racket[_study] in your browser.}

@; }

@;===============================================

@section[#:style 'quiet]{Page Content}

A study @deftech{page} is an @X-expression representing a complete page of HTML content.

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

Parses its contents as Markdown and produces an @tech[#:doc '(lib "xml/xml.scrbl")]{X-expression}
representing a complete @tech{page} containing the resulting HTML content (@racket[md]) or of a
fragment of HTML suitable for use within another page (@racket[md*]).

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

Returns a complete @tech{page} of HTML content (@racket[html]), or a representation of a fragment of
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

Any keyword arguments supplied are converted to attribute names/values in the resulting HTML
representation. Keyword arguments specifically noted above are required for their respective
forms.

@examples[#:eval e
  (a #:href "/" "a link")
  (eval:error (a "/" "a link"))
  (a #:class "my-style" #:href "/" "styled link")]

}

@;------------------------------------------------

@subsection{Static Resources}

@defmodule[conscript/resource]

This module provides a way to access @deftech{resources}: images and other static files that aren’t
stored in the database. The files for linked resources get uploaded automatically as long as they're
linked using @racket[define-static-resource]. Or you can upload a zipped folder as long as the study
is contained/provided from specifically named @filepath{study.rkt} inside that zip file.

@defform[(define-static-resource name path-string)]{

Registers the file at @racket[_path-string] as a static resource, and binds @racket[_id] to a
@racketresultfont{#<resource>} struct value for that file.

Use this in a @filepath{study.rkt} file and make @racket[_path-string] a path that is relative to
that file. Then bundle the referenced files into a zip file before uploading to the Congame server.
See @secref["image-howto"] for more details.

}

@defproc[(resource-uri [r resource?] [subr (or/c list? #f) #f]) string?]{

Generates an absolute URL for the resource @racket[r] on the current study server. The value
@racket[r] must be declared using @racket[define-static-resource].

}


@defproc[(resource? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{resource} defined with @racket[define-static-resource],
@racket[#f] otherwise.

}

@;===============================================

@section{Form0}

@defmodule[conscript/form0]

@(define formform @racketlink[dyn:form]{@racketidfont{form}})
@(define cgs/form @racketlink[cgs:form]{@racketidfont{form}})

@(e '(require conscript/form0))

This module reprovides nearly all the bindings in the @racketmodname[forms] library for creating
forms in your study pages, as well as some additional conveniences for using that library.

See the @seclink["forms-recipe"] for steps and examples to combine these procedures into working
forms in your study steps.

In addition to the tutorials in this documentation, see @other-doc['(lib "forms/forms.scrbl")] for
a tutorial that walks through the functionality of Forms.

@defform[(form+submit [id formlet-expr] ...)]{

Returns two values: a @formform value and a procedure that is called when the form is submitted.

Each @racket[_id] should be a variable declared with @racket[defvar] or similar, and each
@racket[_formlet-expr] should be one of the values in @secref["formlets" #:doc '(lib
"forms/forms.scrbl")], or an @racket[ensure] expression combining one of those formlets with one or
more @secref["validators" #:doc '(lib "forms/forms.scrbl")].

@examples[#:eval e
(defvar my-name)
(defvar my-middle-name)
(defvar my-age)
(define-values (form-data on-submit)
  (form+submit
    [my-name (ensure binding/text (required))]
    [my-middle-name binding/text]
    [my-age (ensure binding/number (required))]))
]

}


@defproc[(form [f form?]
               [action (-> void?)]
               [render (-> (widget-renderer/c) xexpr?)]
               [#:id id string? ""]
               [#:enctype enctype string? "multipart/form-data"]
               [#:combine combine-proc (-> any/c any/c any/c any/c)
                                       (λ (k v1 v2)
                                         (if (pair? v1)
                                             (append v1 (list v2))
                                             (list v1 v2)))]
               [#:defaults defaults hash? (hash)]) xexpr?]{

Renders the form represented by @racket[f] (created using @racket[form+submit] or @racket[form*])
using @racket[render] and executes @racket[action] on successful submission, then continues to the
next step in the study.

Identical to the @cgs/form from @racketmodname[congame/components/study] except for the default
@racket[combine-proc]: when there are multiple bindings for the same field, this procedure’s default
@racket[combine-proc] combines all the bindings for that field into a list (as described in the
documentation for @racket[form-run]).

}

@defproc[(required-unless [pred (-> any/c)])
         (-> (or/c string? #f)
             (or/c (cons/c 'ok any/c)
                   (cons/c 'err string?)))]{

Similar to @racket[required], produces a validator procedure that ensures a value is present (i.e.
not @racket[#f]), except that if @racket[pred] produces a non-false value at validation time, then the
value is allowed to be absent. Use inside @racket[ensure] when building forms.

}

@defproc[(at-least [n number?])
         (-> (or/c number? #f)
             (or/c (cons/c 'ok any/c)
                   (cons/c 'err string?)))]{

Produces a validator procedure that ensures a numeric value is greater than or equal to
@racket[n]. If the value is @racket[#f] (indicating an empty field), validation passes.
Use inside @racket[ensure] when building forms with numeric inputs.

@examples[#:eval e
(defvar contribution)
(define-values (form-data on-submit)
  (form+submit
    [contribution (ensure binding/number (at-least 0))]))
]

}

@defproc[(number-in-range [lo number?] [hi number?])
         (-> (or/c number? #f)
             (or/c (cons/c 'ok any/c)
                   (cons/c 'err string?)))]{

Produces a validator procedure that ensures a numeric value falls within the range
[@racket[lo], @racket[hi]] (inclusive). If the value is @racket[#f] (indicating an empty field),
validation passes. Use inside @racket[ensure] when building forms with numeric inputs that must
stay within a specific range.

@examples[#:eval e
(defvar rating)
(define-values (form-data on-submit)
  (form+submit
    [rating (ensure binding/number (number-in-range 1 10))]))
]

}

@defproc[(list-longer-than [n exact-nonnegative-integer?])
         (-> any/c
             (or/c (cons/c 'ok any/c)
                   (cons/c 'err string?)))]{

Produces a validator procedure that ensures a value is a non-empty list containing at least
@racket[n] items. Unlike @racket[at-least] and @racket[number-in-range], this validator
requires the value to be present and will fail if the value is @racket[#f] or not a list.
Use inside @racket[ensure] with @racket[binding/list] when building forms that collect multiple
values from inputs sharing the same field name (such as multiple checkboxes or number inputs).

@examples[#:eval e
(defvar ratings)
(define-values (form-data on-submit)
  (form+submit
    [ratings (ensure binding/list (list-longer-than 3))]))
]

}

@deftogether[(

@defproc[(checkbox [label (or/c string? #f) #f] [#:attributes attrs null]) widget/c]
@defproc[(input-date [label (or/c string? #f) #f] [#:attributes attrs null]) widget/c]
@defproc[(input-datetime [label (or/c string? #f) #f] [#:attributes attrs null]) widget/c]
@defproc[(input-email [label (or/c string? #f) #f] [#:attributes attrs null]) widget/c]
@defproc[(input-file [label (or/c string? #f)]) widget/c]
@defproc[(input-number [label (or/c string? #f) #f] [#:attributes attrs null]) widget/c]
@defproc[(input-range [label (or/c string? #f) #f] [#:attributes attrs null]) widget/c]
@defproc[(input-text [label (or/c string? #f) #f] [#:attributes attrs null]) widget/c]
@defproc[(input-time [label (or/c string? #f) #f] [#:attributes attrs null]) widget/c]
@defproc[(textarea [label (or/c string? #f) #f] [#:attributes attrs null]) widget/c]
)]{

Returns a widget that can render the given input type.

Note: @racket[input-file] creates a file upload widget and does not accept the
@racket[#:attributes] parameter.

@examples[#:eval e
  ((checkbox) "agree" #f null)
  ((input-time) "arrived" #f null)
]

}

@deftogether[(
@defproc[(radios [options radio-options/c]
                 [label (or/c string? #f) #f]
                 [#:attributes attrs null]) widget/c]
@defproc[(select [options radio-options/c]
                 [label string?]
                 [#:attributes attrs null]) widget/c]
@defproc[(checkboxes [options radio-options/c]
                     [#:attributes attrs null]) widget/c]
)]{

Return a widget that can render @racket[options] in the given input type.

@examples[#:eval e
  (define opts
    '(("high" . "Take the high road")
      ("low" . "Go low")))
  ((radios opts "Metaphorical highway selection") "road" #f null)
  ((checkboxes opts) "roadboxen" #f null)
]

}

@defproc[(make-autofill [v any/c]) xexpr?]{

Renders a @racketresultfont{<meta>} element  whose @racketresultfont{content} attribute varies
depending on the current user: when the current user is a @tech{bot}, it contains @racket[v]
serialized via @racket[write]; otherwise the attribute is empty.

Use this when rendering step @tech{pages} to instruct the bot how to fill in certain fields.

@(e '(require congame/components/bot))
@examples[#:eval e
  (make-autofill (hasheq 'example (hasheq 'name "Frank" 'mood "Delicate")))
  (parameterize ([current-user-bot? #t])
    (make-autofill (hasheq 'example (hasheq 'name "Frank" 'mood "Delicate"))))
]

}

@defproc[(make-autofill-meta [ht hash?]) xexpr?]{

Renders a @racketresultfont{<meta>} element with @racketresultfont{name="formular-autofill"}
and a @racketresultfont{content} attribute containing @racket[ht] serialized via @racket[write].
The autofill content is always included, regardless of the current user type.

Use this when rendering step @tech{pages} to provide autofill instructions that should always
be present in the page markup.

@examples[#:eval e
  (make-autofill-meta (hasheq 'example (hasheq 'name "Frank" 'mood "Delicate")))
]

}



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
(~$ 8)
]

}

@defproc[(round-to-places [num real?] [p real?]) real?]{

Returns @racket[num] rounded to @racket[p] decimal places, resolving ties in favor of an even
number. 

If @racket[p] is not an exact integer, it will be rounded to an exact integer to determine the
number of decimal places for rounding. If @racket[p] is zero, the result is the same as
@racket[(round num)]. If it is less than zero, the result will be zero. 

@examples[#:eval e

(round-to-places 3.14159 2)
(round-to-places 3.5 0)
(round-to-places 4.5 0)
(round-to-places 1.23 -1)
(round-to-places 8 2)
]

}

@defthing[diceroll-js xexpr?]{

An X-expression containing a @tt{<script>} element that enables dice roll functionality.

Include this in a step that has a container element with class @racketidfont{diceroll}. Inside
that container, place an @tt{<a>} element with class @racketidfont{button} (the roll button)
and an @tt{<output>} element (where the result will be displayed). When the button is clicked,
a random number from 1 to 6 is generated and displayed in the output element.

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (roll-dice)
  @md{# Roll the Dice

      @diceroll-js

      @div[#:class "diceroll"]{
        @a[#:class "button" #:href ""]{Roll}
        @output{}}

      @button{Continue}})
}|

}

@defproc[(questions [arg any/c]) any/c]{

@tktk{questions proc}

}

@defproc[(slider-js) xexpr?]{

Returns an X-expression containing a @tt{<script>} element that enables real-time slider
value display.

Include the result of calling this function in a step that has one or more container elements
with class @racketidfont{slider}. Each container should have an @tt{<input type="range">}
element and an @tt{<output>} element. As the user moves the slider, the current value is
automatically displayed in the output element.

This function is typically used internally by the @racket[make-sliders] macro, but can be
used directly when building custom slider interfaces.

}

@defproc[(timer [n exact-positive-integer?]) xexpr?]{

Returns an X-expression representing a countdown timer that displays @racket[_n] seconds
remaining.

The timer counts down and displays the remaining time as "@italic{X} seconds left" (or
"@italic{Y} minutes and @italic{Z} seconds left" for times over 60 seconds). When the timer
reaches zero, it automatically submits any form on the page, or clicks the next button if
no form is present.

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (timed-task)
  @md{# Complete the task

      You have 60 seconds to complete this task.

      @timer[60]

      @form{...}})
}|

}

@defform[(assigning-treatments arg)
         #:contracts ([arg any/c])]{

@tktk{assigning-treatments form --- probably deprecated (related to matchmaking)}

}

@defproc[(is-equal [expected any/c]
                    [#:message message (or/c #f string?) #f])
         (-> any/c (or/c (cons/c 'ok any/c) (cons/c 'err string?)))]{

Returns a validator procedure that checks if a form field's value equals @racket[_expected].

If the value equals @racket[_expected], validation passes. Otherwise, validation fails with
@racket[_message] (or a default message like "Should be equal to @racket[_expected]" if
@racket[_message] is @racket[#f]).

This is useful for creating quiz-like forms where there's a specific correct answer:

@racketblock[
(radios '(("a" . "Option A")
          ("b" . "Option B")
          ("c" . "Option C"))
        #:validators (list (is-equal "c" #:message "That's not correct, try again!")))
]

}

@;Not documenting the optional render-proc argument because it's complicated, its default value
@;is a binding that's not provided outside the module, and it appears to be for internal use only.
@defproc[(make-multiple-checkboxes [options (listof (cons/c symbol? string?))]
                                   [#:n num-required exact-nonnegative-integer? 0]
                                   [#:exactly-n? exactly-n? boolean? #f]
                                   [#:message message (or/c #f string?)])
          formular-field?]{

@margin-note{See @secref["How_to_have_a_form_input_with_multiple_checkboxes"] for more examples of
this function in use.}

Returns a @tech{field} containing multiple checkboxes defined by the @racket[_options] list. The
@racket[_num-required] argument specifies the minimum number of checkboxes the user must check
before they can submit the form. If @racket[_message] is not @racket[#f], it will be shown to
the participant @mark{if they don’t check at least @racket[_num-required] boxes.}

When @racket[#:exactly-n?] is @racket[#t], the user is expected to
check exactly @racket[n] checkboxes, otherwise they get a validation
error.

}

@defform[(make-sliders n maybe-widget-proc)
         #:grammar
         [(maybe-widget-proc (code:line)
                             (code:line widget-proc-expr))]
         #:contracts
         ([n exact-positive-integer?]
          [widget-proc-expr (-> exact-nonnegative-integer? formular-field?)])]{

Creates a form containing @racket[_n] sliders with real-time value display.

This macro generates a complete form with @racket[_n] range input sliders, each wrapped in
a container with a live-updating value display. The form includes a submit button.

If @racket[_widget-proc-expr] is provided, it should be a procedure that takes a slider
index (starting from 0) and returns a @racket[formular-field?]. By default, each slider
is created using @racket[input-range].

@racketblock[
(code:comment @#,elem{Create a form with 3 default sliders})
(make-sliders 3)

(code:comment @#,elem{Create a form with custom slider ranges})
(make-sliders 5
  (lambda (idx)
    (input-range #:min 0 #:max 100 #:step 5)))
]

}

@defproc[(toggleable-xexpr [message string?]
                            [xexpr xexpr?]
                            [#:hidden? hidden? boolean? #t])
         xexpr?]{

Returns an X-expression representing a collapsible content section with a toggle button.

The @racket[_message] argument is displayed as the button text. Clicking the button shows
or hides the content specified by @racket[_xexpr].

When @racket[_hidden?] is @racket[#t] (the default), the content starts hidden and must be
clicked to reveal. When @racket[_hidden?] is @racket[#f], the content starts visible.

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (instructions)
  @md{# Task Overview

      @toggleable-xexpr["Show/Hide Detailed Instructions"
                        @md*{## Detailed Instructions

                             1. First, do this...
                             2. Then, do that...}]

      @button{Continue}})
}|

}


@;===============================================

@section[#:style 'quiet]{Matchmaking}

@declare-exporting[conscript/matchmaking conscript/base]
@defmodule[conscript/matchmaking #:no-declare]

The bindings in this module are also provided by @racketmodname[conscript/base].

These functions are used for matching participants into groups. Tehy take care of the mechanics of
pausing the study until groups are full, and of sharing results between group members.

A @deftech{matchmaker function} is used to collect participants into groups of a given size,
displaying a “Please wait” @tech{step} until the group is full. A @deftech{pending group} is one
that has one or more members but still needs more to meet its quota; a @deftech{ready group} has met
its quota of assigned participants and is ready to proceed.

@defproc[(make-matchmaker [group-size exact-positive-integer?]
                          [group-ok? (-> buid/c boolean?) values]) (-> (-> xexpr?) any/c)]{

Returns a @tech{matchmaker function} that accepts one argument (that argument being a study step
function) and which adds the current participant to the current pending group (creating a new group
if all other groups are full already), and then either skips to the next step in the study (if the
current group now has @racket[_group-size] members) or loads the step @tech{page} provided by the
argument.

@margin-note{@mark{You should avoid calling @racket[make-matchmaker] more than once with different
values of @racket[group-size] in the same study.}}

A @racket[group-ok?] procedure can be supplied in order to give the study author a way to prevent
adding the participant to certain groups. The procedure will be called with the identifier of the
candidate group, which can be used as a key to the hash returned by @racket[get-pending-groups]. The
procedure must return @racket[#t] if the participant can be added to the candidate group or
@racket[#f] if not. (During the body of the @racket[group-ok?] procedure, the hash table returned by
@racket[get-pending-groups] will not yet include the @racket[current-participant-id].)

}

@defproc[(get-current-group) (or/c buid/c #f)]{

Returns the @seclink["Spec" #:doc '(lib "buid/buid.scrbl")]{BUID} of the group the current
participant is assigned to, or @racket[#f] if not currently assigned to any group.

If the result is not @racket[#f], it can be used as a key for the hash returned by either
@racket[get-ready-groups] or @racket[get-pending-groups] (depending on whether the current
participant is in a @tech{ready group} or a @tech{pending group}) to get a list of participant IDs.

}

@defproc[(get-ready-groups) (hash/c buid/c (listof integer?))]{

Returns a hash table of all currently @tech{ready groups}; that is, groups which, as of the last
call to a @tech{matchmaker function}, have been assigned the number of participants given as
@racket[_group-size] in the call to @racket[make-matchmaker] which produced the matchmaker.

Each key in the hash table is a group ID and references a list of participant IDs assigned to that
group.

}

@defproc[(get-pending-groups)  (hash/c buid/c (listof integer?))]{

Returns a hash table of all currently @tech{pending groups}; that is, groups which, as of the last
call to a @tech{matchmaker function}, have been assigned one or more participants but still fewer
than the number of participants given as @racket[_group-size] in the call to
@racket[make-matchmaker] which produced the matchmaker.

Each key in the hash table is a group ID and references a list of participant IDs assigned to that
group.

}

@defproc[(current-group-members [#:include-self? include-self? #f]) (listof integer?)]{

If the current participant is a member of a @tech{ready group}, returns a list the members
in that group. If the current participant is not in a group @mark{or is in a group that is only
partially full}, an empty list is returned.

By default, current participant’s own ID is @bold{not} included in the returned list, but if
@racket[include-self?] is not @racket[#f] then the returned list will include all members of the
group.

}

@defproc[(reset-current-group) void?]{

Removes the participant from any group to which they have been assigned (whether it was filled or
not).

@mark{If, at the time the participant’s current group is reset, they were in a group that was only
partially filled, then a subsequent call to a @tech{matchmaker function} may add them back to the
same group (causing their ID to appear more than once in that group’s member list). If they were in
a filled group, the participant’s ID will remain among the list of the original group members.}

}

@defproc[(store-my-result-in-group! [lookup-key any/c] [val any/c]) void?]{

Records @racket[val] in a table of information within the current group so that it can be referenced
with @racket[lookup-key] (see @racket[other-group-member-results]).

If the current participant is not a member of a group, no data will be recorded.

@inline-note{Note when reading group member results stored with this function, @racket[#f] is
returned in cases where a group member has not recorded any response for the given
@racket[lookup-key]. So storing a @racket[val] of @racket[#f] will make that response
indistinguishable from cases where no response has been recorded.}

}

@defproc[(get-my-result-in-group [lookup-key any/c]) any/c]{

Retrieves the value stored under @racket[lookup-key] for the current participant within their
currently assigned group. If the participant is not a member of a group, or if they have not
previously stored a value under @racket[lookup-key] within the current group, @racket[#f] is
returned.

}

@defproc[(current-group-member-results [lookup-key any/c] 
                                       [#:include-ids? ids? #f]
                                       [#:include-self? include-self? #f])
         (or/c (listof (cons/c id/c any/c))
               (listof any/c))]{

Returns a list containing the result stored under @racket[lookup-key] for members of the
current group. For any member that has not yet stored a value under @racket[_lookup-key],
@racket[#f] will be returned.

If the current participant is not a member of a group, an empty list is returned.

If @racket[ids?] is not @racket[#f], then each element the returned list will be a pair of the form
@racket[(_id . _result)] where @racket[_id] is the ID of the participant that recorded the result.
If @racket[ids?] is @racket[#f], the returned list will simply contain all the @racket[_result]
values.

By default, current participant’s own result is @bold{not} included in the returned list, but if
@racket[include-self?] is not @racket[#f] then the returned list will include a response recorded
by the current participant under @racket[lookup-key], if one exists, or @racket[#f] if it does not
exist. 

As an example, assume the current participant has an ID of @racket[100] and is paired with
participant @racket[199]. If the current participant has recorded a response of @racket["no"] under
lookup key @racket['has-eaten] and the other group member has not yet recorded a response under that
key:

@racketblock[

(current-group-member-results 'has-eaten) (code:comment @#,elem{→ '(#f)})

(current-group-member-results 'has-eaten #:include-ids? #t) (code:comment @#,elem{→ '((199 . #f))})
(current-group-member-results 'has-eaten #:include-self? #t) (code:comment @#,elem{→ '("no" #f)})
(current-group-member-results 'has-eaten 
                              #:include-self? #t
                              #:include-ids? #t) (code:comment @#,tt{→ '((100 . "no") (199 . #f))})

]

}  

@defproc[(current-group-results-count [lookup-key any/c] 
                                      [#:include-self? include-self #f])
         exact-nonnegative-integer?]{

Returns the count of results recorded under @racket[lookup-key] for members of the current group.

If the current participant is not a member of a group, the result will be @racket[0].

By default, current participant’s own result is @bold{not} counted, but if @racket[include-self?] is
not @racket[#f] then the count will reflect any responses recorded by the current participant under
@racket[lookup-key], if any. 

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
