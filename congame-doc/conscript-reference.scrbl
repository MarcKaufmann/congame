#lang scribble/manual

@(require [for-label buid
                     racket/contract
                     congame/components/bot-maker
                     conscript/base
                     conscript/markdown
                     (only-in racket/base string?)
                     xml]
          scribble/examples
          "doc-util.rkt")

@title[#:style 'toc]{Conscript Reference}

Here you can lookup individual Conscript functions and macros to thoroughly
understand their usage.

@table-of-contents[]

@(define e (make-base-eval #:lang 'racket/base))
@(e '(require conscript/base conscript/survey-tools))

@;===============================================

@section{Core}

@defmodule[conscript/base]

The bindings provided by this module are also provided by @code{#lang conscript}.

@defform[(defstep ...)]{
    @tktk{Defines a step.}
}

@defform[(defstep/study ...)]{
    @tktk{...}
}

@deftogether[(

@defform[(defvar ...)]

@defform[(defvar* ...)])]{

@tktk{Define a variable with some extra magic behind it. @racket[defvar] and @racket[defvar*] are both
particiant scope; @racket[defvar*] scope across parent/child studies. ...}

}

@deftogether[(

@defform[(defvar/instance ...)]

@defform[(defvar*/instance ...)])]{

@tktk{Like @racket[defvar] and @racket[defvar*], but scoped to the current study @tech{instance} ---
that is, the stored value is shared by all participants in the study instance.}

}

@;{ Important anti-feature to document: for `defvar*`, we need to provide a unique id to track the
variable in the DB. E.g., `(defvar* bla unique-id-for-bla)`. If any part of the same study uses the
same `unique-id-for-bla`, say because we use `bla` as the id and lazily use the same elsewhere, then
these two variables will overwrite each others values!

We used to have a check that ensured that we had unique ids, but it was too strict and doesn't work
with uploaded studies (as opposed to one's that are bundled in the source code), so we switched it
off. Since these unique ids have to be provided statically, we have to develop some checks and
debugging tools, but also just document this anti-feature. }

@defform[(defview ...)]{

@tktk{...}

}

@defform[#:literals (-->)
         (defstudy maybe-requires maybe-provides step-transition ...)
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

@tktk{More to come...}

Defines a study.

Any number of steps may be joined by transitions using @defidform/inline[-->].

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
next step in the study is loaded.

The @tt{href} attribute
is dynamically set to the URL of a continuation that first calls @racket[_action] with no arguments,
then returns the page provided by @racket[_to-step-id] (or that provided by the next step in the
study if @racket[to-step-id] is @racket[#f]).

If @racket[#:id] is provided, it sets the value of the @tt{data-widget-id} attribute for the
element.


}


@defform[(for/study ...)]{

@tktk{See loops, definition of}

}

@defform[(with-bot step-expr bot-expr)]{
  Wraps @racket[step-expr] so that its default bot is @racket[bot-expr].

  @examples[
    (require conscript/base)
    (defstep (hello)
      (button "Continue..."))
    (defstudy s
      [[hello (with-bot hello bot:continuer)] --> ,(lambda () done)])
  ]
}

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Local Testing}

@; @defmodulelang[conscript/local]

@; @defproc[(preview [study study?]) void?]{

@; @tktk{Run @racket[_study] in your browser.}

@; }

@;===============================================

@section{Page Content}

@defmodule[conscript/struct]

@defstruct[study-page ([renderer (-> xexpr?)]
                       [xexpr-validator (-> any/c xexpr?)]) #:omit-constructor]{

A study @deftech{page} is a special structure representing a complete page of HTML content. When used
as a function, the study page's @racket[_renderer] is called, producing the page’s HTML as
an X-expression.

Any definition of a study @tech{step} must be a function that produces a @racket[study-page]. (or
another study, but we won’t go into that here). But you should never need to create these values
directly; rather, you’ll typically use helper functions like @racket[md] and @racket[html] that do
this for you.

@examples[#:eval e
  (define x (md "# Heading"))
  x
  (x)]

}

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

@defmodule[conscript/html]

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

Return representations (X-expressions) of HTML tags of the same names.

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

The bindings in this module are also provided by @racketmodname[conscript/base].



@tktk{...}

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Resources}

@defmodule[conscript/resource]

@tktk{...}

@; ==============================================

@section{Survey Tools}

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

@examples[#:eval e

(define price 1.045)
(~$ price)
(~euro price)
(~pound price)
]

}

@;===============================================

@section{Matchmaking}

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
@section{Admin}

@defmodule[conscript/admin]

This module provides a combinator for wrapping a study in an admin
section. The resulting study displays an admin area to the study owner
and the passed-in study to other participants.

@defproc[(make-admin-study [s study?]
                           [#:models models (listof (cons/c symbol? model/c))]) study?]{

  Returns a new study by wrapping @racket[s] with admin functionality.

  The @racket[#:models] argument represents a list of bot models that
  can be run from the admin area.
}
