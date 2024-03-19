#lang scribble/manual

@(require (for-label conscript racket/base))

@title{Conscript}

@section{Goals and Philosophy}

@deftech{conscript} aims to provide a simpler and streamlined syntax for
writing studies compared to @tech{congame}. There are two dialects of
@tech{conscript}:

@itemize[
  @item{Plain @tech{conscript}, which cannot require @tech{Racket}
    functions that we are not providing by default.}
  @item{@deftech{conscript/with-require}, which allows the user to
    require.}
]

Since @tech{conscript/with-require} provides a mechanism to require
arbitrary Racket packages, it has essentially the same power as full
@tech{congame}, while @tech{conscript} can only require a limited set
of libraries and functionality of Racket. Due to these limitations, we
can allow such studies to be uploaded to a congame server by users with
``researcher'' accounts, and be run without requiring access to the
code repository. This is not possible (out of security concerns) with
@tech{conscript/with-require} studies.

@section{Grammar}

@racketgrammar*[
  #:literals (require provide defvar defstep defstep/study defstudy unquote -->)

  [definition (require module ...)
              (provide id ...)
              (defvar var-id unique-id-expr)
              (defstep (step-id arg-id ...)
                expr ...+)
              (defstep/study step-id
                #:study study-expr
                maybe-require-bindings
                maybe-provide-bindings)
              (defstudy study-id
                maybe-requires
                maybe-provides
                [transition] ...+)
              racket-definition]

  [maybe-require-bindings (code:line)
                          (code:line #:require-bindings ([local-id parent-id] ...))]

  [maybe-provide-bindings (code:line)
                          (code:line #:provide-bindings ([parent-id local-id] ...))]

  [maybe-requires (code:line)
                  (code:line #:requires [require-id ...])]

  [maybe-provides (code:line)
                  (code:line #:provides [provide-id ...])]

  @; FIXME: Add goto?
  [transition [step-id --> transition]
              [step-id --> ,done]
              [step-id --> ,(lambda () expr)]
              [step-id --> {step-id expr}]]
]

@section{How Tos}

The Tutorial explains how to use conscript. Here we list some @emph{How Tos}
that are not covered in a terse style, providing examples to achieve a given
goal without providing the full study around or explaining how it works.

@subsection{How to add links}

To provide a link on a study page, use the anchor tag @racket[a]:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (links)
  @md{# Links

      A link to the @a[#:href
      "https://docs.totalinsightmanagement.com/Conscript_Tutorial.html"]{Conscript
      Tutorial}.

      Sometimes you want a link to open in a new tab, so you provide the
      attribute `target` with the value `"_blank"`:

      @a[ #:href
      "https://docs.totalinsightmanagement.com/Conscript_Tutorial.html" #:target
      "_blank" ]{Open conscript tutorial in new tab}}) }|


@subsection{How to display monetary amounts}

To display monetary amounts, first @racket[require] the module @racket[conscript/survey-tools] which provides @racket[~$] for dollars, @racket[~euro] for euros, or @racket[~pound] for pounds:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(require conscript/survey-tools)

(define (payments)
  (define bonus 4.25)
  @md{# Bonus

      Your bonus is @(~$ bonus).
  })
}|
