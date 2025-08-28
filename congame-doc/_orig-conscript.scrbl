#lang scribble/manual

@(require (for-label conscript racket/base))

@title{Conscript (Original)}

@section{Goals and Philosophy}

Conscript aims to provide a simpler and streamlined syntax for
writing studies compared to @tech{congame}. There are two dialects of
@racketmodfont{conscript}:

@itemize[
  @item{Plain @racketmodfont{conscript}, which cannot require Racket
    functions that we are not providing by default.}
  @item{@racketmodfont{conscript/with-require}, which allows the user to
    require.}
]

Since @racketmodfont{conscript/with-require} provides a mechanism to require
arbitrary Racket packages, it has essentially the same power as full
@tech{congame}, while @racketmodfont{conscript} can only require a limited set
of libraries and functionality of Racket. Due to these limitations, we
can allow such studies to be uploaded to a congame server by users with
``researcher'' accounts, and be run without requiring access to the
code repository. This is not possible (out of security concerns) with
@racketmodfont{conscript/with-require} studies.

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

