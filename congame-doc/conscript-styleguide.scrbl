#lang scribble/manual

@(require [for-label conscript/base]
          "doc-util.rkt"
          "howtos.rkt")

@title[#:style 'quiet]{Conscript Style Guide}

These are some best practices you should follow to prevent common errors, and to make your code more
consistent and readable.

@;===============================================

@section{Study @racket[provide]s}

Each @tech{study} defined with @racket[defstudy] should have a corresponding @racket[provide]
statement at the top of the file, before any other definitions.

@codeblock|{
#lang conscript

(provide my-study)

; var definitions...
(defvar ...)

; step definitions
(defstep intro ...)

; study definition
(defstudy my-study ...)
}|

Without the @racket[provide], you may not notice any problem during testing, but the Congame server
will not be able to access your study when you upload it.

Placing @racket[provide] statements at the top makes it clear, when reading the source code, exactly
what the file makes available to other modules.

@margin-note{For more about why @racket[provide] should go at the top of the file, see
@secref["Provide" #:doc '(lib "scribblings/style/style.scrbl")] in the Racket style guide.}

@;===============================================

@section{Indentation}

@compare0[
  @codeblock0[#:keep-lang-line? #f]|{
    #lang conscript
    @; don't indent top-level forms
    (defstep (my-step)
      @; within a form, ident using two spaces
      @html{
        @h1{Hello World}
      })
  }|
  @codeblock0[#:keep-lang-line? #f]|{
    #lang conscript

      (defstep (my-step)

      @html{
      @h1{Hello World}
      }
      )
  }|
]
