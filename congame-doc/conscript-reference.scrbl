#lang scribble/manual

@(require [for-label racket/contract
                     conscript/base
                     conscript/markdown]
          "doc-util.rkt")

@title[#:style 'toc]{Conscript Reference}

Here you can lookup individual Conscript functions and macros to thoroughly
understand their usage.

@table-of-contents[]

@;===============================================

@section{Core}

@defmodule[conscript/base]

@defform[(defstep ...)]{
    @tktk{Defines a step.}
}

@defform[(defstep/study ...)]{
    @tktk{...}
}

@deftogether[(

@defform[(defvar ...)]

@defform[(defvar* ...)])]{

@tktk{Define a variable with some extra magic behind it. @racket[defvar] and @racket[defvar*]are both
particiant scope; @racket[defvar*] scope across parent/child studies. ...}

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

@defform[(for/study ...)]{

@tktk{See loops, definition of}

}

@defform[(with-bot)]{

@tktk{Johnny No. 5}

}

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Local Testing}

@; @defmodulelang[conscript/local]

@; @defproc[(preview [study study?]) void?]{

@; @tktk{Run @racket[_study] in your browser.}

@; }

@;===============================================

@section{Page Content}

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Markdown}

@defmodule[conscript/markdown]

@defform[(md ...)]{

@tktk{Parses its contents as Markdown and produces equivalent HTML content.}

}

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{HTML}

@defmodule[conscript/html]

@tktk{...}

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Forms}

@defmodule[conscript/form]

@tktk{...}

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Resources}

@defmodule[conscript/resource]

@tktk{...}

@; ==============================================

@section{Survey Tools}

@defmodule[conscript/survey-tools]

@deftogether[(@defproc[(~$ [n rational?]) string?]
              @defproc[(~euro [n rational?]) string?]
              @defproc[(~pound [n rational?]) string?])]{

Functions

}


