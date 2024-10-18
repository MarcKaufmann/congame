#lang scribble/manual

@(require (for-label racket/contract
                     conscript/base)
          "doc-util.rkt")

@title[#:style 'toc]{Conscript Reference}

Here you can lookup individual Conscript functions and macros to thoroughly
understand their usage.

@table-of-contents[]

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
    @tktk{Define a variable with some extra magic behind it}
}

@defform[(defview ...)]{
    @tktk{...}
}

@defform[(defstudy ...)]{
    @tktk{...}
}

@defform[(for/study ...)]{
    @tktk{See loops, definition of}
}

@defform[(with-bot)]{
    Johnny No. 5
}

@subsection{Local Testing}

@defmodulelang[conscript/local]

@defproc[(preview [study study?]) void?]{

@tktk{Run @racket[_study] in your browser.}

}

@section{Page Content}

@subsection{Markdown}

@defmodule[conscript/markdown]

@defform[(md ...)]{

@tktk{Parses its contents as Markdown and produces equivalent HTML content.}

}

@subsection{HTML}

@defmodule[conscript/html]

@tktk{...}

@subsection{Forms}

@defmodule[conscript/form]

@tktk{...}

@subsection{Resources}

@defmodule[conscript/resource]

@tktk{...}
