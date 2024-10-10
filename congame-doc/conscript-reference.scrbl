#lang scribble/manual

@(require (for-label racket/contract
                     conscript/base))

@title[#:style 'toc]{Conscript Reference}

Here you can lookup individual Conscript functions and macros to thoroughly
understand their usage.

@section{Core}

@defmodule[conscript/base]

@defform[(defstep ...)]{
    Defines a step
}

@defform[(defstep/study ...)]{
    ...
}

@deftogether[(
@defform[(defvar ...)]
@defform[(defvar* ...)])]{
    Define a variable with some extra magic behind it
}

@defform[(defview ...)]{
    ...
}

@defform[(defstudy ...)]{
    ...
}

@defform[(--> ...)]{
    Mysterious arrow
}

@defform[(goto ...)]{
    Considered harmful
}

@defform[(for/study ...)]{
    See loops, definition of
}

@defform[(with-bot)]{
    Johnny No. 5
}

@subsection{Local Testing}

@defmodulelang[conscript/local]

@defproc[(preview [study study?]) void?]{

Run @racket[_study] in your browser.

}

@section{Page Content}

@subsection{Markdown}

@defmodule[conscript/markdown]

@defform[(md ...)]{

Parses its contents as Markdown and produces equivalent HTML content.

}

@subsection{HTML}

@defmodule[conscript/html]

@subsection{Forms}

@defmodule[conscript/form]

@subsection{Resources}

@defmodule[conscript/resource]

