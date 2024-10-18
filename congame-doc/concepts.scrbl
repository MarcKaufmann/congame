#lang scribble/manual

@(require [for-label racket/base]
          "doc-util.rkt"
          scribble/bnf)

@title{Overview}

What follows is a high-level overview of Congame's systems and how they all work together. If you've
just finished the @seclink["intro"]{introductory tour} then this will give you some more context for
what you saw.

@section{Programming in Racket}

The Congame system is built entirely on the Racket programming language, and the Congame studies 
you will create are essentially Racket programs.

If you only read these docs, you should be able to pick up enough Racket to write studies. But
Congame gives you access to lots of Racket capabilities. If you take a few minutes to learn about
Racket programming in general, you will have a better understanding of what’s happening in your
study’s code and how to fix it when it isn’t working.

Here are some great resources for someone new to Racket:

@itemlist[

@item{@secref["intro" #:doc '(lib "scribblings/guide/guide.scrbl")] and @secref["to-scheme" #:doc
'(lib "scribblings/guide/guide.scrbl")], the first two sections of @other-doc['(lib
"scribblings/guide/guide.scrbl")].}

@item{@other-doc['(lib "scribblings/quick/quick.scrbl")]}

@item{For a deep dive that will give you a really solid foundation, see 
@link["https://htdp.org/2024-8-20/Book/index.html"]{@italic{How to Design Programs}, 2nd ed.}}

]

If you are already even a little handy with other programming languages, you may want to bookmark
the @secref["data" #:doc '(lib "scribblings/reference/reference.scrbl")] chapter of @italic{The
Racket Reference}, to build up your vocabulary of the most common functions.


@section{Basic Congame Concepts}

A @deftech{step} is a point within a @tech{study} at which we provide information to
a participant, and/or collect a response from them. In the simplest (and most common) 
case, “steps” in Congame correspond to individual web pages.

A @deftech{study} is a series of @tech{steps} and a transition graph that controls how
study participants proceed through those steps.

A @deftech{study instance} is a discrete time when a @tech{study} is run on a particular
server. Congame keeps separate sets of results for each study instance.

A @deftech{replication} is a duplicate of a @tech{study instance}, created specifically to test
whether a study instance's results can be replicated — or, to run the study with new participants,
or simply to keep a copy of the data.

@section{Building Studies}

Two ways to author studies, depending on whether you're running your own Congame server.

@section{Web Pages}

Study steps eventually become web pages, so you may need to know a little about HTML and Markdown.

@section[#:tag "scribble-in-conscript"]{“Scribble Syntax” in Conscript}

The @emph{default} Racket syntax is based on parenthetical
@link["https://en.wikipedia.org/wiki/S-expression"]{S-expressions} and prefix notation: so instead
of, for example, @tt{2 + 2}, in Racket you would write @racket[(+ 2 2)]. But there are other types
of syntax available.

@margin-note{Racket’s advanced documentation system is called Scribble. The document you’re reading
now was written as a Scribble program, using the syntax described here.}

The @secref["Conscript"] environment in particular allows use of an alternative Racket syntax known
as @italic{Scribble syntax} (or @at syntax). This is an alternative way to write expressions in
situations where most of the code consists of strings of text, or where you want to intermingle code
with text (such as in documentation).

@racketblock[
 @#,BNF-seq[@litchar[@at] @nonterm{command name} @litchar{[} @nonterm{Racket arguments ...} @litchar{]} @litchar["{"] @nonterm{text body ...} @litchar["}"]]
@; @#,BNF-seq[@litchar[@at]
@;            @litchar{(} @nonterm{Racket expression} @litchar{)}]
]

A Scribble-style expression has the three possible parts after the @|_at|:

@itemlist[
@item{The @italic{command name} appears immediately after the @|_at|. Typically it’s an
identifier --- a short word --- bound to a Racket function or macro.}

@item{The @italic{Racket arguments} appear between square brackets @litchar["["] and @litchar["]"].
These are arguments supplied to the function or macro named in the command part. These arguments are
entered using Racket conventions --- e.g., a string of text needs to be put in quotes as a
@code{"string of text"}.}

@item{The @italic{text body} appears between curly braces @litchar["{"] and @litchar["}"]. You can
put any ordinary text here, @emph{or more @|at|-expressions}. Unlike with the Racket arguments, you
don't put quotes around the text. Everything in this text body is @emph{also} supplied as an
argument to the function or macro in the command part.}

]

Each of the three parts is optional. You can also nest commands within each other. However:

@itemlist[

@item{You can never have spaces or line breaks between the three parts. (This is particularly
relevant for people familiar with C-style syntax. You can’t put the @litchar["{"] on a separate
line!)}

@item{Whatever parts you use must always appear in the order above.}

]


