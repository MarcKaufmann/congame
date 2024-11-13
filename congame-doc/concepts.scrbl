#lang scribble/manual

@(require [for-label conscript/base
                     conscript/markdown]
          "atexp-example.rkt"
          "doc-util.rkt"
          scribble/bnf)

@title{Overview}

What follows is a high-level overview of Congame's systems and how they all work together. If you've
just finished the @seclink["intro"]{introductory tour} then this will give you some more context for
what you saw.

@;===============================================

@section{Help and Support}

If you have questions about Congame or about this documentation, create a post in 
@link["https://github.com/MarcKaufmann/congame/discussions"]{the Discussions area of Congame’s
GitHub repository}, and the authors will respond.

@section[#:tag "congame-racket"]{Programming in Racket}

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

@; ==============================================

@section{Basic Congame Concepts}

A @deftech{step} is a point within a @tech{study} at which we provide information to
a participant, and/or collect a response from them. In the simplest (and most common) 
case, “steps” in Congame correspond to individual web pages.

A @deftech{study} is a series of @tech{steps} and a transition graph that controls how
study participants proceed through those steps.

A study @deftech{instance} is a discrete time when a @tech{study} is run on a particular
server. Congame keeps separate sets of results for each study instance.

A @deftech{replication} is a duplicate of a study @tech{instance}, created specifically to test
whether a study instance's results can be replicated — or, to run the study with new participants,
or simply to keep a copy of the data.

@; ==============================================

@section{Web Pages}

@tech{Studies} and study @tech{steps} eventually become web pages, so it helps to know a little
about HTML and Markdown. This section provides a short introduction to each.

@deftech{HTML} (Hypertext Markup Language) is the standard markup language used to create web pages.
It provides the structure and content that the web browser renders to display a website. HTML
consists of @deftech{elements} which are represented by @deftech{tags} (keywords enclosed in
@litchar{<} and @litchar{>}) which surround the content of each element.

Below is a fragment from an HTML document:

@filebox["example.html"]{
@verbatim{
    <h1>Gary Benchley, Rock Star</h1>

    <p>Before I moved to <a href="https://en.wikipedia.org/wiki/New_York_City">New York</a>
    from Albany, I wrote a careful, step-by-step plan:</p>

    <ol>
      <li>Rock out.</li>
      <li>No more data entry.</li>
    </ol>
}}

The @tt{h1} (level 1 heading), @tt{p} (paragraph), @tt{ol} (ordered list) and @tt{li} (list item)
elements are standard HTML elements. When your web browser loads this file, it applies appropriate
styling to each element:

@browser{
    @bold{@larger{Gary Benchley, Rock Star}}

    Before I moved to @link["https://en.wikipedia.org/wiki/New_York_City"]{New York}
    from Albany, I wrote out a careful, step-by-step plan:

    @itemlist[#:style 'ordered

    @item{Rock out.}

    @item{No more data entry.}

    ]
}

@deftech{Markdown} is a simple text format for producing @tech{HTML}-formatted text.

Here’s how we’d write the above fragment using Markdown:

@filebox["example.md"]{
@verbatim{
# Gary Benchley, Rock Star

Before I moved to [New York][1] from Albany, I wrote out a careful, 
step-by-step plan:

1. Rock out.
2. No more data entry.

[1]: https://en.wikipedia.org/wiki/New_York_City
}}

A Markdown processor would convert this directly into the same HTML shown in the first example
above. As you can see, this is much quicker to type than the HTML version, and it is very readable.

Conscript provides functions for generating specific HTML elements, as well as functions that
accept Markdown-formatted text and convert it to HTML for you.

Here are some good resources for learning more:

@itemlist[

@item{HTML: The @link["https://developer.mozilla.org/en-US/docs/Web/HTML"]{HTML documentation on MDN}
provides good tutorials and reference material for HTML.}

@item{Markdown: The Commonmark site has a handy @link["https://commonmark.org/help/"]{intro and quick
reference sheet} which also links to a 10-minute tutorial.}

]

@; ==============================================

@section[#:tag "scribble-in-conscript"]{“Scribble Syntax” in Conscript}

@margin-note{Racket’s advanced documentation system is called Scribble. The document you’re reading
now was written as a Scribble program, using the syntax described here. See
@secref["The_Scribble_Syntax_at_a_Glance" #:doc '(lib "scribblings/scribble/scribble.scrbl")] for
more information on this syntax. }

The @secref["Conscript"] environment allows use of an alternative Racket syntax known as
@italic{Scribble syntax} (or @secref["reader" #:doc '(lib "scribblings/scribble/scribble.scrbl")]).

Expressions in Scribble syntax are normal Racket expressions in disguise. They keep you from having
to use double quotes @litchar{"} around all your strings. This makes them handy in situations where
most of the code consists of strings of text, or where you want to intermingle code with text (such
as in documentation).

Here are some examples of expressions written in Scribble syntax, and their equivalent forms in
normal Racket:

@scribble-examples|==={
  @foo{blah blah blah}
  @foo{blah "blah"}
  @foo{blah @hum[8] blah}
  @foo[1 2]{3 4}
  @foo[1 2 3 4]
  @foo{blah blah
       yada yada}
}===|

Scribble-style expressions have this form:

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

@; ==============================================

@section{Markdown in Scribble syntax in Racket}

If you’re not already familiar with Racket/Scribble, HTML, or Markdown, then your head might be
spinning a little right now.

To try and tie it all together, let's look at some example Conscript code from @secref["intro"]:

@codeblock|{
#lang conscript

(provide tutorial)

(defstep (start)
  @md{
    # The Beginning is the End

    This is all there is.
  })

(defstudy tutorial
  [start --> start])
}|

Armed with the concepts explained in this chapter, you should be able to understand:

@itemlist[

@item{All of this is Racket code. (@secref["congame-racket"])}

@item{This code defines a @tech{step} called @racketidfont{start}, and then a @tech{study} that uses
that step. (@secref["Basic_Congame_Concepts"]).}

@item{The @racketidfont{start} step uses the @racket[md] function to define some content using
@tech{Markdown} formatting. This Markdown gets converted to @tech{HTML}, which is displayed in your
web browser when you run the study. (@secref["Web_Pages"])}

@item{The call to @racket[md] is written using Scribble-style syntax, purely for convenience, so
that the string content can be written without quote marks and newline escapes (@litchar{"} and
@litchar{\n}).}

]
