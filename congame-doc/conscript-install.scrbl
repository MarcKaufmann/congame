#lang scribble/manual

@(require "doc-util.rkt")

@title{Installing Conscript and Racket}

Below are step-by-step instructions for installing Conscript.

Conscript is not a distinct application. Rather, it’s a software library that runs on top of the
Racket programming language environment.

@margin-note{These instructions do not cover everything you’ll need to do to set
up a complete local Congame development server. For that, see @secref{congame-setup}.}

@;===============================================

@section{Prerequisites}

You’ll need a computer running Linux, Mac OS, @mark{or Windows}.


@;===============================================

@section{Conscript Installation Steps}

@itemlist[#:style 'ordered

@item{@link["http://download.racket-lang.org/"]{Download and install Racket} for your OS. This
will also install @other-doc['(lib "scribblings/drracket/drracket.scrbl")].}


@item{Download a copy of the Congame source code. You can get this as a
@link["https://github.com/MarcKaufmann/congame/archive/refs/heads/master.zip"]{ZIP file} (that’s a
direct download link for the most current version) and extract its contents to a suitable location.
@emph{Or}, if you’re comfortable with Git, you can clone
@link["https://github.com/MarcKaufmann/congame"]{the GitHub repository} — again, place the files
somewhere permanent and easy to find.}

@item{Open DrRacket, and click the @onscreen{File} menu → @onscreen{Package Manager…}. On the
@onscreen{Do What I Mean} tab, click the @kbd{Browse} button. When DrRacket asks, “Choose a file or
a directory?”, pick @kbd{Directory}.

@itemlist[

@item{Browse to the Congame source you downloaded in step two above,
and then browse to the @filepath{congame-core} subfolder and click @kbd{Open}, then @kbd{Install}.}

@item{Do the same thing, but with the @filepath{conscript} subfolder.}

]}]

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Terminal setup}

This next part isn’t strictly necessary, but will come in handy if you’re comfortable using terminal
commands on your operating system, or if you do anything at all with Racket outside of Conscript.

In that case, you should update your computer’s @envvar{PATH} environment variable so that it
includes the directories that contain Racket executables. That way, when you issue common Racket
commands, your shell will be able to find those executables without you having to specify the
complete path to them every time.

The simplest way to do this: open DrRacket, then click the @onscreen{Help} menu →
@onscreen{Configure Command Line for Racket}.

