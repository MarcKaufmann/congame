#lang scribble/manual

@(require "doc-util.rkt")

@title{Installing Conscript and Racket}

Below are step-by-step instructions for installing Conscript.

Conscript is not a distinct application. Rather, it’s a software library that runs on top of the
Racket programming language environment. It includes two languages/environments: @code{#lang
conscript/local}, which provides syntax checks, and allows you to run and test studies in a web
browser on your local computer without setting up a complete server environment; and @code{#lang
conscript} which does not provide local testing in a web browser, but does provide syntax checks.
Neither flavor by itself enables you to create @tech{study instances} in which other people can
participate --- for that you need to upload your Conscript studies to a Congame server.

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


@item{Download a copy of the Congame/Conscript source code. You can get this as a
@link["https://github.com/MarcKaufmann/congame/archive/refs/heads/master.zip"]{ZIP file} (that’s a
direct download link for the most current version) and extract its contents to a suitable location.
@emph{Or}, if you’re comfortable with Git, you can clone
@link["https://github.com/MarcKaufmann/congame"]{the GitHub repository} — again, place the files
somewhere permanent and easy to find.}

]

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Package installation (GUI)}

You’ve installed Racket and you’ve downloaded the Congame/Conscript source code files: now you need
to install the Congame package into Racket so that it’s ready to use in the code you write.

If you’re not yet ready to run terminal commands, here is a simple way to do this using only the
DrRacket GUI:

@itemlist[#:style 'ordered

@item{Open DrRacket, and click the @onscreen{File} menu → @onscreen{Package Manager…}. On the
@onscreen{Do What I Mean} tab, click the @kbd{Browse} button. When DrRacket asks, “Choose a file or
a directory?”, pick @kbd{Directory}.}

@item{Browse to the Congame source you downloaded in step two above, and then browse to the
@filepath{congame-core} subfolder and click @kbd{Open}, then @kbd{Install}.}

@item{Do the same thing, but with the @filepath{conscript} subfolder.}

]

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Package installation (terminal)}

If you’re comfortable using terminal commands on your operating system, follow these steps to
install Congame into Racket.

@margin-note{If you do anything at all with Racket outside of Conscript, you should do this step of
configuring the @envvar{PATH}. This will ensure you have ready access to @other-doc['(lib
"scribblings/raco/raco.scrbl")], which are used for several common Racket development and
maintenance tasks.}

First, update your @envvar{PATH} environment variable to include the directories that contain Racket
executables. That way, when you issue common Racket commands, your shell will be able to find those
executables without you having to specify the complete path to them every time.

@itemlist[

@item{The simplest way to do this: open DrRacket, then click the @onscreen{Help} menu →
@onscreen{Configure Command Line for Racket}.}

@item{Or, if you are using Linux or Mac OS, you may prefer to edit the Bash/ZSH profile config file
in your home directory.}

]

To check if Racket’s set up properly on the command line, open a terminal and type this command:

@terminal{
    @:>{racket -v}
    @banner[]
}

You should see the output as shown above. If so, @tt{cd} into the main @filepath{congame} folder and 
run this command:

@terminal{
    @:>{raco pkg install congame-core/ conscript/}
}


