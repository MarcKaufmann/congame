#lang scribble/manual

@(require "doc-util.rkt")

@title[#:tag "install-congame"]{Installing Congame and Conscript}

Below are step-by-step instructions for installing Congame and Conscript on your personal computer.

Conscript is not a distinct application. Rather, it’s a set of software packages that run on top of
the Racket programming language environment.

@;===============================================

@section{Prerequisites}

@margin-note{Congame/Conscript have not yet been tested on Windows.}

You’ll need a computer running Linux, Mac OS, @mark{or Windows}.

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Install Racket}

@link["http://download.racket-lang.org/"]{Download and install Racket} for your OS. This
will also install @other-doc['(lib "scribblings/drracket/drracket.scrbl")].

Once Racket is installed, update your @envvar{PATH} environment variable to include the directories
that contain Racket executables. That way, when you issue common Racket commands, your shell will be
able to find those executables.

@itemlist[

@item{The simplest way to do this: open DrRacket, then click the @onscreen{Help} menu →
@onscreen{Configure Command Line for Racket}.}

@item{Or, if you are using Linux or Mac OS, you may prefer to edit the Bash/ZSH profile config file
in your home directory.}

]

To check that Racket and your @envvar{PATH} are set up properly, open a terminal and type this
command:

@terminal{
    @:>{racket -v}
    @banner[]
}

If everything went according to design, you should see the output as shown above.

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Install Docker}

To test your studies locally, you’ll need Docker installed. Use the link for your OS below:

@itemlist[

@item{@link["https://docs.docker.com/desktop/setup/install/mac-install/"]{Install Docker Desktop on
Mac}}

@item{@link["https://docs.docker.com/desktop/setup/install/linux/"]{Install Docker Desktop on
Linux}}

@item{@link["https://docs.docker.com/desktop/setup/install/windows-install/"]{Install Docker Desktop
on Windows}}

]

Follow the instructions linked above to install and run Docker. On first run, the app may prompt you
to create a Docker “account”, but you can safely skip this step if you wish.

@;===============================================

@section[#:tag "download"]{Download Congame and Conscript}

Download a copy of the Congame/Conscript source code:

@itemlist[

@item{You can get the source code as a
@link["https://github.com/MarcKaufmann/congame/archive/refs/heads/master.zip"]{ZIP file} (that’s a
direct download link for the most current version) and extract its contents to a suitable location.}

@item{@emph{Or}, if you’re comfortable with Git, you can clone
@link["https://github.com/MarcKaufmann/congame"]{the GitHub repository} — again, place the files
somewhere permanent and easy to find.}

]

@;===============================================

@section{Install the packages}

Now the Congame/Conscript packages need to be installed into Racket.

In a terminal, @tt{cd} into the main @filepath{congame} folder and run this command:

@margin-note{Note the trailing slashes! These tell @exec{raco pkg} to use these local folders as the
package source, rather than looking for the packages on the main package catalog.}

@terminal{
    @:>{raco pkg install congame-core/ congame-web/ congame-cli/ conscript/ congame-doc/}
}

If prompted @tt{Would you like to install these dependencies? [Y/n/a/c/?]}, answer @litchar{a} to
install @bold{a}ll required dependencies.

@;===============================================

@section{Build the Docker container}

The Congame Docker container is a comparatively simple way to run an entire Congame server on your
computer, so you can test your studies before uploading them to a “real” server accessible by other
participants.

First, make sure the Docker desktop app is installed @emph{and running}.

@margin-note{The @exec{docker compose} terminal commands won’t work unless the Docker app is
running.}

@bold{Make sure you’re on a strong, stable internet connection.}

In a terminal, run this command within the main @filepath{congame} folder:

@terminal{
    @:>{docker compose up}
}

@bold{This command will take 10–15 minutes to complete.} This is still quite a bit faster than it
would take most people to install, configure and run a web server and a database server, prepare the
databases, and configure the web application. Automation is a beautiful thing.

When the command completes, a Congame server will be running on @tt{localhost:5100}.  You can stop
it by typing @kbd{CTRL}@kbd{C} in the same terminal window, or by pressing the stop ⏹️ button for the
@tt{congame} container in the Docker desktop app.

@;===============================================

@section{Normal use: starting, stopping, and logging in}

From this point, when you need to test your studies locally, simply open the Docker desktop app and
press the play ▶️  button for the @tt{congame} container. This will start the Congame server, at which
point you should be able to browse to @url{http://localhost:5100} in your browser to see the Congame
home page.

@margin-note{If you prefer the command line, you can enter @exec{docker compose up} from the main
@filepath{congame} folder to start the Congame server, and press @kbd{CTRL}@kbd{C} in the same
terminal window to stop it again. You’ll still need the Docker desktop app runnning in the
background, however.}

To log in, use the built-in user @tt{admin@"@"congame.local} with password @tt{admin}.

When finished, you can stop the Congame server by pressing the stop ⏹️ button for the @tt{congame}
container in the Docker desktop app.

@;===============================================

@section{Updating the software}

To update to a new version of Congame and Conscript:

@itemlist[

@item{Ensure your local Congame container isn’t running.}

@item{Update your local copy of the source code:

@itemlist[

@item{If you downloaded by cloning the GitHub repository: do a @exec{git pull} from within the main
@filepath{congame} folder.}

@item{If you downloaded Congame as a ZIP file: download a fresh copy using the download link in
@secref["download"]. @mark{Delete the old @filepath{congame} folder and replace
it with the extracted contents of the new ZIP file.}}

]}

@item{Do @exec{raco setup congame conscript} to compile the new versions of the packages.}

@item{@mark{Rebuild the Docker container}: start the Docker app on your computer. Then in the
terminal, run these commands:

@terminal{
    @:>{docker compose pull}
    @:>{docker compose up --force-recreate --build}
}

Like the original installation, this will take a while and leave Congame in a running state.
Optionally run @exec{docker image prune -f} as well to clear out images no longer used by the
container.}

]
