#lang scribble/manual

@(require "doc-util.rkt")

@title[#:tag "install-congame" #:style 'quiet]{Installing, Running, and Updating}

Below are step-by-step instructions for installing Congame and Conscript on your personal computer, so
you can develop and test studies.

@margin-note{These instructions don’t cover the steps needed to run your studies on a server so others
can participate; for that, see @secref["congame-setup"].}

Conscript is not a distinct application. Rather, it’s a set of software packages that run on top of
the Racket programming language environment.

@;===============================================

@section{Prerequisites}

@(define git-crlf
   "https://github.com/MarcKaufmann/congame/issues/168#issue-2706078344")

@margin-note{Congame/Conscript have not been thoroughly tested on
Windows. Windows users should @hyperlink[git-crlf]{turn on auto CRLF
conversion for Git}.}

You’ll need a computer running Linux, Mac OS, or Windows.

Ensure Git is installed.

@define[git-lfs]{https://docs.github.com/en/repositories/working-with-files/managing-large-files/installing-git-large-file-storage}

Install Git Large File Storage (Git LFS): follow @hyperlink[git-lfs]{these installation
instructions} for your operating system.

@margin-note{Git LFS (Large File Storage) is an extension for Git that manages large files
efficiently by storing them separately and replacing them with lightweight pointers in the
repository. Congame uses Git LFS to manage MP3 and PNG files included as part of certain example
studies.}

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

Using your Git client, clone @link["https://github.com/MarcKaufmann/congame"]{the GitHub repository}
to your computer — somewhere permanent and easy to find.

Using the CLI, you would do this:

@terminal{
    @:>{git clone https://github.com/MarcKaufmann/congame}
}

@margin-note{If you get an error at this point, ensure you have @hyperlink[git-lfs]{Git LFS}
installed (see @secref{Prerequisites} above).}

This will place the files in a subfolder of the current directory, named @filepath{congame}.

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
@filepath{congame} folder to start the Congame server, and (when finished) press @kbd{CTRL}@kbd{C}
in the same terminal window to stop it. You’ll still need the Docker desktop app runnning in the
background, however.}

When finished, you can stop the Congame server by pressing the stop ⏹️ button for the @tt{congame}
container in the Docker desktop app.

@;------------------------------------------------

@subsection{Logging in}

To log in as the server admin, use the built-in user @tt{admin@"@"congame.local} with password
@tt{admin}.

@subsubsection{Logging in anonymously}

Sometimes you need to log in as an anonymous participant in order to test a study (for example, a study
that coordinates results between participants).

Here’s how (assuming you've already uploaded a study and created at least one @tech{instance} for it):

@itemlist[#:style 'ordered

@item{Open a new @emph{incognito} browser window (or use a different browser than the one you used to
log in as admin).

@itemlist[#:style 'compact
@item{@bold{Chrome}: Click the menu icon (three dots) → Select @onscreen{New incognito window}}
@item{@bold{Firefox}: Click the menu icon (three lines) → Select @onscreen{New Private Window}}
@item{@bold{Safari}: Click @onscreen{File} menu → @onscreen{New Private Window}}
@item{@bold{Edge}: Click the menu icon (three dots) → @onscreen{New InPrivate window}}

]

}

@item{In the new browser window, open @racketresultfont{http://localhost:5100/_anon-login/[STUDY_INSTANCE]} —
replacing the last part with the instance (or “slug”) of your study.

@itemlist[#:style 'compact
@item{You can find your study’s “slug” on the server by navigating to @onscreen{Admin} → click your
study → click your study instance. The top of the study instance page will show the study
@onscreen{Slug}.}]}

]


@;===============================================

@section{Updating the software}

To update to a new version of Congame and Conscript:

@itemlist[#:style 'ordered

@item{Open a terminal window and go to the main @filepath{congame} folder.}

@item{Stop and remove the existing container by running this command:

@terminal{
    @:>{docker compose down}
}

@inline-note{Simply stopping the container (e.g. with the stop ⏹️ button in Docker Desktop) is not
enough. You must remove the old container so Docker can create a new one with your updates.}

}

@item{Update your local copy of the source code:

@terminal{
    @:>{git pull}
    @:>{raco pkg update congame-core/ congame-web/ congame-cli/ conscript/ congame-doc/}
}

}

@item{Ensure the @emph{Docker desktop app is open} and running on your computer, and run these
additional commands in the terminal:

@terminal{
    @:>{docker compose pull}
    @:>{docker compose build}
}

This will take much less time than the initial Docker setup.}

@item{Recreate the container:

@terminal{
    @:>{docker compose up --no-start}
}

}

]
