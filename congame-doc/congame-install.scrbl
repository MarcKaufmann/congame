#lang scribble/manual

@(require "doc-util.rkt")

@title[#:tag "congame-setup" #:style 'quiet]{Installing and Setting up Congame servers}

Below are step-by-step instructions for installing Congame.

@section{Congame Prerequisites}

@subsection{Preparing PostgreSQL databases}

@section{Installing Congame Packages}

@section{Environment setup}

Copy @filepath{.env.default} to @filepath{.env}. Chief will automatically load the
variables defined in this file into the environment of the
subprocesses defined in the @filepath{Procfile} whenever it is run.

The app expects to be run behind an SSL terminated connection (for
example, behind an nginx instance using a self-signed cert), even for
local development. You can disable this requirement by setting the
@tt{CONGAME_DEBUG} environment variable to @tt{x}.

@section{Starting the server}

@terminal{
    @:>{nvm use}
    @:>{raco chief start}
}

By default the app will listen on @tt{localhost:5100}.

@section{Adding an admin user}

@section{Installing study packages}

@section{Clearing study caches}
