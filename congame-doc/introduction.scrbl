#lang scribble/manual

@(require "doc-util.rkt" 
        [for-label conscript/base
                   conscript/markdown])

@title[#:tag "intro"]{Introduction: a quick tour using Conscript}

The best way to learn is by doing, so let’s do!

Assuming you've installed Racket, launch DrRacket. Start a new file. Click into the top/main area
(the "definitions" window) and change the top line to:

@codeblock{
#lang conscript/local
}

The first line of every Conscript @tech{study} starts with @code{#lang conscript} or
@code{#lang conscript/local}. The latter allows us to test drive our studies in the 
web browser, without setting up a server or databases. When you’re ready to start
using it "for real", you change the first line to @code{#lang conscript} and then
upload it to a Congame server. 

@section{The simplest study}

Add some lines to your new program, so it looks like this:

@codeblock|{
#lang conscript/local

(defstep (start)
  @md{
    # The Beginning is the End

    This is all there is.
  })

(defstudy tutorial
  [start --> start])
}|

This code defines a @tech{step} named @racket[start], and a @tech{study} named @racket[tutorial],
which starts with a single step and ends with a single step.

We'll get into the specifics later. To try out this study, click DrRacket's @onscreen{Run} button.

You should see some text like the below in the bottom pane of DrRacket's main window (called the 
"interactions" window):

@terminal{
@dr-message{@banner[]}
@dr-message{Language: conscript/local, with debugging; memory limit: 512 MB.}
@:>{ }
}

This is where you can enter code one line at a time to interact with your program.

Click on this pane and type @code{(preview tutorial)} at the prompt, so that the interactions window
looks like this:

@terminal{
@dr-message{@banner[]}
@dr-message{Language: conscript/local, with debugging; memory limit: 512 MB.}
@:>{(preview tutorial)}
}

Then press your @kbd{ENTER} key. You should see your web browser open with a page that looks
roughly like this:

@browser{
    @bold{@larger{The Beginning is the End}}

    This is all there is.
}