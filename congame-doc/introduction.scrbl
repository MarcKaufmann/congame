#lang scribble/manual

@(require "doc-util.rkt" 
        [for-label conscript/base
                   conscript/markdown])

@title[#:tag "intro"]{Introduction: a quick tour using Conscript}

The best way to learn is by doing, so let’s do!

@section{Creating a new file}

Assuming you've installed Racket, launch DrRacket. Start a new file. Click into the top/main area
(the "definitions" window) and change the top line to:

@codeblock{
#lang conscript/local
}

The first line of every Conscript @tech{study} program starts with @code{#lang conscript} or
@code{#lang conscript/local}. The latter allows us to test drive our studies in the 
web browser, without setting up a server or databases. When you’re ready to start
using it "for real", you change the first line to @code{#lang conscript} and then
upload it to a Congame server. 

@section{Writing the simplest study}

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

We'll get into the specifics later; but at a high level:

@itemlist[

@item{Conscript @tech{studies} are Racket programs that start @code{#lang conscript} or
@code{#lang conscript/local}.}

@item{In Conscript, you can use “@"@" notation” to intermingle code and text. This is explained
further in @secref["Scribble_Syntax"].}
           
@item{Each @tech{step} is contained in a @racket[defstep] expression. Within this expression
are more expressions that provide the content and functionality for that step. The step shown
here uses an @racket[md] expression to denote text that will be formatted using Markdown.}

@item{The steps are tied together into a transition graph using a @racket[defstudy] expression.}

]

@section{Previewing the study}

To try out this study, click DrRacket's @onscreen{Run} button.

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

The browser is showing you the first and only step of the study: a page with some formatted text.

@section{Getting input}

Of course, to be at all useful, a study must collect information. To do that, we need to
give participants a way to interact with our study.

Create a new file in DrRacket (click @onscreen{File} → @onscreen{New}, or @onscreen{File} → @onscreen{New Tab}
according to your preference). Add these lines at the top of your new, empty file:

@codeblock{
#lang conscript/local

(defvar first-name)
(defvar age)
}

Here, we’ve introduced two @racket[defvar] expressions. Each one defines a new variable bound to an
identifier, in this case @racket[_first-name] and @racket[_age].

The @racket[defvar] expression looks similar to “declare a variable” expressions in Python, JavaScript,
and other languages. Using @racket[defvar] tells Conscript that this variable is a key piece of
information we want to record from each participant.

Now let’s add steps to our new study. First, we’ll explain to participants what to expect:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript/local

(defstep (description)
  @md{
    # The study

    Welcome to our study. In this study, we will ask for

    * Your first name
    * Your age
    
    @button{Start Survey}
  })
}|

This looks similar to the step in our first example study --- some simple Markdown-formatted text ---
but contained inside the @racket[md] expression is a new @racket[button] expression. This is the first
bit of interactivity: giving the participant a way to proceed to the next step. By default,
@racket[button] generates a button that, when clicked, navigates to the next step in the study,
whatever that is.

Speaking of which, let’s write the next step! Add these lines to the end of your source file:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript/local

(defstep (age-name-survey)
  @md{
    # Survey
   
    @form{
      What is your first name? @(set! first-name (input-text))
                               
      What is your age in years? @(set! age (input-number))
      
      @submit-button
  }})
}|

Once again, we’re introducing new expressions inside the @racket[md] expression to provide functionality
as well as explanatory text.

Working from the inside out:

@itemlist[#:style 'ordered

@item{The @racket[(input-text)] and @racket[(input-number)] expressions (at the end of the fifth and
seventh lines in the code above) generate input boxes where
the participant can enter data. There are other expressions that can be used to insert the various
other form elements like checkboxes and dropdown lists.}
          
@item{The @racket[(set! first-name (input-text))] expression tells Conscript, “when this form is submitted, set the value in
the @racket[_first-name] variable to the whatever the user has entered in @racket[(input-text)]. }

@item{The @racket[submit-button] expression inserts the button the user can click to submit all the
information they have entered into the form elements.}

@item{All the interactive form elements are wrapped in a @racket[form] expression to keep them grouped together.}

]

Next, write the final step by adding these lines to the end of your file:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript/local

(defstep (thank-you)
  @md{
    # Good job, @first-name

    Thank you for participating in our survey despite being @number->string[age] years old.
  })
}|

We’re doing something else interesting here: we’re inserting our variables directly into the text in
the @racket[md] expression! We’ve told Conscript to record @racket[_first-name] and @racket[_age] in
the previous step of the study, but in addition to storing those values for later analysis, we can make
use of those values while the user is still participating. There are many ways we might want to do this;
in this case, we’re simply displaying the values back to the user.

@margin-note{Note that since we used @racket[input-number] to assign a value to @racket[_age], we must use
@racket[number->string] to convert that numeric value to text if we want to display it within
@racket[md].}

Finally, we’ll define the study as a whole by tying all the steps together in a transition graph:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript/local

(defstudy simple-survey
  [description --> age-name-survey --> thank-you]
  [thank-you --> thank-you])
}|

We’ve now told Conscript that our study is named @racket[_simple-survey], and that it consists of
three steps @racket[_description], @racket[_age-name-survey] and @racket[_thank-you] in that order.
Because every step must have a transition, even the last one, we add @racket[[thank-you --> thank-you]]
to tell Conscript that that step simply transitions to itself.

To try out this study, same steps as before: click DrRacket's @onscreen{Run} button, then
click near the @litchar{>} prompt on the lower pane and type @code{(preview simple-survey)} (note
the new name of the survey used in the @racket[defstudy] expression).

Then press your @kbd{ENTER} key. You should see your web browser open with a page that looks
roughly like this:

@browser{
    @bold{@larger{The study}}

    Welcome to our study. In this study, we will ask for

    @itemlist[
     @item{Your first name}
     @item{Your age}
    ]

    @kbd{Start Survey}
    
}