#lang scribble/manual

@(require "doc-util.rkt"
        [for-label conscript/base
                   conscript/markdown])

@title[#:tag "intro" #:style 'quiet]{Introduction: a quick tour using Conscript}

You can follow the steps in this tour to get a fast, high-level introduction to Congame and
Conscript:

@itemlist[

@item{Creating and testing a minimal study}

@item{Creating studies with multiple steps}

@item{Interactively collecting data from participants}

@item{Uploading and running your study on a Congame server.}

]

This will give you a taste of how the system works. You’ll get a sequence of @emph{steps to take} to
try everything out, but not (yet) the reasoning behind them.  When you’re ready, you can start
reading the @secref{Conscript} and @secref["The_Congame_Server"] sections for detailed explanations
of the concepts introduced here.

@;===============================================

@section{First example: the simplest study}

The best way to learn is by doing, so let’s do!

To follow along on your computer, first follow the instructions in @secref["install-congame"].

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Creating a new file}

Assuming you've  @seclink["install-congame"]{installed Racket and Conscript}, launch
the DrRacket application. Start a new file. Click into the top/main area (the "definitions" window)
and change the top line to:

@codeblock{
#lang conscript
}

The first line of every Conscript @tech{study} program starts with @code{#lang conscript}.

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Writing the simplest study code}

Add some lines to your new program, so it looks like this:

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

This code defines a @tech{step} named @racket[start], and a @tech{study} named @racket[tutorial],
which starts with a single step and ends with a single step.

We'll get into the specifics later; but at a high level:

@itemlist[

@item{Conscript @tech{studies} are Racket programs that start @code{#lang conscript}.}

@item{In Conscript, you can use “@"@" notation” to intermingle code and text. This is explained
further in @secref["scribble-in-conscript"].}

@item{Each @tech{step} is contained in a @racket[defstep] expression. Within this expression are
more expressions that provide the content and functionality for that step. The step shown here uses
an @racket[md] expression to denote text that will be formatted using Markdown.}

@item{The steps are tied together into a transition graph using a @racket[defstudy] expression.}

]

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Test-driving the simplest study}

To try out your study, you’ll need to upload it to a Congame server and enroll in an @tech{instance}
of the study. The Docker container you set up during the initial
@seclink["install-congame"]{installation process} provides the Congame server for use in testing.

Open the Docker desktop app and press the play ▶️  button for the @tt{congame} container.

In DrRacket, save the study you created above as @filepath{simple.rkt}.

Then click the @onscreen{Upload Study} button:

@screenshot{intro-drr-upload-btn.png}

@subsubsection{Aside: one-time connection setup}

The very first time you upload a Conscript study from DrRacket, DrRacket will ask you the address of
the Congame server to connect to, and then prompt you to log in on that server.

DrRacket assumes the server you want is at @tt{http://localhost:5100} --- that is, is the one
running on the Docker container on your computer. No need to change it, so click the @onscreen{Log
in} button. Log in using the built-in user @tt{admin@"@"congame.local} with password @tt{admin}.

@margin-note{@mark{If you later want DrRacket to send uploads to a different server, too bad.}}

Once the login is complete, DrRacket will ask you the name of your study: 

@margin-note{DrRacket keeps a cache matching your @filepath{.rkt} filenames to study names. You
won’t have to re-enter the study name more than once for each file you upload --- until you close
and reopen DrRacket.}

@screenshot{intro-drr-studyname.png}

Enter @racketvalfont{tutorial} as shown, since that’s the name we gave our study in the
@racket[defstudy] expression.

At this point, DrRacket will upload your study. Next time you edit and upload this study, you’ll
skip past all these steps and the upload will happen as soon as you click the @onscreen{Upload
Study} button.

Aside over!

@subsubsection{Make an instance and enroll}

Once your study is uploaded, you should see your web browser open to a page titled
@onscreen{Instances of tutorial}.

Click on the @onscreen{New Instance} button. For now, just fill in a value like
@racketvalfont{Instance 1} in the @italic{Name} field, and leave the rest of the fields at their
default values:

@browser-screenshot["intro-new-instance.png"]

Click @kbd{Create} to create an instance of the study.

You’ll be taken back to the study’s instance list, and this time the new instance will appear in the
list.

Go back to the @onscreen{Dashboard} (link at top) and you’ll see the new instance listed there.

You’ll also see an @onscreen{Enroll} link --- click it, and you’ll be taken through the study as a
participant:

@browser{
     @bold{@larger{The Beginning is the End}}

     This is all there is.
}

That right there is your first study! The browser is showing you the first and only step of the
study: a page with some formatted text.

@;===============================================

@section{Second example: addings steps and getting input}

Of course, to be at all useful, a study must collect information. To do that, we need to give
participants a way to interact with our study.

Create a new file in DrRacket (click @onscreen{File} → @onscreen{New}, or @onscreen{File} →
@onscreen{New Tab} according to your preference). Add these lines at the top of your new, empty
file:

@codeblock{
#lang conscript

(defvar first-name)
(defvar age)
}

Here, we’ve introduced two @racket[defvar] expressions. Each one defines a new variable bound to an
identifier, in this case @racket[_first-name] and @racket[_age].

The @racket[defvar] expression looks similar to “declare a variable” expressions in Python,
JavaScript, and other languages. Using @racket[defvar] tells Conscript that this variable is a key
piece of information we want to record from each participant.

Now let’s add steps to our new study. First, we’ll explain to participants what to expect:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (description)
  @md{
    # The study

    Welcome to our study. In this study, we will ask for

    * Your first name
    * Your age

    @button{Start Survey}
  })
}|

This looks similar to the step in our first example study --- some simple Markdown-formatted text
--- but contained inside the @racket[md] expression is a new @racket[button] expression. This is the
first bit of interactivity: giving the participant a way to proceed to the next step. By default,
@racket[button] generates a button that, when clicked, navigates to the next step in the study,
whatever that is.

Speaking of which, let’s write the next step! Add these lines to the end of your source file:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (age-name-survey)
  @md{
    # Survey

    @form{
      What is your first name? @(set! first-name (input-text))

      What is your age in years? @(set! age (input-number))

      @submit-button
  }})
}|

Once again, we’re introducing new expressions inside the @racket[md] expression to provide
functionality as well as explanatory text.

Working from the inside out:

@itemlist[#:style 'ordered

@item{The @racket[(input-text)] and @racket[(input-number)] expressions (at the end of the sixth and
eighth lines in the code above) generate input boxes where the participant can enter data. There
are other expressions that can be used to insert the various other form elements like checkboxes and
dropdown lists.}

@item{The @racket[(set! first-name (input-text))] expression tells Conscript, “when this form is
submitted, set the value in the @racket[_first-name] variable to the whatever the user has entered
in @racket[(input-text)]. }

@item{The @racket[submit-button] expression inserts the button the user can click to submit all the
information they have entered into the form.}

@item{All the interactive form elements are wrapped in a @racket[form] expression to keep them
grouped together.}

]

Next, write the final step by adding these lines to the end of your file:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (thank-you)
  @md{
    # Good job, @first-name

    Thank you for participating in our survey despite
    being @number->string[age] years old.
  })
}|

We’re doing something else interesting here: we’re inserting our variables directly into the text!
We’ve told Conscript to record @racket[_first-name] and @racket[_age] in the previous step of the
study; in addition to storing those values for later analysis, we can make use of those values while
the user is still participating. There are many ways we might want to do this: in this case, we’re
simply displaying the values back to the user.

@margin-note{Note that since we used @racket[input-number] to assign a value to @racket[_age], we
must use @racket[number->string] to convert that numeric value to text if we want to display it
within @racket[md].}

Finally, we’ll define the study as a whole by tying all the steps together in a transition graph:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstudy simple-survey
  [description --> age-name-survey --> thank-you]
  [thank-you --> thank-you])
}|

We’ve now told Conscript that our study is named @racket[simple-survey], and that it consists of
three steps @racket[description], @racket[age-name-survey] and @racket[thank-you] in that order.
Because every step must have a transition, even the last one, we add @racket[[thank-you -->
thank-you]] to tell Conscript that that step simply transitions to itself.

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Being a good provider}

There’s one more thing you need to do: Add a statement at the top of your file, just below the
@code{#lang conscript} line, to @racket[provide] the study you just defined:

@codeblock{
#lang conscript

(provide simple-survey)

(defvar first-name) ; ...
}

@margin-note{It’s a good practice to add @racket[(provide _studyname ...)] at the top of your file,
where @racket[_studyname] is the identifier used in your @racket[defstudy] expression.}

This line will be needed later when we upload our study to a Congame server. Without it, the server
will not be able to access the study bound to the @racket[simple-study] identifier.

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Review: the code so far}

Combining all these snippets, the code should look like the below example. Go ahead and save it as
@filepath{age-survey.rkt}.

@filebox["age-survey.rkt"]{
@codeblock|{
#lang conscript

(provide simple-survey)

(defvar first-name)
(defvar age)

(defstep (description)
  @md{
    # The study

    Welcome to our study. In this study, we will ask for

    * Your first name
    * Your age

    @button{Start Survey}
  })

(defstep (age-name-survey)
  @md{
    # Survey

    @form{
      What is your first name? @(set! first-name (input-text))

      What is your age in years? @(set! age (input-number))

      @submit-button
  }})

(defstep (thank-you)
  @md{
    # Good job, @first-name

    Thank you for participating in our survey despite being
    @number->string[age] years old.
  })

(defstudy simple-survey
  [description --> age-name-survey --> thank-you]
  [thank-you --> thank-you])
}|}

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Test-driving the age study}

Once again, click the @onscreen{Upload Study} button in DrRacket, and (if prompted for the study
name) enter the name of this new study, @racketvalfont{simple-survey}.

@margin-note{If you receive errors at this point, double check that the contents of
@filepath{age-survey.rkt} exactly match the contents of the example file shown above. In particular,
check that the top line says @code{#lang conscript} and that the @racket[(provide simple-survey)]
expression is present in the file.}

If all goes well, you’ll be taken to a page titled @onscreen{Instances of simple-survey}:

@browser-screenshot{intro-empty-instancelist.png}

As before, click on @onscreen{New Instance} and enter a name for the survey instance --- say,
@racketvalfont{Age Instance 1}. 

Now that you’ve published your study and created an instance for it, you can click on the
@onscreen{Dashboard} link and see the new instance listed there.

You’ll also see a link titled @onscreen{Enroll} — click it! 

You should see your web browser open with a page that looks roughly like this:

@browser{
  @bold{@larger{The study}}

  Welcome to our study. In this study, we will ask for

  @itemlist[
   @item{Your first name}
   @item{Your age}
  ]

  @kbd{Start Survey}
}

Click the @onscreen{Start Survey} link/button to proceed to the next step in the survey:

@browser{
  @bold{@larger{Survey}}

  What is your first name? @mock-textbox[]

  What is your age in years?  @mock-textbox[]

  @kbd{Submit}
}

Enter some values in the textboxes and click “Submit”.

@margin-note{For fun, try entering a non-numeric value in the “age” text box. What happens when you
click @onscreen{Submit}?}

This will bring you to the final step in the study:

@browser{
  @bold{@larger{Good Job, Joel}}

  Thank you for participating in our survey despite being 11 years old.
}

That’s it! You can see that the values you entered in the previous step have carried over and are
being used in the text displayed in this step.

@margin-note{When you upload and run the study on the Congame server, values collected and stored in
variables created with @racket[defvar] and similar forms get recorded in the server’s database and
comprise the results of the study.}

You’ve now observed a few more basic concepts first-hand:

@itemlist[

@item{You use @racket[defvar] to define the discrete pieces of information you want to collect from
study participants.}

@item{You can create multiple @tech{steps} for your study through repeated use of @racket[defstep].}

@item{Within steps, you can insert input controls like @racket[input-text] inside a @racket[form] to
collect info from participants, and save that info into your @racket[defvar] variables.}

@item{You use the @racket[defstudy] form to define your study as a whole. Within that form, you use
@racket[-->] to tell Congame how the steps connect to each other in sequences.}

]

It’s possible to create much more advanced studies than those we have built so far: studies with
conditional branching, a variety of form controls, timers, randomizing assignments, etc. But what
you have seen so far gives you the basic framework.

@margin-note{As long as the study instance is in “active” status, other people who log into this same study as
participants will see this same study instance and the @onscreen{Enroll} link on their dashboard,
allowing them to complete the study as well.}

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Reviewing collected responses}

Finally, let’s take a look at what happens to the responses submitted by study participants.

Once you’re done with the study, navigate back to the dashboard. Then click on the @onscreen{Admin}
→ @onscreen{simple-survey} (in the list of studies), then on the @onscreen{Age Instance 1} link.

All the way at the bottom, you’ll see a section titled @bold{Participants} — and you’ll be
the first one:

@browser-screenshot["intro-participant.png"]

The number under the @emph{Participant ID} column is a link — go ahead and click on it. You’ll see
a detailed listing of your responses to the study:

@browser-screenshot["intro-participant-responses.png"]

@;===============================================

@section{Wrapping it up}

At this point, you’ve seen all of the essential features of creating studies in Conscript, testing
them on a Congame server, and collecting some sample responses.

From here, you should check out the @secref["Overview"], and then, when you’re ready, dive into the
more detailed @secref["Conscript"] guide.
