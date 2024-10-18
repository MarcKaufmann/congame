#lang scribble/manual

@(require "doc-util.rkt" 
        [for-label conscript/base
                   conscript/markdown])

@title[#:tag "intro"]{Introduction: a quick tour using Conscript}

You can follow the steps in this tour to get a fast, high-level introduction to Congame and
Conscript:

@itemlist[
          
@item{Creating and testing a minimal study}

@item{Creating studies with multiple steps}

@item{Interactively collecting data from participants}

@item{Uploading and running your study on a Congame server.}

]

For now, we’ll focus on @emph{steps of action} that introduce the complete Congame development
cycle. When you’re ready, you can start reading the @secref{Conscript} and
@secref["The_Congame_Server"] sections for detailed explanations of the concepts introduced here.

@;===============================================

The best way to learn is by doing, so let’s do!

@;===============================================

@subsection{Creating a new file}

Assuming you've  @seclink["Installing_Conscript_and_Racket"]{installed Racket and Conscript}, launch
the DrRacket application. Start a new file. Click into the top/main area (the "definitions" window)
and change the top line to:

@codeblock{
#lang conscript/local
}

The first line of every Conscript @tech{study} program starts with @code{#lang conscript} or
@code{#lang conscript/local}. The latter allows us to test drive our studies in the web browser,
without setting up a server or databases. When you’re ready to start using it "for real", you change
the first line to @code{#lang conscript} and then upload it to a Congame server.

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Writing the simplest study code}

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

@item{Conscript @tech{studies} are Racket programs that start @code{#lang conscript} or @code{#lang
conscript/local}.}

@item{In Conscript, you can use “@"@" notation” to intermingle code and text. This is explained
further in @secref["Scribble_Syntax"].}
           
@item{Each @tech{step} is contained in a @racket[defstep] expression. Within this expression are
more expressions that provide the content and functionality for that step. The step shown here uses
an @racket[md] expression to denote text that will be formatted using Markdown.}

@item{The steps are tied together into a transition graph using a @racket[defstudy] expression.}

]

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Previewing the study}

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

Then press your @kbd{ENTER} key. You should see your web browser open with a page that looks roughly
like this:

@browser{
    @bold{@larger{The Beginning is the End}}

    This is all there is.
}

The browser is showing you the first and only step of the study: a page with some formatted text.

@;===============================================

@section{Second example: addings steps and getting input}

Of course, to be at all useful, a study must collect information. To do that, we need to give
participants a way to interact with our study.

Create a new file in DrRacket (click @onscreen{File} → @onscreen{New}, or @onscreen{File} →
@onscreen{New Tab} according to your preference). Add these lines at the top of your new, empty
file:

@codeblock{
#lang conscript/local

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

This looks similar to the step in our first example study --- some simple Markdown-formatted text
--- but contained inside the @racket[md] expression is a new @racket[button] expression. This is the
first bit of interactivity: giving the participant a way to proceed to the next step. By default,
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

Once again, we’re introducing new expressions inside the @racket[md] expression to provide
functionality as well as explanatory text.

Working from the inside out:

@itemlist[#:style 'ordered

@item{The @racket[(input-text)] and @racket[(input-number)] expressions (at the end of the fifth and
seventh lines in the code above) generate input boxes where the participant can enter data. There
are other expressions that can be used to insert the various other form elements like checkboxes and
dropdown lists.}
          
@item{The @racket[(set! first-name (input-text))] expression tells Conscript, “when this form is
submitted, set the value in the @racket[_first-name] variable to the whatever the user has entered
in @racket[(input-text)]. }

@item{The @racket[submit-button] expression inserts the button the user can click to submit all the
information they have entered into the form elements.}

@item{All the interactive form elements are wrapped in a @racket[form] expression to keep them
grouped together.}

]

Next, write the final step by adding these lines to the end of your file:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript/local

(defstep (thank-you)
  @md{
    # Good job, @first-name

    Thank you for participating in our survey despite
    being @number->string[age] years old.
  })
}|

We’re doing something else interesting here: we’re inserting our variables directly into the text in
the @racket[md] expression! We’ve told Conscript to record @racket[_first-name] and @racket[_age] in
the previous step of the study, but in addition to storing those values for later analysis, we can
make use of those values while the user is still participating. There are many ways we might want to
do this; in this case, we’re simply displaying the values back to the user.

@margin-note{Note that since we used @racket[input-number] to assign a value to @racket[_age], we
must use @racket[number->string] to convert that numeric value to text if we want to display it
within @racket[md].}

Finally, we’ll define the study as a whole by tying all the steps together in a transition graph:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript/local

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
#lang conscript/local

(provide simple-survey)

(defvar first-name) ; ...
}

This line will be needed later when we upload our study to a Congame server. Without it, the server
will not be able to access the study bound to the @racket[simple-study] identifier. 

@margin-note{It’s a good practice to add @racket[(provide _studyname ...)] at the top of your file,
where you include each @racket[_studyname] defined in a @racket[defstudy] expression in your
program.}

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Review: the code so far}

Combining all these snippets, the code should look like the below example. Go ahead and save it as
@filepath{age-survey.rkt}.

@filebox["age-survey.rkt"]{
@codeblock|{
#lang conscript/local

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

@;===============================================

@subsection{Previewing the second study}

To try out this study, same steps as before: click DrRacket's @onscreen{Run} button, then click near
the @litchar{>} prompt on the lower pane and type @code{(preview simple-survey)} (note the new name
of the survey used in the @racket[defstudy] expression).

Then press your @kbd{ENTER} key. You should see your web browser open with a page that looks roughly
like this:

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
@racketid[-->] to tell Congame how the steps connect to each other in sequences.}

]

It’s possible to create much more advanced studies than those we have built so far: studies with
conditional branching, a variety of form controls, timers, randomizing assignments, etc. But what
you have seen so far gives you the basic framework.

@;===============================================

@section{Uploading and running your study}

When you have finished designing your study, it's time to go live and start collecting responses
from participants. We’re going to do that now with the @filepath{age-survey.rkt} study we just
created.

In order to do this, you need access to a Congame server. For the steps that follow, we'll assume:

@itemlist[#:style 'ordered

@item{You have a link/address to a running Congame server.}

@item{You already have a researcher or an administrator account on that server.}

]

@margin-note{See @seclink["congame-setup"] if you need to know how to prepare a server for use with
studies.} 


@;{ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Preparing the study: from @racketmodname[conscript/local] to @racketmodname[conscript]}

@bold{Important:} before uploading the study to a Congame server, we need to change it to use 
@code{#lang conscript} instead of @code{#lang conscript/local}.

The @racketmodname[conscript/local] environment is only useful for local testing; it’s quick to use,
but it doesn’t permanently record any of the data collected, and cannot differentiate between
participants. Using @racketmodname[conscript] @mark{unlocks access to the full server’s participant
and responses database.}

Open the @filepath{age-survey.rkt} study created in the previous section, and edit the first line in
the file, removing @racketvalfont{/local} from the @hash-lang[] line so that it reads like this:

@codeblock{
    #lang conscript
}

@margin-note{If you forget this step and attempt to upload a study that uses @code{conscript/local},
you’ll get an error.}

Make sure you save the file!
}

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Upload the study}

Navigate to the Congame server in your web browser and log in with your username and password.

Click on the @onscreen{Admin} tab. (If you don’t see this, contact your server admin.)

Under the @italic{Studies} section, click @onscreen{New Study}, and fill out the upload form as
follows:

@itemlist[

@item{@bold{Name}: Enter @racketvalfont{Age Survey} (any human-friendly title will do).}

@item{@bold{Slug}: Leave this blank.}

@item{@bold{Type}: Set to @racketresultfont{Conscript}.}

@item{@bold{Study ID}: Enter @racketvalfont{simple-survey}. This is the identifier we used in the 
@racket[defstudy] and @racket[provide] expressions in our @filepath{age-survey.rkt} file.}

@item{@bold{Study Source}: Click the @kbd{Browse} button and locate/select the
@filepath{age-survey.rkt} file.}

]

@screenshot["intro-upload.png"]

When ready, click @kbd{Create} to upload and create the study.

@margin-note{If you receive errors at this point, double check that the contents of
@filepath{age-survey.rkt} exactly match the contents of the example file shown above. In particular,
check that the top line says @code{#lang conscript} and that the @racket[(provide simple-survey)]
expression is present in the file.}

Alternatively, if too much time has passed since you clicked the @onscreen{New Study} link, you may
receive a “session expired” error — simply fill out the form and try again more quickly this time.}

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Creating a study instance}

As soon as the upload is complete, you’ll be taken to a screen listing all the @tech{study
instances} for the newly-created study:

@screenshot["intro-empty-instancelist.png"]

Click on @onscreen{New Instance}. For now, just fill in a value like @racketvalfont{Instance 1}
in the @italic{Name} field, and leave the rest of the fields at their default values:

@screenshot["intro-new-instance.png"]

Click @kbd{Create} to create an instance of the study.

You’ll be taken back to the study’s instance list, and this time the new instance will appear in the
list.

@;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@subsection{Trying it out}

Now that you’ve published your study and created an instance for it, you can go click on the
@onscreen{Dashboard} link and see the new instance listed there. 

You’ll also see a link titled @onscreen{Enroll} — click it! You will be taken through the study
exactly as you were when you tried it out on your computer earlier. Go ahead and complete all the
steps.

@margin-note{As long as the study instance is in “active” status, other people who log into this same study as
participants will see this same study instance and the @onscreen{Enroll} link on their dashboard,
allowing them to complete the study as well.}

Once you’re done with the study, @mark{navigate back to the dashboard somehow}. Then click on the
@onscreen{Admin} → @onscreen{Instance 1} link.

All the way at the bottom, you’ll see a section titled @bold{Participants} — and you’ll be
the first one:

@screenshot["intro-participant.png"]

The number under the @emph{Participant ID} column is a link — go ahead and click on it. You’ll see
a detailed listing of your responses to the study:

@screenshot["intro-participant-responses.png"]

@;===============================================

@section{Wrapping it up}

At this point, you’ve seen all of the essential features of creating studies in Conscript, testing
them locally, and running them on a Congame server.

From here, you should check out the @secref["Overview"], and then, when you’re ready, dive into the
more detailed @secref["Conscript"] guide.
