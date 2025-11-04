#lang scribble/manual

@(require (for-label conscript/base
                     (except-in conscript/survey-tools make-sliders)
                     racket/contract
                     racket/match)
          "doc-util.rkt")

@title[#:style 'quiet #:tag "prisoners-dilemma"]{Tutorial: the Prisoner’s Dilemma}

The Prisoner’s Dilemma is a classic problem in game theory that demonstrates how two rational
individuals might not cooperate, even if it is in their best interest to do so. In its standard
form, two suspects are arrested and interrogated separately. If both remain silent (that is, if they
cooperate with each other), they each receive a light sentence. If one betrays the other (defects)
while the other stays silent, the defector goes free, and the silent prisoner gets the maximum
sentence. If both betray each other, they each receive a moderate sentence. The dilemma arises
because betrayal is the dominant strategy for both, leading to a worse collective outcome than
mutual cooperation.

In this tutorial, we’ll write a study that matches each participant with one other participant, and
pits the two against each other in a “Prisoner’s Dilemma”.

Until now, our tutorial studies have only recorded and considered the responses of individual
participants in isolation. But in the Prisoner’s Dilemma that’s not enough: each participant’s
response will need to be compared with that of one other participant.

In doing so, you’ll learn how to design studies that match people into groups, and that incorporate
responses of multiple people when calculating results.

@bold{To follow along:} open DrRacket on your computer and create a new file that starts with
@code{#lang conscript}.

@;===============================================

@section{Defining our variables}

As with all studies, we start by defining variables to keep track of responses and results of the
study.

@codeblock[#:keep-lang-line? #t]{
  #lang conscript

  (require conscript/survey-tools
           racket/match)

  (defvar my-choice)
  (defvar their-choice)
  (defvar prison-sentence)
}

We'll use @racket[my-choice] to record the current participant's choice, @racket[their-choice] to
store the choice made by the person they were paired with, and @racket[prison-sentence] to record
the final result (the length of the prison sentence they end up with).

Because all three variables are declared with @racket[defvar], they will have @tech{participant
scope} — that is, each participant will have their own separate values for these variables.

We also @racket[require] two modules we'll need later: @racketmodname[conscript/survey-tools]
provides @racket[refresh-every], and @racketmodname[racket/match] provides the @racket[match*] form
we'll use to determine the outcome based on both players' choices.

@;===============================================

@section{The introduction step}

The first step in our study will be a simple one explaining what is about to happen:

@codeblock[#:keep-lang-line? #f]|{
  #lang conscript
  (defstep (intro)
    @md{# Prisoner's Dilemma

    You are a suspect in a crime investigation. Your accomplice (another
    study participant) has also been arrested and is being held separately.
    Each of you has a choice: will you attempt to **cooperate** with your
    accomplice by staying silent, or will you **defect** and admit everything
    to the police, betraying your partner?

    * If you both choose to cooperate with each other, you’ll each get a 1-year
      prison sentence.

    * If you choose to defect and your partner tries to cooperate, you’ll go free
      and your partner will get a 20-year prison sentence.

    * If you both try to betray each other, you’ll each receive a 5-year prison
      sentence.

    @button{Continue...}})
}|

In concrete programming terms, this study @tech{step} is implemented as a function with no
arguments (here named @racket[intro]) that returns a study @tech{page}.

@margin-note{The @litchar{@"@"} and surrounding curly-style @litchar["{}"] indicate that we’re calling
the @racket[md] function using @secref["scribble-in-conscript"], to make it easier to intermingle
form elements and text.}

The @racket[md] function can take any mix of @tech{Markdown} and form controls and produce a study
@tech{page} for us. 

By adding a @racket[button], we give the user a way to move on to the next step.

@;===============================================

@section{Pairing up prisoners}


@codeblock[#:keep-lang-line? #f]|{
  #lang conscript
  (defstep (waiter)
    @md{# Please Wait

        Please wait while another participant joins the queue.

        @refresh-every[5]})

  (define matchmaker (make-matchmaker 2))

  (defstep (pair-with-someone)
    (matchmaker waiter))
}|

Conscript gives you functions that handle the messy work of matching up people into groups.

@racket[make-matchmaker] gives you a function that takes 1 argument, which should be a
procedure that produces a study step/page. When you call this matchmaker function, it will put the
current participant into a group, and display the page you provided. Every time that page is
refreshed, it checks to see if the group has been filled up by other participants; if not, it shows
the same page again. If the group is filled, the matchmaker function will automatically move
participants to the next step in the study.

Here we’re creating a matchmaker function that keeps group sizes to @racket[2] members. Inside the
@racket[pair-with-someone] step we call that function, giving it the @racket[waiter] step that
refreshes the page every 5 seconds. That refresh will in turn kick back to the
@racket[pair-with-someone] step, keeping the loop going until another participant fills the other
slot in the current group.

@;===============================================

@section{Making and storing choices}

Now comes the heart of our study: presenting choices to participants and storing them in a way that
participants in the same group can access each other's responses.

@codeblock[#:keep-lang-line? #f]|{
  #lang conscript

  (define (store-my-choice! val)
    (set! my-choice val)
    (store-my-result-in-group! 'choice val))

  (defstep (make-choice)
    (define (cooperate) (store-my-choice! 'cooperate))
    (define (defect) (store-my-choice! 'defect))

    @md{# Make Your Choice

        @button[#:id "cooperate" cooperate]{Cooperate}
        @button[#:id "defect" defect]{Defect}})
}|

@inline-note[#:type 'tip]{In DrRacket, hover your mouse over the @racket[cooperate] and
@racket[defect] identifiers after @racket[define]: you should see arrows that make clear where these
functions are being used (in the calls to @racket[button] that follow).}

Let's break down what's happening here:

@subsection{The @tt{store-my-choice!} helper function}

This function does two things when a participant makes their choice:

@itemlist[#:style 'ordered

@item{@racket[(set! my-choice val)] — Stores the choice in the current participant's @racket[my-choice]
variable for later reference.}

@item{@racket[(store-my-result-in-group! 'choice val)] — Stores the choice in a special shared data
structure that other members of the current group can access. The first argument, @racket['choice],
is a @deftech{lookup key} that we'll use later to retrieve this value. Think of it as a label or
tag for this piece of information.}

]

The @racket[store-my-result-in-group!] function is provided by Conscript specifically for
multi-participant studies. It handles all the complexity of managing shared data within groups,
allowing each participant to store values that their group members can later retrieve.

@subsection{The @tt{make-choice} step}

The page shows you two buttons: you can cooperate or defect. Buttons normally just move you to the
next step, but these also call functions that record the choice that was made.

We define two small helper functions (@racket[cooperate] and @racket[defect]) inside this
@racket[defstep] because they won't be used anywhere else. Each one calls our
@racket[store-my-choice!] function with the appropriate value: @racket['cooperate] or
@racket['defect].

@;===============================================

@section{Waiting for your partner}

Even after you've made your choice, you can't really move on until the other person in your group
has made theirs. This step handles that waiting:

@codeblock[#:keep-lang-line? #f]|{
  #lang conscript
  (defstep (wait)
    (if (= (current-group-results-count 'choice) 0)
        @md{# Please Wait

            Please wait for the other participant to make their choice...

            @refresh-every[5]}
        (skip)))
}|

The @racket[current-group-results-count] function counts how many @emph{other} members of your group
have stored a result under the given lookup key (in this case, @racket['choice]). Note that by
default, this function does @bold{not} count the current participant's own result.

@itemlist[

@item{If the count is @racket[0], that means the other participant hasn't made their choice yet.
In this case, we display a waiting page that refreshes every 5 seconds. Each refresh triggers this
step again, giving us another chance to check.}

@item{If the count is not @racket[0] (meaning the other participant @emph{has} made their choice),
we call @racket[(skip)] to automatically move to the next step.}

]

@;===============================================

@section{Calculating and displaying the result}

Once both participants have made their choices, we can determine the outcome:

@codeblock[#:keep-lang-line? #f]|{
  #lang conscript
  (defstep (display-result)
    (define their-choice (first (current-group-member-results 'choice)))
    (set! prison-sentence
          (match* (my-choice their-choice)
            [('cooperate 'cooperate) 1]
            [('cooperate 'defect) 20]
            [('defect 'defect) 5]
            [('defect 'cooperate) 0]))

    @md{# Result

        The other person chose to @~a[their-choice], while you chose to @~a[my-choice].

        You get @~a[prison-sentence] years of prison.})
}|

@subsection{Retrieving the partner's choice}

@racket[(current-group-member-results 'choice)] returns a list containing the results that
@emph{other} members of the current group have stored under the lookup key @racket['choice]. Since
we're in a group of 2, this list will contain exactly one element: our partner's choice.

We use @racket[first] to extract that single value and store it in @racket[their-choice].

@subsection{Determining the outcome}

The @racket[match*] form lets us pattern-match against multiple values at once. We provide it with
two values (@racket[my-choice] and @racket[their-choice]) and then list all possible combinations:

@itemlist[

@item{If both cooperated: 1 year each}
@item{If I cooperated and they defected: I get 20 years (the sucker's payoff)}
@item{If both defected: 5 years each}
@item{If I defected and they cooperated: I go free (0 years)}

]

We store this result in @racket[prison-sentence], which records it in the database for later
analysis.

@subsection{Displaying the result}

Finally, we display both choices and the outcome to the participant. The @racket[~a] function
converts values to strings so they can be displayed in the Markdown text.

@;===============================================

@section[#:tag "pd-all-together"]{Putting it all together}

Now we tie all the steps together into a complete study:

@codeblock[#:keep-lang-line? #f]|{
  #lang conscript
  (defstudy prisoners-dilemma
    [intro --> pair-with-someone --> make-choice --> wait --> display-result]
    [display-result --> display-result])
}|

This @racket[defstudy] form defines our study's transition graph: participants move through
@racket[intro], then @racket[pair-with-someone], then @racket[make-choice], then @racket[wait], and
finally @racket[display-result]. The last line @racket[[display-result --> display-result]]
indicates that @racket[display-result] is a terminal step — it transitions to itself, so
participants remain on that page once they reach it.

@inline-note[#:type 'warning]{Don't forget to add @racket[(provide prisoners-dilemma)] at the top of
your file so that the Congame server can access your study!}

@;===============================================

@section{Testing the Prisoner's Dilemma}

To test your study, you'll need to simulate two participants:

@itemlist[#:style 'ordered

@item{Upload your study to your local Congame server (the Docker container) by clicking the
@onscreen{Upload Study} button in DrRacket.}

@item{Create a new instance of your study.}

@item{In your main browser window, enroll in the study as the first participant. You should
progress through the introduction and then see the “Please wait while another participant joins the
queue” page.}

@item{Open a new @bold{private/incognito browser window} and navigate to your local Congame server
(usually @tt{http://localhost:5100/_anon-login/[INSTANCENAME]}). This will create a new anonymous
participant and enroll it in the study. @inline-note{Using a private/incognito window ensures you're not sharing cookies or session data
between the two participants. Alternatively, you can use a completely different browser (e.g.,
Firefox if you're testing with Chrome).}}

@item{In the second browser window, progress through the study as the second participant. At the
matchmaking step, both participants should be matched together and can proceed to make their
choices.}

@item{Make a choice in each browser window and observe how the waiting and result steps work.}

]


@;===============================================

@section{Key concepts recap}

This tutorial introduced several important concepts for multi-participant studies:

@itemlist[

@item{@bold{Matchmaking} (@racket[make-matchmaker]): Automatically groups participants and manages
the waiting process until groups are filled.}

@item{@bold{Storing group results} (@racket[store-my-result-in-group!]): Allows participants to
store data that other members of their group can access, using lookup keys to organize different
pieces of information.}

@item{@bold{Retrieving group results} (@racket[current-group-member-results]): Retrieves data that
other group members have stored under a specific lookup key.}

@item{@bold{Counting group results} (@racket[current-group-results-count]): Checks how many other
group members have stored data under a specific lookup key, useful for wait conditions.}

@item{@bold{Pattern matching} (@racket[match*]): A powerful way to handle different combinations of
values and determine outcomes.}

]

@;===============================================

@section[#:tag "pd-next-steps"]{Next Steps}

Now that you've built a basic multi-participant study, you might want to explore:

@itemlist[

@item{Experiment with different group sizes. This would involve changing the argument to
@racket[make-matchmaker], the checking of other group members’ results in the @racket[wait] step,
and the @racket[match*] comparison of results in the @racket[display-result] step. What would a
three-person Prisoner's Dilemma look like?}

@item{Try adding multiple rounds to your Prisoner's Dilemma. See
@github-link{congame-example-study/prisoners-dilemma.rkt} for an example of how to implement a
repeated game.}

@item{Consult the @secref["Conscript_Cookbook"] for more recipes and patterns for common study
tasks.}

@item{Review the reference documentation for @racket[make-matchmaker],
@racket[store-my-result-in-group!], and related functions to understand all their options and
capabilities.}

]
