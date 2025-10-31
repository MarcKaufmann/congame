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

@section{Modeling our data}

As with all studies, we start by defining variables to keep track of responses and results of the
study. 

The wrinkle in this case is that our study code needs to be able to see the responses of other
participants, not just the current one --- because each person’s outcome depends on how their
response matches up with the response of the person they were paired with.

We can do this by adjusting the @tech{scope} of the variables we declare.

@;------------------------------------------------

@subsection{Understanding variables and scope}

A variable’s @deftech{scope} is the context in which its value is recorded and shared:

@itemlist[

@item{A variable with @deftech{participant scope} will record a unique value for each participant;
when accessed, such a variable will return the value that was last set for the current participant.}

@item{A variable with @deftech{instance scope} will record a unique value for the current study
@tech{instance}. When accessed, the variable will return the value that was last set by @emph{any
participant} in the current study instance.}

]

Consider an example study that begins with the following code:

@racketblock[
  (defvar myval)

  (defstep (get-random)
    (set! myval (random 10))) (code:comment @#,elem{random number 0–9 for current participant})

  (code:comment @#,elem{...})
]

Let’s say Alice and Bob both take this study: when Alice participates, she gets @racket[8] stored
in @racket[myval], and when Bob participates, he gets @racket[6]. During Alice’s participation, 
code that looks at @racket[myval]’s value will see @racket[8] there; likewise it will see @racket[6]
when when Bob is the current participant. If, while Alice is participating, the study code
overwrites @racket[myval] with @racket[(set! myval 9)], the value of Bob’s @racket[myval] will
remain @racket[6].

We can change the @tech{scope} of a variable by changing the way we define it:

@racketblock[
  ((code:hilite defvar/instance) myval)

  (defstep (get-random)
    (set! myval (random 10))) (code:comment @#,elem{random number 0–9 for current participant})

  (code:comment @#,elem{...})
]

Here we have changed the definition of @racket[myval] to use @racket[defvar/instance] instead of
@racket[defvar], which means that @racket[myval] now has @tech{instance scope}.

Now, when Alice and Bob take this study (assuming they enroll in the same study @tech{instance},
they share the value of @racket[myval]. If Bob participants after Alice, whatever random number the
study generates for him during the @racket[get-random] step will @emph{overwrite} the value that
was put there during Alice’s participation.

@;------------------------------------------------

@subsection{Prisoner’s Dilemma variables}

The Prisoner’s Dilemma is modeled around @emph{pairs of prisoners}, each of whom can provide one of
two responses: @racketvalfont{cooperate} or @racketvalfont{defect}.

This suggests a nested data structure that is more complicated than simple strings and numbers. We
want to be able to look up the pair that the current participant is assigned to, and to see the
choice of each prisoner in the pair:

@verbatim[#:indent 6]{
┏━━━━━━━━━All prisoner groups━━━━━━━━━━┓
┃                                      ┃
┃ Group ID ──▶ Group━━━━━━━━━━━━━━━━━┓ ┃
┃              ┃ Prisoner ID──▶Choice┃ ┃
┃              ┃ Prisoner ID──▶Choice┃ ┃
┃              ┗━━━━━━━━━━━━━━━━━━━━━┛ ┃
┃                                      ┃
┃ Group ID ──▶ Group━━━━━━━━━━━━━━━━━┓ ┃
┃              ┃ Prisoner ID──▶Choice┃ ┃
┃              ┃ Prisoner ID──▶Choice┃ ┃
┃              ┗━━━━━━━━━━━━━━━━━━━━━┛ ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
}

Racket provides @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{hash tables} for implementing
these kinds of @tt{Lookup key → value} pairs. We won’t get into the details of hash tables here. The
key point is that the box labeled @tt{All prisoner groups} constitutes a single variable that can
hold the nested lookup table inside. We will declare this variable with @tech{instance scope} so
that it will be shared between all participants in the study instance.

@margin-note{See @secref["hash-tables" #:doc '(lib "scribblings/guide/guide.scrbl")] in the Racket
Guide for more info on hash tables (called “dictionaries” in some other languages)}

So here’s how we start our study:

@codeblock[#:keep-lang-line? #t]{
  #lang conscript

  (defvar/instance all-prisoner-groups)
  (defvar prison-sentence)
}

The @racket[all-prisoner-groups] variable is declared with @racket[defvar/instance] to give it
@tech{instance scope}. We don’t need to actually specify the details of its structure in the code
at this stage --- we just need to give it a name and a scope.

The @racket[(defvar prison-sentence)] line is familiar enough: this is where we'll record the result
of the study for the current participant (the length of the prison sentence they end up with).
Because it is declared with @racket[defvar], it will have @tech{participant scope}.

@;===============================================

@section{The “continue” step}

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

@section{Pairing up Prisoners}


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

@racket[make-matchmaker] gives you a function that takes 1 argument, that argument should be a
procedure that produces a study step/page. When you call this matchmaker function, it will put the
current participant in the current partial group, and display the page using the argument you gave
it. Every time that page is refreshed, it checks to see if the group has been filled up by other
participants; if not it shows the same page again. If the group is filled, the matchmaker function
will skip the user to the next step in the study.

Here we’re creating a function @racket[matchmaker] that keeps group sizes to @racket[2] members.
Inside the @racket[pair-with-someone] step we call that function, giving it a study step
@racket[waiter] that refreshes the page every 5 seconds. That refresh will in turn kick back to the
@racket[pair-with-someone] step, keeping the loop going until another participant fills the other
slot in the current group.

@;===============================================

@section{Making choices}

@codeblock[#:keep-lang-line? #f]|{
  #lang conscript
  (require data/monocle)
  (define (&my-choice)
    (parameterize ([current-hash-maker hash])
      (&opt-hash-ref*
       (get-current-group)
       (current-participant-id))))

  (define (make-choice! choice)
    (with-study-transaction
      (set! all-prisoner-groups ((&my-choice) (if-undefined all-prisoner-groups (hash)) choice))))
}|

The @racket[all-prisoner-groups] variable is a @tech[#:doc '(lib
"scribblings/guide/guide.scrbl")]{hash table} whose keys are all the groups in the study instance.
If we get that table’s value for @racket[get-current-group], we get another hash table whose keys
are the participants in that group, and whose values are their individual choices.

@tktk{At some point Congame will provide helpful abstractions for doing this in a simpler way.}

This code is all very complicated but it’s basically saying “take the whole @tt{all-prisoner-groups}
hash table and update just the value for the current participant in the current group.” 

@codeblock[#:keep-lang-line? #f]|{
  #lang conscript
  (defstep (make-choice)
    (define (cooperate)
      (make-choice! 'cooperate))

    (define (defect)
      (make-choice! 'defect))

    @md{# Make Your Choice

        @button[#:id "cooperate" cooperate]{Cooperate}
        @button[#:id "defect" defect]{Defect}})
}|

The page shows you two buttons: you can cooperate or defect. Like we said before, buttons move you
to the next step. But these also are given functions that do additional work to record the choice
that was made. Yes, you can make functions inside functions. We do so inside this defstep because
these functions won't be used anywhere else. The two functions in turn use the `make-choice!`
function we defined above.

@codeblock[#:keep-lang-line? #f]|{
  #lang conscript
  (defstep (wait)
    (if (= (hash-count (hash-ref all-prisoner-groups (get-current-group))) 1)
        @md{# Please Wait

            Please wait for the other participant to make their choice...

            @refresh-every[5]}
        (skip)))
}|

Even after you’ve made your choice, you can't really move on until the other person in your group
has made theirs. So this step checks to see if there are 2 choices recorded in the current group --
if not, displays a page that refreshes every 5 seconds (which triggers the same step again); but if
it finds 2 choices, it automatically skips to the next step.

@;===============================================

@section{Sentencing time}

@codeblock[#:keep-lang-line? #f]|{
  #lang conscript
  ; Helper function
  (define (group->choices group)
    (match group
      [(hash (current-participant-id) my-choice
             #:rest (app hash-values (list their-choice)))
       (values my-choice their-choice)]))

  (defstep (display-result)
    (define my-group (hash-ref all-prisoner-groups (get-current-group)))
    (define-values (my-choice their-choice) 
      (group->choices my-group))
    (set! outcome
          (match* (my-choice their-choice)
            [('cooperate 'cooperate) 1]
            [('cooperate 'defect) 20]
            [('defect 'defect) 5]
            [('defect 'cooperate) 0]))

    @md{# Result

        You get @~a[outcome] years of prison.})
}|

The current “group” is a hash of participant IDs to choices. We know what the current participant's
ID is, but we don't know the ID of the rando we got paired with (and we don’t care). So the
@racket[group->choices] helper function is a way to say “find my choice in the current group and put
it here, and whatever the other person’s response was, put it over there.”

Then we set @racket[outcome] to the result of the paired choices. This records it in the database.
We also display it to the user (@racket[~a] converts the number to a string).

@codeblock[#:keep-lang-line? #f]|{
  #lang conscript
  (defstudy prisoners-dilemma
    [intro --> pair-with-someone --> make-choice --> wait --> display-result]
    [display-result --> display-result])
}|

@tktk{Define the whole study.}

@;===============================================

@section{Testing the Prisoner’s Dilemma}

@tktk{Upload and test; tricks for logging in anonymously in another tab to simulate a second
participant}

