#lang scribble/manual

@(require (for-label conscript/base
                     racket/contract
                     racket/match)
          "doc-util.rkt")

@title{Conscript tutorial: simple studies}

Prisoner's Dilemma (PD) is a classic game theory scenario. @mark{Google it.}

@tktk{Explanation of the Prisoner's Dilemma}

We're going to make a study that pits participants against each other in this scenario.

@codeblock[#:keep-lang-line? #f]{
  #lang conscript
  (defvar/instance all-choice-groups dilemma-choices)
  (defvar outcome)
}

@tktk{First we need variables to keep track of answers:

@racket[choices] = hash of instance groups to (hash of participants to answers). There's only going
to be one of these for the entire instance of the study, hence the use of @racket[defvar/instance].

@racket[outcome] = how many years in prison you ended up with. This is unique to each participant,
so we use @racket[defvar].}

@tktk{Explain variables and scope}

@tktk{Explain structure of @tt{choices} variable}

@codeblock[#:keep-lang-line? #f]|{
  #lang conscript
  (defstep (intro)
    @md{# Prisoner's Dilemma

    @button{Continue...}})
}|

The first step just shows a button to continue. 

Buttons give the user a way to move on to the next step. You can also give it a procedure to call
when clicked if you want something else to happen in addition to moving on to the next step.

Whatever you put in a defstep expression better result in a study @tech{page}. The @racket[md] and
@racket[html] functions do this part for you.

@codeblock[#:keep-lang-line? #f]|{
  #lang conscript
  (defstep (waiter)
    @md{# Please Wait

        Please wait while another participant joins the queue.

        @refresh-every[5]})

  (defstep matchmake
    (let ([matchmaker (make-matchmaker 2)])
      (lambda ()
        (matchmaker waiter))))
}|

Conscript gives you functions that handle the messy work of matching up people into groups.

@racket[make-matchmaker] gives you a function that takes 1 argument, that argument should be a
procedure that produces a @racket['page] X-expression. When you call this matchmaker function, it
will put the current participant in the current partial group, and displays the page using the
argument you gave it. Every time that page is refreshed, it checks to see if the group has been
filled up by other participants; if not it shows the same page again. If the group is filled, the
matchmaker function will skip the user to the next step in the study.

@codeblock[#:keep-lang-line? #f]|{
  #lang conscript
  (define (&my-choice)
    (parameterize ([current-hash-maker hash])
      (&opt-hash-ref*
       (get-current-group)
       (current-participant-id))))

  (define (make-choice! choice)
    (with-study-transaction
      (set! all-choice-groups ((&my-choice) (if-undefined all-choice-groups (hash)) choice))))
}|

The @racket[all-choice-groups] is a hash table whose keys are all the groups in the study instance.
If we get that table’s value for @racket[get-current-group], we get another hash table whose keys
are the participants in that group, and whose values are their individual choices.

This code is all very complicated but it’s basically saying “take the whole @tt{all-choice-groups}
hash table and update just the value for the current participant in the current group.” @mark{Lenses
are cool, read about them.}

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
    (if (= (hash-count (hash-ref all-choice-groups (get-current-group))) 1)
        @md{# Please Wait

            Please wait for the other participant to make their choice...

            @refresh-every[5]}
        (skip)))
}|

Even after you’ve made your choice, you can't really move on until the other person in your group
has made theirs. So this step checks to see if there are 2 choices recorded in the current group --
if not, displays a page that refreshes every 5 seconds (which triggers the same step again); but if
it finds 2 choices, it automatically skips to the next step.

@codeblock[#:keep-lang-line? #f]|{
  #lang conscript
  ; Helper function
  (define (group->choices group)
    (match group
      [(hash (current-participant-id) my-choice
             #:rest (app hash-values (list their-choice)))
       (values my-choice their-choice)]))

  (defstep (display-result)
    (define my-group (hash-ref all-choice-groups (get-current-group)))
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
@racket[group-choices] helper function is a way to say “find my choice in the current group and put
it here, and whatever the other person’s response was, put it over there.”

Then we set @racket[outcome] to the result of the paired choices. This records it in the database.
We also display it to the user (@racket[~a] converts the number to a string).

@codeblock[#:keep-lang-line? #f]|{
  #lang conscript
  (defstudy prisoners-dilemma
    [intro --> matchmake --> make-choice --> wait --> display-result]
    [display-result --> display-result])
}|

@tktk{Define the whole study.}

@;===============================================

@section{Testing the Prisoner’s Dilemma}

@tktk{Upload and test; tricks for logging in anonymously in another tab to simulate a second
participant}

