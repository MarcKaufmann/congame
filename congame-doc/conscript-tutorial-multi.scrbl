#lang scribble/manual

@(require (for-label conscript/base
                     conscript/form0
                     conscript/survey-tools
                     conscript/matchmaking
                     racket/contract
                     racket/list)
          "doc-util.rkt")

@title[#:style 'quiet #:tag "multi-participant-tutorial"]{Tutorial: Multi-Participant Studies}

This tutorial walks through a complete example of a multi-participant study that demonstrates how to:

@itemlist[

@item{Randomly assign participants to treatment or control groups}

@item{Match treatment participants into pairs}

@item{Have participants complete tasks and collect their responses}

@item{Calculate outcomes based on comparisons between matched participants}

]

The study implements a simple experiment where participants complete arithmetic tasks. Control group
participants receive payment based only on their own performance, while treatment group participants
are matched with a partner, and their payment depends on whether they score higher than their
partner.

This tutorial assumes you're familiar with the basic concepts covered in @secref["overview"] and
@secref["intro"]. We'll focus on explaining the code itself, walking through each section to
understand what it does and why.

@bold{To follow along:} You can find the complete code for this study in the
@seclink["Multi-participant_Study_Recipe"]{Multi-participant Study Recipe} section of the Conscript
cookbook. 

@;===============================================

@section{Study Overview}

Here's what happens when someone participates in this study:

@itemlist[#:style 'ordered

@item{The participant is randomly assigned to either the treatment group or the control group, using
balanced randomization (2 treatment, 2 control per group of 4 participants).}

@item{They see instructions explaining how payment works for their assigned group.}

@item{They complete 2 simple arithmetic tasks.}

@item{They see their score (number of correct answers).}

@item{@bold{Control group:} They immediately see their payment, calculated as $1.00 base + $0.20 per
correct task.}

@item{@bold{Treatment group:} They wait to be matched with another treatment participant. Once
matched, the system compares their scores. The participant with the higher score receives $3.40
($1.00 + $2.40 bonus); the one with the lower score receives just $1.00. If scores are tied, one
winner is chosen randomly.}

]

Now let's look at how each part of this study is implemented in code.

@;===============================================

@section{Preamble}

@codeblock|{
#lang conscript

(require conscript/form0
         conscript/survey-tools
         racket/list
         racket/match)

(provide simple-treatment-study)
}|

As always we begin with @code{#lang conscript}, @racket[require] the additional modules we’ll need,
and @racket[provide] the name of our study (which we’ll define at the very end). (To suppress errors
in DrRacket while you’re writing the file, you can comment out the @racket[provide] line with
@litchar{;} and then uncomment it when you’ve finished actually defining
@racket[simple-treatment-study].)

@;===============================================

@section{Treatment Assignment}

At the start of the study, we need to set up variables and assign each participant to a treatment
condition. Here's the code:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defvar/instance treatments)
(defvar is-treatment?)

(defstep (assign-treatment)
  ; Balanced: out of every 4 participants, assign 2 to treatment, 2 to control
  (with-study-transaction
      (when (or (undefined? treatments) (null? treatments))
        (set! treatments (shuffle '(#t #t #f #f))))
    (set! is-treatment? (first treatments))
    (set! treatments (rest treatments)))
  (skip))
}|

@subsection{Instance-scoped Variables}

@racket[(defvar/instance treatments)] creates a variable with @tech{instance scope}. Unlike
regular @racket[defvar] variables (which store a separate value for each participant),
instance-scoped variables are @emph{shared across all participants} in a study instance.

Here, @racket[treatments] holds a list of treatment assignments (@racket[#t] for treatment,
@racket[#f] for control) that all participants will draw from. This is how we ensure balanced
assignment: we create a list with exactly 2 @racket[#t] and 2 @racket[#f] values, so out of every 4
participants, 2 will be assigned to each condition.

@racket[is-treatment?] is a regular (participant-scoped) variable that stores whether the
@emph{current} participant is in the treatment group.

@subsection{The Assignment Step}

The @racket[assign-treatment] step does three things:

@itemlist[#:style 'ordered

@item{Checks if the @racket[treatments] list is empty or undefined. If so, it creates a new shuffled
list of assignments: @racket['(#t #t #f #f)]. The @racket[shuffle] function randomly reorders this
list.}

@item{Takes the first assignment from the list and stores it in @racket[is-treatment?] for the
current participant.}

@item{Removes that assignment from the list (using @racket[rest]) so the next participant gets the
next assignment.}

]

All of this happens inside @racket[with-study-transaction], which ensures that if two participants
reach this step at exactly the same time, only one will access the @racket[treatments] list at a
time. This prevents both participants from accidentally getting the same assignment.

The step ends with @racket[(skip)], which immediately moves the participant to the next step without
displaying any page.

@;===============================================

@section{Instructions}

After assignment, participants see instructions explaining the study. The instructions differ based
on whether they're in the treatment or control group:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(define instructions-control
  @md*{You are in the **control group**.

    You will complete 2 simple arithmetic tasks. Your payment will be:
    - $1 base payment
    - $0.20 for each task you get correct

    Good luck!})

(define instructions-treatment
  @md*{You are in the **treatment group**.

    You will complete 2 simple arithmetic tasks. After finishing, you will be matched
    with another participant in the treatment group. Your payment will depend on both
    your scores:

    - **Winner** (higher score): $1 + $2.40 = $3.40
    - **Loser** (lower score): $1
    - **Tie**: 50-50 chance of winning

    **Important:** After you complete the tasks, you may need to wait briefly while
    we match you with another participant. Please be patient and wait for another
    participant to complete their tasks so the two of you can be matched.})

(defstep (instructions)
  @md{# Instructions

    @(if is-treatment? instructions-treatment instructions-control)

    @button{Begin}})
}|

This code defines two sets of instructions using @racket[md*] (note the asterisk). The @racket[md*]
function is like @racket[md], but returns a value that can be reused within other page content,
rather than as a page by itself.

In the @racket[instructions] step, we use @racket[(if is-treatment? instructions-treatment
instructions-control)] to display the appropriate instructions based on the participant's treatment
assignment. This is a straightforward conditional: if @racket[is-treatment?] is @racket[#t], show
treatment instructions; otherwise show control instructions.

@;===============================================

@section{Tasks}

Next, participants complete two arithmetic tasks. This section involves several pieces:

@subsection{Setting Up Variables}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defvar task1-response)
(defvar task2-response)
(defvar score)
(defvar payment)

(defstep (init-tasks)
  (set! score 0)
  (set! payment 0)
  (skip))
}|

We create separate variables for each task response, plus variables for @racket[score] and
@racket[payment] that we'll calculate later. The @racket[init-tasks] step initializes @racket[score]
and @racket[payment] to @racket[0] and immediately skips to the next step.

@subsection{Task 1: Using Forms}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
;; Task 1: What is 7 + 5?
(define-values (task1-form task1-onsubmit)
  (form+submit
   [task1-response (ensure binding/number (required))]))

(define (render-task1 rw)
  @md*{@rw["task1-response" @input-number{Your answer}]
    @|submit-button|})

(defstep (task1)
  @md{# Task 1

    What is 7 + 5?

    @form[task1-form task1-onsubmit render-task1]})
}|

This demonstrates the standard pattern for collecting input via forms, which you may have seen in
@secref["intro"]:

@itemlist[#:style 'ordered

@item{@racket[form+submit] defines the structure of the form data and returns both a form object and
a submission handler. Here we specify that @racket[task1-response] must be a number (via
@racket[binding/number]) and is @racket[required].}

@item{@racket[render-task1] is a helper function that renders the form's input widget. It takes one
argument @racket[rw] (a widget renderer) and returns the rendered form elements.}

@item{The @racket[task1] step displays the question and includes the form by passing the form object,
submission handler, and renderer to @racket[form].}

]

@subsection{Task 2}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
;; Task 2: What is 15 - 6?

;; Lots of identical code from above is repeated.
;; See conscript-multi-example.rkt in the repo for an example of how this could
;; be simplified

(define-values (task2-form task2-onsubmit)
  (form+submit
   [task2-response (ensure binding/number (required))]))

(define (render-task2 rw)
  @md*{@rw["task2-response" @input-number{Your answer}]
    @|submit-button|})

(defstep (task2)
  @md{# Task 2

    What is 15 - 6?

    @form[task2-form task2-onsubmit render-task2]})
}|

Task 2 follows the exact same pattern as Task 1, just with a different question and variable name.
As the comment notes, in a real study you might want to refactor this repeated code (see
@github-link{congame-example-study/conscript-multi-example.rkt} for an example), but for this
tutorial we keep it simple and explicit.

@subsection{Calculating and Displaying the Score}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (show-score)
  (set! score (+ (if (= task1-response 12) 1 0)
                 (if (= task2-response 9) 1 0)))

  @md{# Your Score

    You answered **@(~a score) out of 2** tasks correctly.

    @button{Continue}})
}|

After both tasks are completed, we calculate the score by checking each answer against the correct
answer (12 for task 1, 9 for task 2). Each @racket[if] expression returns @racket[1] if the answer
is correct and @racket[0] if not; we add these values together to get the total score.

We then display the score to the participant using @racket[~a] to convert the number to a string for
display in the Markdown text.

@;===============================================

@section{Control Group Payment}

For control group participants, calculating payment is straightforward:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (control-payment)
  (set! payment (+ 1.00 (* score 0.20)))
  @md{# Your Payment

    Your payment is:
    - $1.00 base
    - @(~$ (* score 0.20)) for @(~a score) correct task(s)

    **Total: @(~$ payment)**

    Thank you for participating!})
}|

We calculate @racket[payment] as $1.00 plus $0.20 times their score. The @racket[~$] function
(provided by @racketmodname[conscript/survey-tools]) formats numbers as dollar amounts with two
decimal places.

@;===============================================

@section{Treatment Group: Matchmaking and Results}

The treatment group process is more complex because we need to match participants and compare their
scores:

@subsection{Variables and the Matchmaker}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defvar opponent-score)
(defvar did-win?)

(define matchmaker (make-matchmaker 2))
}|

We create two more variables: @racket[opponent-score] to store the matched partner's score, and
@racket[did-win?] to store whether the current participant won the competition.

@racket[(make-matchmaker 2)] creates a @tech{matchmaker function} that groups participants into
pairs (groups of 2). This function will handle the complex logic of matching participants as they
arrive.

@subsection{Waiting for a Match}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
;; Wait for a match using the matchmaking module
(defstep (wait-for-match)
  @md{# Waiting for Match

    You have finished the tasks. We are now matching you with another participant...

    Please wait.

    @refresh-every[5]})

(defstep (pair-with-someone)
  (matchmaker wait-for-match))
}|

The @racket[wait-for-match] step shows a waiting page that refreshes every 5 seconds (using
@racket[refresh-every]).

The @racket[pair-with-someone] step calls our @racket[matchmaker] function, passing it the
@racket[wait-for-match] step. Here's how this works:

@itemlist[

@item{When a participant reaches @racket[pair-with-someone], the matchmaker assigns them to a group.}

@item{If there's not yet a partner available for their group, the matchmaker displays the
@racket[wait-for-match] page.}

@item{Each time the page refreshes (every 5 seconds), it checks again whether a partner has been
found.}

@item{Once another treatment participant is matched into the same group, both participants
automatically proceed to the next step.}

]

@subsection{Storing and Retrieving Group Results}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (record-score-for-group)
  (store-my-result-in-group! 'score score)
  (skip))

(defstep (get-opponent-score)
  (define other-score (first (current-group-member-results 'score)))
  (cond
    [other-score
     (set! opponent-score other-score)
     ; Determine winner
     (set! did-win?
           (or (and (= score opponent-score)
                    (> (random 2) 0))
               (> score opponent-score)))
     (skip)]
    [else
     @md{# Please wait

       Waiting to learn your opponent's score…

       @refresh-every[2]}]))
}|

These steps handle the coordination between matched participants:

@bold{@tt{record-score-for-group}:} Uses @racket[store-my-result-in-group!] to save the current
participant's score in a way that other participants in the same group can access it. This function
stores results in a shared data structure (similar to how @racket[defvar/instance] creates shared
variables). The step immediately skips to the next one.

@bold{@tt{get-opponent-score}:} Retrieves scores from all other group members using
@racket[current-group-member-results]. Since we're in a group of 2, this returns a list with one
element: our partner's score.

If the partner's score is available, we:
@itemlist[#:style 'ordered

@item{Store it in @racket[opponent-score]}

@item{Determine the winner: @racket[did-win?] is @racket[#t] if either (a) scores are tied and
@racket[(random 2)] returns @racket[1], or (b) our score is higher than the opponent's score}

@item{Skip to the next step}
]

If the partner's score isn't available yet (because they haven't finished their tasks), we display a
waiting page that refreshes every 2 seconds.

@subsection{Displaying Treatment Results}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (treatment-results)
  (set! payment (+ 1.0 (if did-win? 2.4 0)))

  @md{# Match Results

    **Your score:** @(~a score) out of 2
    **Opponent's score:** @(~a opponent-score) out of 2

    **You @(if did-win? "🎉 WON!" "lost this round.")**

    Your payment is:
    - $1.00 base
    - @(if did-win? "$2.40 for winning" "$0.00 (you lost)")

    **Total: @(~$ payment)**

    Thank you for participating!})
}|

Finally, we calculate payment based on whether they won ($3.40 total) or lost ($1.00 total), and
display the results along with both participants' scores.

@;===============================================

@section{Main Study Flow}

The @racket[defstudy] form ties all the steps together into the study's transition graph:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstudy simple-treatment-study
  [assign-treatment
   --> instructions
   --> init-tasks
   --> task1
   --> task2
   --> show-score
   --> ,(lambda ()
          (if is-treatment? 'pair-with-someone 'control-payment))]

  [control-payment --> control-payment]

  [pair-with-someone
   --> record-score-for-group
   --> get-opponent-score
   --> treatment-results]
  [treatment-results --> treatment-results])
}|

This defines a study named @racket[simple-treatment-study] with several paths:

@bold{Main path:} Starts with @racket[assign-treatment] and proceeds through instructions, task
initialization, both tasks, and score display. After @racket[show-score], an inline
@racket[lambda] function determines the next step based on treatment assignment:
@itemlist[
@item{Treatment participants go to @racket['pair-with-someone]}
@item{Control participants go to @racket['control-payment]}
]

@bold{Control branch:} The @racket[control-payment] step transitions to itself, meaning it's a
terminal step (participants stay on this page once they reach it).

@bold{Treatment branch:} Proceeds through the matchmaking and result steps
(@racket[pair-with-someone] → @racket[record-score-for-group] → @racket[get-opponent-score] →
@racket[treatment-results]), with @racket[treatment-results] also transitioning to itself as a
terminal step.

@;===============================================

@section{Key Concepts Recap}

This tutorial introduced several important concepts for multi-participant studies:

@itemlist[

@item{@bold{Instance-scoped variables} (@racket[defvar/instance]): Variables shared across all
participants in a study instance, useful for coordinating assignments and shared data.}

@item{@bold{Study transactions} (@racket[with-study-transaction]): Ensures that when multiple
participants are accessing shared variables simultaneously, their operations don't interfere with
each other.}

@item{@bold{Matchmaking} (@racket[make-matchmaker]): Automatically groups participants and manages
the waiting process until groups are filled.}

@item{@bold{Group result storage} (@racket[store-my-result-in-group!],
@racket[current-group-member-results]): Functions that let participants in the same group share and
access each other's data.}

@item{@bold{Conditional transitions}: Using @racket[lambda] functions in the transition graph to
send participants down different paths based on their data (like treatment assignment).}

]

@;===============================================

@section{Next Steps}

This study demonstrates the core mechanics of multi-participant research in Conscript. From here,
you might want to:

@itemlist[

@item{Explore the more sophisticated version in @github-link{congame-example-study/conscript-multi-example.rkt},
which refactors the repeated task code using helper functions}

@item{Look at the @seclink["prisoners-dilemma"]{Prisoner’s Dilemma tutorial} for another
example of participant matching and group decisions}

@item{Consult the @secref["Conscript_Cookbook"] for more recipes and patterns}

@item{Review the reference documentation for functions like @racket[make-matchmaker],
@racket[store-my-result-in-group!], and @racket[with-study-transaction] to understand all their
options and capabilities}

]
