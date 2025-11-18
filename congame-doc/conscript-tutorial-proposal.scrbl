#lang scribble/manual

@(require (for-label conscript/base
                     conscript/form0
                     conscript/survey-tools
                     conscript/matchmaking
                     racket/contract
                     racket/list
                     racket/match)
          "doc-util.rkt")

@title[#:style 'quiet #:tag "proposal-choice-tutorial"]{Tutorial: Proposal Choice Study}

This tutorial walks through a multi-participant study that demonstrates how to create different
experiences for participants in different roles. The study implements an economic experiment where
three participants propose offers to a fourth participant, who then chooses whether to accept.

You'll learn how to:

@itemlist[

@item{Assign participants to different roles within the same study}

@item{Show completely different interfaces to different participants based on their role}

@item{Implement control and treatment groups with different decision-making processes}

@item{Coordinate information flow between participants (what each participant sees vs. reality)}

]

This tutorial builds on concepts from @secref["multi-participant-tutorial"]. If you haven't worked
through that tutorial yet, we recommend doing so first, as this tutorial assumes familiarity with
basic matchmaking, instance-scoped variables, and group result storage.

@bold{To follow along:} You can find the complete code for this study at
@github-link{congame-example-study/proposal-choice.rkt}.

@;===============================================

@section[#:tag "pc-overview"]{Study Overview}

In this study, participants are placed into groups of 4:

@itemlist[

@item{@bold{3 Proposers}: Each makes two offers — an @italic{actual offer} (the real amount they
would give) and a @italic{communicated offer} (what the Responder sees). These values can differ,
allowing Proposers to potentially mislead the Responder.}

@item{@bold{1 Responder}: Sees only the communicated offers from all three Proposers and decides
whether to accept or reject.}

]

The Responder is randomly assigned to one of two conditions:

@itemlist[

@item{@bold{Control group}: If they accept, a random Proposer is selected for them}

@item{@bold{Treatment group}: If they accept, they choose which specific Proposer to pair with}

]

If the Responder accepts (and a Proposer is selected/chosen), the Responder receives the
@italic{actual offer} from that Proposer, which may differ from what was communicated.

@;===============================================

@section[#:tag "pc-preamble"]{Preamble}

@codeblock|{
#lang conscript

(require conscript/form0
         conscript/survey-tools
         racket/list
         racket/match)

(provide proposal-choice)
}|

As always we begin with @code{#lang conscript}, @racket[require] the additional modules we'll need,
and @racket[provide] the name of our study. The comment block at the top serves as helpful
documentation for anyone reading the code.

We require @racketmodname[racket/list] for @racket[shuffle], and @racketmodname[racket/match] for
@racket[match-define] and @racket[match*], which we'll use to extract values from complex data
structures.

@;===============================================

@section[#:tag "pc-instance-vars"]{Instance-Scoped Variables and Role Assignment}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
;; =============================================================================
;; INSTANCE-SCOPED VARIABLES, ROLE ASSIGNMENT
;; =============================================================================

(defvar/instance group-roles)            ; (shuffle '(proposer proposer proposer responder))
(defvar/instance responder-treatments)   ; list of #t/#f for treatment assignment
(defvar role)                            ; 'proposer or 'responder

(defstep (assign-roles)
  ; Balanced assignment of 3 proposers and 1 responder
  (with-study-transaction
      (when (or (undefined? group-roles) (null? group-roles))
        (set! group-roles (shuffle '(proposer proposer proposer responder))))
    (set! role (first group-roles))
    (set! group-roles (rest group-roles)))
  (skip))
}|

We use two types of variable scopes here:

@itemlist[

@item{@bold{Instance-scoped variables} (@racket[group-roles], @racket[responder-treatments]): These
are shared across all participants in the study instance. We use them to coordinate role assignment
and treatment assignment.}

@item{@bold{Participant-scoped variables} (@racket[role]): Stores a unique value for each
participant.}

]

The @racket[assign-roles] step assigns each participant to either the @racket['proposer] or
@racket['responder] role:

@itemlist[#:style 'ordered

@item{The first participant to reach this step creates a shuffled list of roles:
@racket['(proposer proposer proposer responder)]}

@item{Each participant takes the first role from the list and stores it in their @racket[role]
variable}

@item{The role is removed from the shared list, so the next participant gets the next role}

]

The @racket[with-study-transaction] ensures that even if multiple participants reach this step
simultaneously, they each get a unique role without conflicts.

@;===============================================

@section[#:tag "pc-participant-vars"]{Participant-Scoped Variables}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
;; =============================================================================
;; PARTICIPANT-SCOPED VARIABLES
;; =============================================================================

(defvar is-responder-treatment?)        ; #t or #f (only for responders)
(defvar actual-offer)                   ; number 0-5
(defvar communicated-offer)             ; number 0-5
(defvar responder-decision)             ; 'accept or 'reject
(defvar chosen-proposer-id)             ; participant id or #f
(defvar outcome)                        ; string describing outcome
}|

These variables store data that is unique to each participant. All participants will have these
variables, though not all will use them (for example, only Responders will use
@racket[is-responder-treatment?], and only Proposers will use @racket[actual-offer] and
@racket[communicated-offer]).

@;===============================================

@section[#:tag "pc-instructions"]{Instructions}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
;; =============================================================================
;; INSTRUCTIONS
;; =============================================================================

(defstep (instructions)
  @md{# Proposal Choice Study

    Welcome! In this study, you will be placed in a group of 4 participants.

    Three participants will be **Proposers** and one will be a **Responder**.

    ## How it works:

    **Proposers** will make two types of offers:
    - An **actual offer**: the real amount they would give to the Responder (0-5)
    - A **communicated offer**: the amount they tell the Responder (0-5)

    These two values can differ.

    **Responders** will see only the communicated offers from all three Proposers.
    They can then either accept or reject the offers.

    The specific pairing mechanism will be explained to you based on your assigned role.

    @button{Continue}})
}|

This is a straightforward instructions step that explains the study to all participants before they
are assigned to roles. The instructions are intentionally general since participants don't yet know
whether they will be Proposers or Responders.

@;===============================================

@section[#:tag "pc-matchmaking"]{Matchmaking}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
;; =============================================================================
;; MATCHMAKING
;; =============================================================================

(defstep (waiter)
  @md{# Please Wait

    Please wait while other participants join your group...

    @refresh-every[5]})

(define matchmaker (make-matchmaker 4))

(defstep (matchmake)
  (matchmaker waiter))
}|

The matchmaking section creates groups of 4 participants. We define a @racket[waiter] step that
displays while participants are waiting, then use @racket[make-matchmaker] to create a matchmaker
function for groups of 4. The @racket[matchmake] step calls this function, passing it the
@racket[waiter] step to display while waiting.

When all 4 participants are matched into a group, they automatically proceed to the next step.

@;===============================================

@section[#:tag "pc-treatment-assignment"]{Responder Treatment Assignment}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
;; =============================================================================
;; RESPONDER TREATMENT ASSIGNMENT
;; =============================================================================

(defstep (assign-responder-treatment)
  ; Balanced: out of every 2 responders, assign 1 to treatment, 1 to control
  (with-study-transaction
    (when (or (undefined? responder-treatments) (null? responder-treatments))
      (set! responder-treatments (shuffle '(#t #f))))
    (set! is-responder-treatment? (first responder-treatments))
    (set! responder-treatments (rest responder-treatments)))
  (skip))
}|

This step uses the same pattern as role assignment, but only affects Responders. Out of every 2
Responders, one is assigned to treatment (@racket[#t]) and one to control (@racket[#f]). This
ensures balanced assignment across the treatment conditions.

@inline-note{This step will be reached by all participants in the study flow, but only Responders
will actually use the @racket[is-responder-treatment?] value. Proposers will have this variable set
but will never reference it.}

@;===============================================

@section[#:tag "pc-proposer-offers"]{Proposer: Make Offers and Await Responder Choice}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
;; =============================================================================
;; PROPOSER: MAKE OFFERS AND AWAIT RESPONDER CHOICE
;; =============================================================================

(define-values (offers-form offers-onsubmit)
  (form+submit
   [actual-offer (ensure binding/number (range/inclusive 0 5) (required))]
   [communicated-offer (ensure binding/number (range/inclusive 0 5) (required))]))

(define (render-offers-form rw)
  @md*{
    @rw["actual-offer" @input-number{Actual offer (the real amount you will give): }]

    @rw["communicated-offer" @input-number{Communicated offer (what the Responder sees): }]

    @|submit-button|})

(defstep (proposer-make-offers)
  @md{# Proposer: Make Your Offers

    You are a **Proposer**.

    Please choose two numbers between 0 and 5:

    1. **Actual offer**: The real amount you would give to the Responder if chosen
    2. **Communicated offer**: The amount you tell the Responder you will give

    These two values can differ. The Responder will only see your communicated offer.

    @form[offers-form offers-onsubmit render-offers-form]})

(defstep (store-proposer-offers)
  (store-my-result-in-group! 'offer (list actual-offer communicated-offer))
  (skip))

(defstep (proposer-wait-for-responder)
  (define decision (filter values (current-group-member-results 'decision)))

  (if (and decision (not (null? decision)))
      (skip)
      @md{# Waiting for Responder

        You have submitted your offers. Please wait while the Responder makes their decision...

        @refresh-every[5]}))
}|

This section handles the Proposer experience. Let's break it down:

@subsection[#:tag "pc-offers-form"]{The Offers Form}

We use @racket[form+submit] to create a form that collects both offers. Both fields use
@racket[binding/number] with @racket[range/inclusive] to constrain the values to 0-5, and both are
@racket[required].

The @racket[render-offers-form] function uses @racket[md*] (note the asterisk) to create reusable
content that can be embedded within other content.

@subsection[#:tag "pc-proposer-steps"]{Proposer Steps}

@itemlist[

@item{@racket[proposer-make-offers]: Displays the form to collect the two offers from the Proposer}

@item{@racket[store-proposer-offers]: Stores both offers as a list under the key @racket['offer]
using @racket[store-my-result-in-group!], making them accessible to other group members}

@item{@racket[proposer-wait-for-responder]: Checks if the Responder has made their decision yet. If
so, it skips to the next step. If not, it displays a waiting page that refreshes every 5 seconds.
The @racket[filter] and @racket[values] are used to remove any @racket[#f] values from the list
before checking if it's non-empty.}

]

@;===============================================

@section[#:tag "pc-responder-wait"]{Responder: View Offers and Make Decision}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
;; =============================================================================
;; RESPONDER: VIEW OFFERS AND MAKE DECISION
;; =============================================================================

(defstep (responder-wait-for-offers)
  ; Check if all 3 proposers have submitted (we're the 4th member, so count should be 3)
  (if (= 3 (current-group-results-count 'offer))
      (skip)
      @md{# Responder: Waiting for Proposers

        You are assigned to be the **Responder**.

        Please wait while all Proposers submit their offers...

        @refresh-every[3]}))

; Helper to get sorted list of (pid communicated-offer) pairs
(define (get-offers-with-pids)
  (define offers (current-group-member-results 'offer #:include-ids? #t))
  ;(sort offers < #:key car) ; We could sort offers, perhaps introducing bias
  offers)
}|

The @racket[responder-wait-for-offers] step checks if all 3 Proposers have submitted their offers
by counting the number of results stored under the @racket['offer] key. If the count is 3, it skips
to the next step; otherwise, it displays a waiting page.

The helper function @racket[get-offers-with-pids] retrieves the offers from all group members along
with their participant IDs. The @racket[#:include-ids? #t] argument makes
@racket[current-group-member-results] return a list of @racket[(cons _pid _result)] pairs. Note the
commented-out line about sorting — the study deliberately doesn't sort the offers to avoid
introducing bias.

@subsection[#:tag "pc-control-interface"]{Control Group Interface}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (responder-view-offers-control)
  (define offers (get-offers-with-pids))

  (define (accept)
    (set! responder-decision 'accept))

  (define (reject)
    (set! responder-decision 'reject))

  (match-define (list (cons pid1 (list _ comm1))
                      (cons pid2 (list _ comm2))
                      (cons pid3 (list _ comm3))) offers)

  @md{# Responder: View Offers (Control Group)

    You are in the **Control Group**.

    Here are the communicated offers from the three Proposers:

    - **Proposer 1:** @~a[comm1]
    - **Proposer 2:** @~a[comm2]
    - **Proposer 3:** @~a[comm3]

    You can either:
    - **Accept**: A random Proposer will be selected for you
    - **Reject all**: End the game with no pairing

    @button[accept]{Accept one (Random Selection)}
    @button[reject]{Reject All Offers}})
}|

Let's break down what's happening here:

@itemlist[#:style 'ordered

@item{@racket[get-offers-with-pids] retrieves the offers. Each offer is a @racket[cons] pair:
@racket[(cons _pid (list _actual _communicated))]}

@item{@racket[match-define] destructures this list. We extract the communicated offers into
@racket[comm1], @racket[comm2], and @racket[comm3], using @racket[_] to ignore the actual offers
and the participant IDs (though we do capture the PIDs as @racket[pid1], @racket[pid2], and
@racket[pid3] — they just aren't used in the control interface)}

@item{We define two button handler functions: @racket[accept] and @racket[reject], each setting
@racket[responder-decision] appropriately}

@item{The control group interface shows all three communicated offers but only provides two buttons:
accept (with random selection) or reject all}

]

@subsection[#:tag "pc-treatment-interface"]{Treatment Group Interface}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (responder-view-offers-treatment)
  (define offers (get-offers-with-pids))

  (match-define (list (cons pid1 (list _ comm1))
                      (cons pid2 (list _ comm2))
                      (cons pid3 (list _ comm3))) offers)

  (define (choose1)
    (set! responder-decision 'accept)
    (set! chosen-proposer-id pid1))

  (define (choose2)
    (set! responder-decision 'accept)
    (set! chosen-proposer-id pid2))

  (define (choose3)
    (set! responder-decision 'accept)
    (set! chosen-proposer-id pid3))

  (define (reject)
    (set! responder-decision 'reject)
    (set! chosen-proposer-id #f))

  @md{# Responder: View Offers (Treatment Group)

    You are in the **Treatment Group**.

    Here are the communicated offers from the three Proposers:

    - **Proposer 1:** @~a[comm1]
    - **Proposer 2:** @~a[comm2]
    - **Proposer 3:** @~a[comm3]

    You can either:
    - **Choose** one specific Proposer to pair with
    - **Reject all** offers

    @button[choose1]{Choose Proposer 1}
    @button[choose2]{Choose Proposer 2}
    @button[choose3]{Choose Proposer 3}
    @button[reject]{Reject All Offers}})
}|

The treatment group interface follows a similar pattern, but with a crucial difference: instead of
accepting with random selection, the Responder gets four buttons — one for each Proposer, plus one
to reject all. Each "choose" button stores which Proposer was selected in
@racket[chosen-proposer-id] by capturing the corresponding participant ID (@racket[pid1],
@racket[pid2], or @racket[pid3]).

@subsection[#:tag "pc-storing-decision"]{Storing the Responder's Decision}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (store-responder-decision)
  ; If control group and accepted, choose a random proposer
  (when (and (equal? responder-decision 'accept)
             (not is-responder-treatment?))
    (define offers (get-offers-with-pids))
    (define random-proposer (car (list-ref offers (random 0 3))))
    (set! chosen-proposer-id random-proposer))

  ; Store the decision for all group members to see
  (store-my-result-in-group! 'decision (list responder-decision chosen-proposer-id))
  (skip))
}|

After the Responder makes their choice, this step handles the control group's random selection. If
the Responder is in the control group and accepted, we randomly select one of the three Proposers:

@itemlist[#:style 'ordered

@item{Get the list of offers (with participant IDs)}

@item{Use @racket[(random 0 3)] to generate a random index (0, 1, or 2)}

@item{Use @racket[list-ref] to get the offer at that index, then @racket[car] to extract the
participant ID from the @racket[cons] pair}

]

Finally, we store both the decision and the chosen Proposer ID using
@racket[store-my-result-in-group!] so all group members can access it.

@;===============================================

@section[#:tag "pc-outcomes"]{Outcomes}

@subsection[#:tag "pc-proposer-outcomes"]{Proposer Outcomes}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
;; =============================================================================
;; OUTCOMES
;; =============================================================================

(defstep (show-outcome-proposer)
  (define decision-data (first (filter values (current-group-member-results 'decision))))
  (define decision (first decision-data))
  (define chosen-pid (second decision-data))

  (cond
    [(equal? decision 'reject)
     (set! outcome "The Responder rejected all offers. You were not chosen.")
     @md{# Outcome

       **The Responder rejected all offers.**

       You were not chosen.

       The game has ended.}]

    [(equal? chosen-pid (current-participant-id))
     (set! outcome (format "You were chosen! You gave ~a to the Responder (after communicating ~a)."
                           actual-offer communicated-offer))
     @md{# Outcome

       **You were chosen by the Responder!**

       Your communicated offer was **@~a[communicated-offer]**.

       Your actual offer was **@~a[actual-offer]**.

       You have given @~a[actual-offer] to the Responder.}]

    [else
     (set! outcome "You were not chosen. The Responder chose another Proposer.")
     @md{# Outcome

       **You were not chosen.**

       The Responder chose a different Proposer.

       The game has ended.}]))
}|

The Proposer outcome step retrieves the Responder's decision from group results. The @racket[filter]
with @racket[values] removes any @racket[#f] values (the Proposers do not record a response for
@racket['decision] so @racket[current-group-member-results] will return a @racket[#f] value in the
list for each Proposer).

The step uses @racket[cond] to handle three cases:

@itemlist[

@item{The Responder rejected all offers}

@item{This Proposer was chosen (determined by comparing @racket[chosen-pid] with the current
participant's ID)}

@item{Another Proposer was chosen}

]

When chosen, the Proposer sees both offers to understand what they communicated versus what they
actually gave.

@subsection[#:tag "pc-responder-outcomes"]{Responder Outcomes}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (show-outcome-responder)
  (define decision-data (first (filter values (current-group-member-results 'decision #:include-self? #t))))
  (define decision (first decision-data))
  (define chosen-pid (second decision-data))

  (cond
    [(equal? decision 'reject)
     (set! outcome "You rejected all offers.")
     @md{# Outcome

       You chose to **reject all offers**. What a bummer.

       The game has ended.}]

    [else
     ; Get the chosen proposer's offer
     (define all-offers (get-offers-with-pids))
     (match-define (list actual communicated)
       (for/or ([offer (in-list all-offers)])
         (match-define (list pid act comm) offer)
         (and (= pid chosen-pid) (list act comm))))
     (set! outcome (format "You received ~a (communicated offer was ~a)." actual communicated))
     @md{# Outcome

       You accepted and were paired with Proposer #@~a[chosen-pid].

       The **communicated offer** was **@~a[communicated]**.

       The **actual offer** was **@~a[actual]**.

       You received **@~a[actual]**.}]))
}|

The Responder outcome step also retrieves the decision data, but in this case it's retrieving the
Responder's own decision (since the current participant is the Responder).

If the Responder accepted, we need to find the chosen Proposer's offers. We use @racket[for/or] to
loop through all offers until we find the one matching @racket[chosen-pid]. The @racket[for/or]
form returns the first non-@racket[#f] value it encounters:

@itemlist[

@item{For each offer, we destructure it using @racket[match-define] to get the participant ID,
actual offer, and communicated offer}

@item{If the participant ID matches @racket[chosen-pid], we return a list containing the actual and
communicated offers}

@item{Otherwise, the @racket[and] expression returns @racket[#f] and the loop continues}

]

We then use another @racket[match-define] to extract the actual and communicated offers from the
result, and display both values to show the Responder what they were promised versus what they
actually received.

@;===============================================

@section[#:tag "pc-study-flow"]{Main Study Flow}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
;; =============================================================================
;; MAIN STUDY FLOW
;; =============================================================================

(defstudy proposal-choice
  [instructions
   --> matchmake
   --> assign-roles
   --> ,(lambda ()
          (if (equal? role 'responder)
              'assign-responder-treatment
              'proposer-make-offers))]

  ; Responder path
  [assign-responder-treatment
   --> responder-wait-for-offers
   --> ,(lambda ()
          (if is-responder-treatment?
              'responder-view-offers-treatment
              'responder-view-offers-control))]

  [responder-view-offers-control
   --> store-responder-decision
   --> show-outcome-responder]

  [responder-view-offers-treatment
   --> store-responder-decision
   --> show-outcome-responder]

  ; Proposer path
  [proposer-make-offers
   --> store-proposer-offers
   --> proposer-wait-for-responder
   --> show-outcome-proposer]

  [show-outcome-proposer --> show-outcome-proposer]
  [show-outcome-responder --> show-outcome-responder])
}|

The transition graph uses inline @racket[lambda] functions to route participants down different paths
based on their role and treatment assignment. All participants start with the same sequence:
@racket[instructions], @racket[matchmake], and @racket[assign-roles]. After role assignment, the
first conditional transition checks @racket[(equal? role 'responder)] to send Responders to
@racket['assign-responder-treatment] and Proposers to @racket['proposer-make-offers].

Responders then encounter a second conditional transition that checks
@racket[is-responder-treatment?] to route them to either @racket['responder-view-offers-treatment]
or @racket['responder-view-offers-control]. Both responder paths converge at
@racket[store-responder-decision] and end at @racket[show-outcome-responder]. Meanwhile, Proposers
follow their own linear path through making offers, storing them, waiting for the Responder's
decision, and seeing their outcome.

The final two transitions (@racket[[show-outcome-proposer --> show-outcome-proposer]] and
@racket[[show-outcome-responder --> show-outcome-responder]]) make these terminal steps where
participants remain once the study is complete.

@;===============================================

@section[#:tag "pc-recap"]{Key Concepts Recap}

This tutorial demonstrated several advanced patterns for multi-participant studies:

@itemlist[

@item{@bold{Role-based experiences}: Using instance-scoped variables and conditional transitions to
assign participants to different roles with completely different interfaces}

@item{@bold{Conditional UI rendering}: Creating multiple step definitions for different conditions
(control vs. treatment) and routing participants to the appropriate version}

@item{@bold{Information asymmetry}: Storing multiple values (actual vs. communicated offers) and
selectively revealing different information to different participants}

@item{@bold{Pattern matching with @racket[match-define]}: Extracting values from complex nested
data structures returned by group result functions}

@item{@bold{Conditional data manipulation}: Implementing random selection in code (control group)
versus user selection (treatment group) before storing the final decision}

@item{@bold{Coordination through waiting steps}: Using @racket[current-group-results-count] and
@racket[refresh-every] to create waiting steps that poll until all participants are ready}

]

@;===============================================

@section[#:tag "pc-next-steps"]{Next Steps}

This study demonstrates how to create rich, multi-role experiments with different experiences for
different participants. You might want to explore:

@itemlist[

@item{Add multiple rounds to the study, allowing participants to learn and adapt their strategies
over time}

@item{Implement feedback systems where participants can rate or comment on their experience}

@item{Consult the @secref["Conscript_Cookbook"] for more recipes and patterns for common study
tasks}

]
