#lang conscript

;; Proposal Choice Study
;;
;; Groups of 4 participants:
;;   - 3 Proposers
;;   - 1 Responder
;;
;; Proposers choose actual offer (real split) and communicated offer (what Responder sees)
;; Responder sees communicated offers and decides to accept or reject
;;
;; Treatment Assignment (for Responders):
;;   - Balanced randomization: 1 control, 1 treatment per 2 responders
;;
;; Control: Responder rejects all (end) OR accepts -> random Proposer selected
;; Treatment: Responder rejects all (end) OR chooses which Proposer

(require conscript/form0
         conscript/survey-tools
         racket/list
         racket/match)

(provide proposal-choice)

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

;; =============================================================================
;; PARTICIPANT-SCOPED VARIABLES
;; =============================================================================

(defvar is-responder-treatment?)        ; #t or #f (only for responders)
(defvar actual-offer)                   ; number 0-5
(defvar communicated-offer)             ; number 0-5
(defvar responder-decision)             ; 'accept or 'reject
(defvar chosen-proposer-id)             ; participant id or #f
(defvar outcome)                        ; string describing outcome

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
