#lang conscript

; Experiment 8 from Johannes

(provide
 opera-or-football)

(require conscript/admin
         conscript/game-theory
         conscript/survey-tools
         data/monocle
         racket/match
         threading)

(with-namespace xyz.trichotomy.congame.congame-gtai.opera-football
  (defvar*/instance choices)
  (defvar*/instance choices/rounds))

(define n 2)

(define roles '(opera-fan football-fan))

(defvar/instance group-roles)
(defvar/instance group-participants)
(defvar role)
(defvar round)
(defvar answers)
(defvar current-round)
(defvar round-choice)

(define group-roles-box
  (case-lambda
     [() group-roles]
     [(v) (set! group-roles v)]))

(define group-participants-box
  (case-lambda
     [() group-participants]
     [(v) (set! group-participants v)]))

(define (set!/if-undefined l-box v)
  (with-study-transaction
    (when (undefined? (l-box))
      (l-box v))))

(define instructions
  @md*{# Instructions
 This experiment goes over **3 rounds.** In the beginning of **each** round, participants are randomly matched to pairs of two. The experiment is computerized. You make all your decisions at the computer.
 In each round, both participants in each group are randomly assigned roles. One participant is an “opera fan”, the other participant is a “football fan”. (Think of a couple.) Both make plans for tonight. The opera fan would most of all like to go to the opera (a value of E$ 10); the football fan would love to go to a football game (also a value of E$ 10). However, both would like to be at the same place rather than different ones (a value of E$ 20 for both). But they have to make their decision right now, without being able to communicate before.
 So, to repeat:
 - If the opera fan goes to the opera, he receives a value of E$ 10; if he goes to the football game, he has a value of E$ 0.
 - If the football fan goes to the opera, she receives a value of E$ 0; if she goes to the football game, she has a value of E$ 10.
 - If both end up at the same place, both receive an additional value of E$ 20; if both end up at different places, both receive no additional value.
 - In each round you are told your role and you will have to decide whether to go to the opera or to the football game.

Your payoffs will be summed up over rounds and added to your E$ account.
})

(define (init)
  (with-study-transaction
    (when (undefined? choices/rounds)
      (set! choices/rounds (hash))))
  (when (undefined? round)
    (set! round 1)))

(defstep (intro)
  (set!/if-undefined group-participants-box (hash))
  (set!/if-undefined group-roles-box (hash))
  @md{
      @instructions
      @button[init]{Continue}})

(defstep (waiter)
  @md{# Please Wait

      Please wait while the other participants join the queue...

      @refresh-every[5]})

(defstep matchmake
  (let ([matchmaker (make-matchmaker n)])
    (lambda ()
      (matchmaker waiter))))

(define (current-group-participants)
  (hash-ref group-participants (get-current-group) '()))

(defstep (register-as-group-participant)
  (with-study-transaction
    (define new-group-participants
      (cons (current-participant-id) (current-group-participants)))
    (set! group-participants
          (hash-set group-participants (get-current-group) new-group-participants)))
  (skip))

(defstep (wait-for-group-ids)
  (if (>= (length (current-group-participants)) n)
      (skip)
      @md{# Please Wait

          Please wait until your group has fully registered.

          @refresh-every[1]}))

(defstep (assign-roles)
  (with-study-transaction
    ; Unless we already have roles for the current group...
    (unless (hash-ref group-roles (get-current-group) #f)
      ; get participant ids
      (define pids
        (hash-ref group-participants (get-current-group)))
      ; create a hash (dict) where each pid is mapped to a random role
      (define assigned-roles
        (for/hash ([pid pids]
                   [r (shuffle roles)])
          (values pid r)))
      ; store this hash in group-roles under the current group
      (set! group-roles
            (hash-set group-roles (get-current-group) assigned-roles))))
  ; Set the role in a local variable for the current participant
  (set! role
        (hash-ref
         (hash-ref group-roles (get-current-group))
         (current-participant-id)))
  (skip))

(defstep (the-game)
    (define current-group-roles
    (hash-ref group-roles (get-current-group)))
 @md{
    @style{
      .form-container {
        display: flex;
        flex-direction: column;
        align-items: flex-start; /* Align everything to the left */
        margin: 20px 0;
        gap: 10px; /* Add space between rows */
        font-family: Arial, sans-serif;
      }

      .form-label {
        font-weight: bold;
      }

      select, button {
        padding: 8px 12px; /* Add padding for dropdown and button */
        font-size: 1rem;
      }

      button {
        background-color: #4a4a4a;
        color: white;
        border: none;
        border-radius: 5px;
        cursor: pointer;
      }

      button:hover {
        background-color: #6a6a6a;
      }
    }

    # The Game

    ## You are randomly matched with a new and different participant. You are the @(match role
    ['opera-fan "opera fan"]
    ['football-fan "football fan"]
    [else "Unknown participant"]).

    @div[#:class "form-container"]{
      @span[#:class "form-label"]{Please decide where you want to go:}
      @form{
        @(set! round-choice (select
                               '(("" . "-- choose --")
                                 ("Opera" . "Opera")
                                 ("Football" . "Football"))
                               ""))
        @submit-button
      }
    }

    @toggleable-xexpr["Show/Hide Instructions" instructions]
  })


(defstep (store-choices)
  ; This stores it so the other participant can see it.
  (make-choice/rounds! (string->symbol round-choice) round)
  (skip))

(define (&group-round-choices)
  (parameterize ([current-hash-maker hash])
    (&opt-hash-ref* (get-current-group) round)))

(define (other-choice-made)
  (= (hash-count ((&group-round-choices) choices/rounds)) n))

(defstep (wait-for-choice)
  (if (not (other-choice-made))
      @md{# Please wait for the other person to make their choice

 @refresh-every[5]}
      (skip)))

(defstep (display-choices)
  (define my-choice (get-own-choice/rounds round))
  (define other-choice (get-other-choice/rounds round))

  (define (base-payout role choice)
    (match (list role choice)
      [(list 'opera-fan 'Opera) 10]
      [(list 'opera-fan 'Football) 0]
      [(list 'football-fan 'Opera) 0]
      [(list 'football-fan 'Football) 10]))

  (define (bonus-payout my-choice other-choice)
    (if (equal? my-choice other-choice) 20 0))

  (define own-payout
    (+ (base-payout role my-choice)
       (bonus-payout my-choice other-choice)))

  (define other-role
    (if (eq? role 'opera-fan) 'football-fan 'opera-fan))

  (define other-payout
    (+ (base-payout other-role other-choice)
       (bonus-payout my-choice other-choice)))

  (set! current-round
        (hash 'self my-choice
              'other other-choice
              'own-payout own-payout
              'other-payout other-payout))

  @md{# Round Results

      - **Your choice:** @(~a my-choice)
      - **Other participant's choice:** @(~a other-choice)

      - **Your total payout:** @(~a own-payout)
      - **Other participant's total payout:** @(~a other-payout)

      @button{Continue}})

(defstep (store-round)
  (set! answers
        (cons current-round (if-undefined answers '())))
  (reset-current-group)
  (skip))

(defstep (the-end)
  (define total-points
    (for/sum ([a answers])
      (hash-ref a 'own-payout)))

  @md{# Final Results

      - **Total points earned over 3 rounds:** @(~a total-points)

      ## Summary of Your Game:

      @`(ul
         ,@(for/list ([a (reverse answers)])
             (li (format "Round: You chose ~a, other chose ~a → You: ~a, Other: ~a"
                         (hash-ref a 'self)
                         (hash-ref a 'other)
                         (hash-ref a 'own-payout)
                         (hash-ref a 'other-payout)))))})

; Admin

(defview (admin-view _req)
  (cond [(not (current-participant-owner?))
         @md{# Nothing to see here}]
        [else
         (with-study-transaction
           (when (undefined? choices/rounds)
             (set! choices/rounds (hash))))
         (define round-choices
           (for/fold ([c (hash 1 '() 2 '() 3 '())])
                     ([(_ gc) (in-hash choices/rounds)])
             (define (update i)
               (lambda (x)
                 (let ([new-choice (hash-ref gc i #f)])
                   (if new-choice
                       (cons new-choice x)
                       x))))
             (~> c
                 (hash-update 1 (update 1))
                 (hash-update 2 (update 2))
                 (hash-update 3 (update 3))
                 )))
         (define (count-ap i)
           (for/fold ([res (hash)])
                     ([gc (hash-ref round-choices i)])
             (hash-update res (hash-values gc) add1 0)))

         (define (display-round i)
           @md*{## Results for round @(~a i)
                @`(ul
                   ,@(for/list ([(ap n) (count-ap i)])
                       (li (format "Action profile ~a chosen ~a times."
                                   ap n))))})
         @md{# Admin

             @display-round[1]

             @display-round[2]

             @display-round[3]
             }]))

(defstep (admin)
  @md{# Admin

      @a[#:href (~current-view-uri)
         #:up-layer "new"
         #:up-target ".container"]{Overlay}})

(defstudy opera-or-football/no-admin
  [intro --> matchmake
         --> register-as-group-participant
         --> wait-for-group-ids
         --> assign-roles
         --> the-game
         --> store-choices
         --> wait-for-choice
         --> display-choices
         --> store-round
         --> ,(lambda ()
          (define n 3)
                (set! round (add1 round))
                (if (< round (add1 n))
                    'matchmake
                    'the-end))]

  [the-end --> the-end]
  [{admin (make-step
          #:view-handler admin-view
          'admin admin)} --> admin])
