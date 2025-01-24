#lang conscript

; Experiment 8 from Johannes

(provide
 opera-or-football)

(require conscript/admin
         conscript/survey-tools
         data/monocle
         racket/match
         threading)

(with-namespace xyz.trichotomy.congame.congame-gtai.opera-football
  (defvar*/instance choices/rounds))

(define n 2)

(define roles '(opera-fan football-fan))

(defvar/instance group-roles)
(defvar/instance group-participants)
(defvar role)
(defvar round)
(defvar next-round)
(defvar enter-transitions)
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
 This experiment goes over **5 rounds.** In the beginning of **each** round, participants are randomly matched to pairs of two. The experiment is computerized. You make all your decisions at the computer.
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
    (set! round 1))
  (when (undefined? next-round)
    (set! next-round 1))
  (when (undefined? enter-transitions)
    (set! enter-transitions '())))

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
      (log-conscript-warning "entering matchmake with round ~a for participant ~a" round (current-participant-id))
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
  (set! next-round (add1 round))
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
      @form[#:bot ([test (#:round-choice "Opera")])]{
        @(set! round-choice (input-text "Bla")
               #;(select
                               '(("" . "-- choose --")
                                 ("Opera" . "Opera")
                                 ("Football" . "Football"))
                               ""))
        @submit-button
      }
    }

    @toggleable-xexpr["Show/Hide Instructions" instructions]
  })

(define (autofill-form)
  (bot:autofill 'test))

(define (&my-choice/rounds r)
  (parameterize ([current-hash-maker hash])
    (&opt-hash-ref*
     (get-current-group)
     r
     (current-participant-id))))

(define (get-own-choice/rounds r)
  ((&my-choice/rounds r) choices/rounds))

(define (make-choice/rounds! choice r)
  (with-study-transaction
    (set! choices/rounds ((&my-choice/rounds r) choices/rounds choice))))

(define (&my-group/rounds r)
  (parameterize ([current-hash-maker hash])
    (&opt-hash-ref*
     (get-current-group)
     r)))

; This assumes that there are two people, but doesn't check for it. Will return some random other participant as 'other' otherwise.
(define (get-other-choice/rounds r)
  (define-values (_my-choice other-choice)
    (for/fold ([my-choice #f]
               [other-choice #f])
              ([(k v) (in-hash ((&my-group/rounds r) choices/rounds))])
      (if (equal? k (current-participant-id))
          (values v other-choice)
          (values my-choice v))))
  other-choice)

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
  (skip))

(defstep (the-end)
  (define total-points
    (for/sum ([a answers])
      (hash-ref a 'own-payout)))

  @md{# Final Results

      - **Total points (equal rescaled points) earned over 5 rounds:** @(~a total-points)

      ## Summary of Your Game:

      @`(ul
         ,@(for/list ([a (reverse answers)])
             (li (format "Round: You chose ~a, other chose ~a → You: ~a, Other: ~a"
                         (hash-ref a 'self)
                         (hash-ref a 'other)
                         (hash-ref a 'own-payout)
                         (hash-ref a 'other-payout)))))})

(defstep (store-score)
  (define total-points
    (for/sum ([a answers])
      (hash-ref a 'own-payout)))
  (put/identity 'score total-points)
  (skip))

(defstudy opera-or-football/no-admin
  [intro --> matchmake
         --> register-as-group-participant
         --> wait-for-group-ids
         --> assign-roles
         --> {the-game (with-bot the-game autofill-form)}
         --> store-choices
         --> wait-for-choice
         --> display-choices
         --> store-round
         --> ,(lambda ()
                (define n 5)
                (log-conscript-warning "transition: round at start is ~a for participant ~a~n" round (current-participant-id))
                (set! round next-round)
                (set! enter-transitions
                      (cons next-round enter-transitions))
                (log-conscript-warning "transition: round at end is ~a for participant ~a~n" round (current-participant-id))
                (if (< round (add1 n))
                    'matchmake
                    'store-score))]

  [store-score --> the-end --> the-end])

;; Bot

(define (opera-model k proc)
  (match k
    [`(*root* the-end)
     (bot:completer)]
    [`(*root* ,(or 'matchmake 'wait-for-choice 'wait-for-group-ids 'assign-roles))
     (sleep 1)]
    [_ (proc)]))

; Admin

(defstep (admin)
  (with-study-transaction
    (when (undefined? choices/rounds)
      (set! choices/rounds (hash))))
  (define round-choices
    (for/fold ([c (hash 1 '() 2 '() 3 '() 4 '() 5 '())])
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
          (hash-update 4 (update 4))
          (hash-update 5 (update 5))
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

      @display-round[4]

      @display-round[5]

      ## Bots

      @button[#:to-step-id 'bots]{Manage Bots}
      })

(define opera-or-football
  (make-admin-study
   opera-or-football/no-admin
   #:models `((opera-or-football . ,opera-model))
   #:admin admin))
