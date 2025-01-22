#lang conscript

(provide
 chicken)

(require conscript/survey-tools
         conscript/game-theory
         data/monocle
         racket/format
         racket/match
         )

(defvar/instance choices)
(defvar round_decision)
(defvar current-round)
(defvar round)
(defvar answers)

(define instructions
  @md*{# Instructions

  @style{
    table {
      width: 80%; /* Increased table width */
      margin: 20px auto;
      border-collapse: collapse;
      text-align: center;
      font-family: Arial, sans-serif;
    }
    th, td {
      border: 1px solid black;
      padding: 15px; /* Slightly more padding for better spacing */
    }
    th {
      background-color: #f0f0f0;
      font-size: 1.1rem; /* Make header text slightly larger */
    }
    caption {
      font-weight: bold;
      margin-bottom: 10px;
    }
    .driver-label {
      text-align: left;
      font-weight: bold;
    }
  }

  This experiment goes over **3 rounds**. In the beginning of **each** round, participants are randomly matched to pairs of two. The experiment is computerized. You make all your decisions at the computer.

  In each round, both participants are participating in a game of Chicken. Each participant sits in a car. Both cars are driving on a line on the street, in opposite directions onto each other. The one who goes straight and keeps the line between his tires all the time wins the contest, the one who swerves away loses face. In our experiment, both participants are at the point of time where they have to make their decision right now: swerve or go straight.

  The payoffs for the possible choices are given in the table below:

  @table{
    @thead{
      @tr{
        @th{} @th{Driver 2 Swerve} @th{Driver 2 Straight}}}
    @tbody{
      @tr{
        @th[#:class "driver-label"]{Driver 1 Swerve} 
        @td{Driver 1: E$ 0, Driver 2: E$ 0} 
        @td{Driver 1: E$ -20, Driver 2: E$ 20}}
      @tr{
        @th[#:class "driver-label"]{Driver 1 Straight} 
        @td{Driver 1: E$ 20, Driver 2: E$ -20} 
        @td{Driver 1: E$ -100, Driver 2: E$ -100}}}}

  If both swerve, none has a positive payoff. If one swerves and the other does not, the former loses face, while the latter gains reputation. If both go straight, they will definitely crash, with large costs for both.

  Your payoffs will be summed up over rounds and added to your E$ account.
})

(defstep (introduction)
  @md{@instructions

      @button{Continue}})

(define (&my-choice)
  (parameterize ([current-hash-maker hash])
    (&opt-hash-ref*
     (get-current-group)
     (current-participant-id))))

(define (make-choice! choice)
  (with-study-transaction
      (set! choices ((&my-choice) (if-undefined choices (hash)) choice))))

(defstep (waiter)
  @md{# Please Wait

 Please wait while another participant joins the queue.

 @refresh-every[5]})

(defstep matchmake
  (let ([matchmaker (make-matchmaker 2)])
    (lambda ()
      (matchmaker waiter))))

(defstep (chicken_game)
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

    # The game

    @div[#:class "form-container"]{
      @span[#:class "form-label"]{Please select your choice in this round:}
      @form{
        @(set! round_decision (select
                               '(
                                 (""  . "--Please choose an option--")
                                 ("Swerve" . "Swerve")
                                 ("Straight" . "Straight"))
                               ""))
        @submit-button
      }
    }

    @toggleable-xexpr["Show/Hide Instructions" instructions]
  })

(defstep (store-answer)
  ; This stores it so the other participant can see it.
  (make-choice! (string->symbol round_decision))
  (skip))

(define (get-choices/current-group)
  (hash-ref choices (get-current-group)))

(define (get-own-choice)
  (hash-ref (get-choices/current-group) (current-participant-id)))

(define (get-other-choice)
  (define-values (my-choice other-choice)
    (for/fold ([my-choice #f]
               [other-choice #f])
              ([(k v) (in-hash (hash-ref choices (get-current-group)))])
      (if (equal? k (current-participant-id))
          (values v other-choice)
          (values my-choice v))))
  other-choice)

(define (other-choice-made)
  (= 2 (hash-count (get-choices/current-group))))

(defstep (wait-for-choice)
  (if (not (other-choice-made))
      @md{# Please wait for the other person to make their choice

 @refresh-every[5]}
      (skip)))

(defstep (display-answer)
  (define my-choice (get-own-choice))
  (define other-choice (get-other-choice))
  (define (payout c1 c2)
    (match* (c1 c2)
      [('Swerve 'Swerve) 0]
      [('Swerve 'Straight) -20] 
      [('Straight 'Swerve) 20]
      [('Straight 'Straight) -100]))
      
  (define own-payout (payout my-choice other-choice))
  (define other-payout (payout other-choice my-choice))

  (set! current-round
        (hash 'self my-choice
              'other other-choice
              'own-payout own-payout
              'other-payout other-payout ))
  @md{# The result
      
      - Your choice: @(~a my-choice)
      - The other participant's choice: @(~a other-choice)


      You receive @(~a own-payout), the other one receives @(~a other-payout).

      @button{Continue}})

(defstep (store-round)
  (set! answers
        (cons current-round (if-undefined answers '())))
  (reset-current-group)
  (skip))

(define (rescale-score s)
  (+ (* 0.333 s) 100))

(defstep ((chicken-summary [the-end #t]))
    (define total-points
    (for/sum ([a answers])
      (hash-ref a 'own-payout))) 
  (eprintf "Answers is ~a for participant ~a~n~n" answers (current-participant-id))
  @md{# Chicken Game Summary
      
      Over the 3 rounds, you earned a total of @(~a total-points). This is equivalent to @(~r #:precision 0 (rescale-score total-points)) rescaled points.

      Here is the summary of your game:

      @`(ol
          ,@(for/list ([a (reverse answers)])
              (li (format " You chose ~a the other chose ~a, you got ~a and they got ~a"
                          (hash-ref a 'self)
                          (hash-ref a 'other)
                          (hash-ref a 'own-payout)
                          (hash-ref a 'other-payout)
                          ))))

      (if the-end? @div{} @button{Continue})})


(defstep (store-score)
  (define total-score
    (for/sum ([a answers])
      (hash-ref a 'own-payout)))
  (put/identity 'total-score (rescale-score total-score))
  (skip))

(defstudy chicken/no-admin
  [introduction --> matchmake
                --> chicken_game
                --> store-answer
                --> wait-for-choice
                --> display-answer
                --> store-round
                --> ,(lambda ()
                       (define n 3)
                       (set! round (add1 (if-undefined round 0)))
                       (if (< round n)
                           'matchmake
                           'store-score))]
  [store-score --> [the-end (chicken-summary)] --> the-end])

; FIXME: how to not duplicate all the transitions from above?
(provide
 chicken-substudy
 chicken-admin)

(defstudy chicken-substudy
  [introduction --> matchmake
                --> chicken_game
                --> store-answer
                --> wait-for-choice
                --> display-answer
                --> store-round
                --> ,(lambda ()
                       (define n 3)
                       (set! round (add1 (if-undefined round 0)))
                       (if (< round n)
                           'matchmake
                           'store-score))]
  [store-score --> [summary (chicken-summary #f)]
               --> ,(lambda ()
                      done)])

;; Admin

(require conscript/admin)

(defstep (chicken-admin)
  (define aps
    (for/fold ([r (hash)])
              ([(_ v) (in-hash (if-undefined choices (hash)))])
      (match-define (hash-table (_ a1) (_ a2)) v)
      (hash-update r (cons a1 a2) add1 0)))

  @md{# Admin

      ## Strategies Played

      @`(ul
         ,@(for/list ([(ap freq) (in-hash aps)])
             (li (format "(~a, ~a) played ~a times"
                         (car ap) (cdr ap) freq))))})

(define chicken
  (make-admin-study
   #:models `()
   #:admin chicken-admin
   chicken/no-admin))
