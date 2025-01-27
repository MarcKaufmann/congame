#lang conscript

(provide
 volunteer-dilemma)

(require conscript/admin
         conscript/game-theory
         conscript/survey-tools
         racket/format
         racket/unit)

;(with-namespace xyz.trichotomy.congame.congame-gtai.volunteer-dilemma)

(defvar/instance choices)
(defvar/instance choices/rounds)
(defvar/instance round-of-group)
(defbox choices)
(defbox choices/rounds)
(define-values/invoke-unit game-theory@
  (import game-theory-vars^)
  (export game-theory^))

(define n 2)
(define n-rounds 3)

(defvar counter)
(defvar decision)
(defvar decisions)
(defvar payoffs)

(define (current-round)
  (add1 (- n-rounds counter)))

(defstep (init)
  (with-study-transaction
    (when (undefined? choices/rounds)
      (set! choices/rounds (hash))))
  (with-study-transaction
    (when (undefined? round-of-group)
      (set! round-of-group (hash))))
  (set! decisions '())
  (set! counter n-rounds)
  (set! payoffs '())
  (skip))

(defstep (intro)
  @md{# Introduction
 This experiment is computerized. You make all your decisions at the computer.
 
 In this experiment, you will be randomly matched into groups of three participants in each round. This means that the members of your group will change every round.

 In each round, you will make a decision about whether or not to volunteer. The outcomes are as follows:
 
 - **If at least one group member volunteers:** Each group member receives 100, but the volunteer(s) pay 40.
 - **If no one volunteers:** No one receives anything.
 
@button{Continue}})

(define instructions_template
  @md*{# Instructions
 The experiment will run over 3 different rounds. In each round:
 
 * You will be randomly assigned to a new group of three participants.
 * You will decide whether to volunteer or not. 
 * Once all group members have made their decisions, you will see the results of the round, including:
   - Whether anyone volunteered.
   - Your individual payoff for the round.})

(defstep (instructions)
  @md{@instructions_template

@button{Continue}})

(defstep (match-waiter)
  ; NOTE: We cannot wrap this in a transaction, as the entire match-waiter is
  ; wrapped in one by matchmaker.
  (unless (hash-ref round-of-group (get-current-group) #f)
    (set! round-of-group
          (hash-set
           round-of-group
           (get-current-group)
           (current-round))))
@md{# Please Wait (Round @(~a (current-round)))

    Please wait while another participant joins the queue.

    @refresh-every[5]})

(define (participant-same-round? group-id)
  (equal? (current-round)
          (hash-ref round-of-group group-id #f)))

(defstep matchmake
  (let ([matchmaker (make-matchmaker n participant-same-round?)])
    (lambda ()
      (matchmaker match-waiter))))

(defstep (decisionstep)
  @md{# Decision for This Round
     
     Will you volunteer in this round? 
     
     If you volunteer, you pay E$40 but ensure that everyone, including yourself, gets E$100.
     If you don’t volunteer, you risk no one volunteering, in which case no one gets anything.

     @form{
       @(set! decision (radios
             '(("yes" . "Yes, I will volunteer")
               ("no" . "No, I will not volunteer"))
             "Make your choice"))
       @submit-button}
     @instructions_template})

(defstep (store-result!)
  (define d (string=? decision "yes"))
  (make-choice/rounds! d (current-round))
  (set! decisions (cons d decisions))
  (skip))

(defstep (wait-for-choice)
  (define r (current-round))
  (if (= (hash-count ((&group-choices/rounds r) choices/rounds)) n)
      (skip)
      @md{# Round @|r|: Please wait for the other participants to make their choices

          @refresh-every[5]}))

(defstep (results)
  (define group-choices
    (hash-values
     ((&group-choices/rounds (current-round)) choices/rounds)))
  (define volunteers
    (apply + (map (lambda (x) (if x 1 0)) group-choices)))
  (define payoff
    (+ (if (> volunteers 0) 100 0)
       (if (string=? decision "yes") -40 0)))

  @md{# Results for This Round

      Total volunteers in your group: @(~a volunteers)
      
      Your payoff this round: @(~a payoff)
      
      @button[(λ () (set! payoffs (cons payoff payoffs)))]{Continue}})

(defstep (store-identity!)
  (define total-payoffs
    (* 0.35 (apply + payoffs)))
  (put/identity 'score total-payoffs)
  (skip))

(defstep (end)
  (define total-payoffs
    (apply + payoffs))
  @md{# Thank you for participating

      Your total payoffs are @(~a total-payoffs), which is equivalent to @(~r #:precision 0 (* 0.35 total-payoffs)) rescaled points.
      })

(defstudy volunteer-dilemma/no-admin
  [init --> intro
        --> instructions
        --> matchmake
        --> decisionstep
        --> store-result!
        --> wait-for-choice
        --> results
        --> ,(lambda ()
               (set! counter (sub1 counter))
               (reset-current-group)
               (if (> counter 0)
                   'matchmake
                   'store-identity!))]
  [store-identity! --> end --> end])

; Admin

(defstep (admin)
  (with-study-transaction
    (when (undefined? choices/rounds)
      (set! choices/rounds (hash))))
  (define (choices-in-round i)
    (for/list ([(_gid c) (in-hash choices/rounds)]
               #:when (hash-has-key? c i))
      (hash-ref c i)))
  (define (count-volunteers chs)
    (for/fold ([count (hash)])
              ([ch chs])
      (define n-volunteers
        (apply +
               (map
                (lambda (x) (if x 1 0))
                (hash-values ch))))
      (hash-update count n-volunteers add1 0)))
  @md{# Admin study

      @`(div

        ,@(for/list ([i '(1 2 3)])
            @md*{## Round @(~a i)

                 @(~a (count-volunteers (choices-in-round i)))}))})

(define volunteer-dilemma
  (make-admin-study
   #:models `()
   #:admin admin
   volunteer-dilemma/no-admin))
