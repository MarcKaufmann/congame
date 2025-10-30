#lang conscript

(provide
 tramline-exp10)

(require conscript/admin
         conscript/form0
         conscript/game-theory
         conscript/survey-tools
         data/monocle
         racket/format
         racket/unit
         threading)

(with-namespace xyz.trichotomy.congame.congame-gtai.tramline-exp10
  (defvar*/instance choices/rounds)
  ; Needed due to design of conscript/game-theory, which needs FIXME.
  (defvar*/instance choices))

(defbox choices/rounds)
(defbox choices)
(define-values/invoke-unit game-theory@
  (import game-theory-vars^)
  (export game-theory^))

(defvar contribution)
(defvar counter)
(defvar payoffs)

(define n 4)
(define (rescale-score s)
  (* 2 s))

(defstep (init)
  (set! counter 3)
  (set! payoffs '())
  (with-study-transaction
    (when (undefined? choices/rounds)
      (set! choices/rounds (hash))))
  (skip))

(defstep (intro)
  @md{# Introduction
 This experiment is computerized. You make all your decisions at the computer.

 In this experiment, you represent the mayor of a suburb. There are three other mayors in your group, making a total of four. These four suburbs are connected by a tram line to the capital.

 Each suburb benefits equally from the tram line, and the more funds are allocated to the tram, the greater the benefits for all suburbs. However, you must also decide how much to allocate to beautifying your own suburb’s streets.

 Each suburb has a transportation budget of 10. Your task is to decide how much of this budget to allocate to the tram line (“T”) and how much to keep for street beautification (“O”). Your suburb’s payoff is:

 **Benefit (Bᵢ) = Oᵢ + 0.4(T₁ + T₂ + T₃ + T₄)**,
 where **Oᵢ = 10 – Tᵢ.**

@button{Continue}})

(define instructions_template
  @md*{# Instructions
  The experiment will run over 3 different rounds.

  In each round:
  * You will interact with the same three participants throughout the experiment.
  * Each participant will decide how much of their 10 budget to allocate to the tram line. The remaining amount will automatically go to beautifying their own streets.
  * After all decisions are made, you will be informed of:
    - The total contributions to the tram line.
    - The benefit to your suburb for that round.

  Your goal is to maximize your suburb’s benefit across all rounds.

  **Benefit (Bᵢ) = Oᵢ + 0.4(T₁ + T₂ + T₃ + T₄)**,
 where **Oᵢ = 10 – Tᵢ.**})

(defstep (instructions)
  @md{@instructions_template

      @button{Continue}})

(defstep (waiter)
  @md{# Please Wait

      Please wait while other participants join the queue.

      @refresh-every[5]})

(defstep matchmake
  (let ([matchmaker (make-matchmaker n)])
    (lambda ()
      (matchmaker waiter))))

(defstep (decision)
  (define-values (f on-submit)
    (form+submit
     [contribution
      (ensure
       binding/number
       (required)
       (number-in-range 0 10))]))
  (define (render rw)
    @div{@rw["contribution" @input-number{For the tram line I will allocate:}]
         @|submit-button|})
  @md{# Contribution Decision

     How much of your 10 budget will you allocate to the tram line?

     The remaining amount will automatically go to street beautification.

     Enter the number between 0 and 10.

     @form[f on-submit render]
     @toggleable-xexpr["Show/Hide Instructions" instructions_template]})

(defstep (store-choice/round!)
  (make-choice/rounds! contribution (- 4 counter))
  (skip))

(define (current-round)
  (- 4 counter))

(define (&group-round-choices)
  (parameterize ([current-hash-maker hash])
    (&opt-hash-ref* (get-current-group) (current-round))))

(define (other-choices-made)
  (= (hash-count ((&group-round-choices) choices/rounds)) n))

(defstep (wait-for-group-choices)
  (if (not (other-choices-made))
      @md{# Please wait for your group

          Please wait until the others participants have made their choice in this round.

          @refresh-every[5]}
      (skip)))

(defstep (results)
  (define round-choices
    ((&group-round-choices) choices/rounds))
  (define own-tram
    (hash-ref round-choices (current-participant-id)))
  (define own-beautification
    (- 10 own-tram))
  (define tram-contributions
    (apply + (hash-values round-choices)))
  (define own-payoff
    (+ own-beautification (* 0.4 tram-contributions)))

  (define street-contribution (- 10 contribution))
  @md{# Results for This Round

      Your contribution to the tram line: @(~a own-tram)

      Your contribution to the streets: @(~a own-beautification)

      Total contributions to the tram line: @(~a tram-contributions)

      Your payoff: @(~r #:precision 1 own-payoff)

      @button[(lambda () (set! payoffs (cons own-payoff payoffs)))]{Continue}})

(defstep (count)
  (set! counter (sub1 counter))
  (skip))

(defstep (store-score)
  (put/identity
   'total-score
   (rescale-score
    (apply + payoffs)))
  (skip))


(defstep (end)
  (define total
    (apply + payoffs))
  @md{# Thank you for participating

      Your payoffs are

      @`(ol
         ,@(for/list ([p payoffs])
             (li (~r #:precision 1 p))))

      This yields a total payoff of @(~r #:precision 1 total), which is worth a rescaled score of @(~r #:precision 1 (rescale-score total))
      })

(defstudy tramline-exp10/no-admin
  [init --> intro
        --> instructions
        --> matchmake
        --> decision
        --> store-choice/round!
        --> wait-for-group-choices
        --> results
        --> count
        --> ,(lambda ()
               (if (> counter 0)
                   'decision
                   'store-score))]
  [store-score --> end --> end])

;; Admin Page

(defstep (admin)
  (with-study-transaction
    (when (undefined? choices/rounds)
      (set! choices/rounds (hash))))
  (define round-choices
    (for/fold ([contribs (hash 1 '() 2 '() 3 '())])
              ([(_ gc) (in-hash choices/rounds)])
      (define (update i)
        (lambda (x)
          (let ([new-choice (hash-ref gc i #f)])
            (if new-choice
                (cons new-choice x)
                x))))
      (~> contribs
          (hash-update 1 (update 1))
          (hash-update 2 (update 2))
          (hash-update 3 (update 3))
          )))
  (define (avg-group-contrib r)
    (define contribs (hash-values r))
    (/ (apply + contribs) (* 1.0 (length contribs))))
  (define (display-round i)
    @md*{## Results for round @(~a i)
         @`(ul
            ,@(for/list ([r (hash-ref round-choices i)])
                (li (format "Average group contribution is ~a"
                            (~r #:precision 2 (avg-group-contrib r))))))})
  @md{# Admin

      @display-round[1]

      @display-round[2]

      @display-round[3]
      })

(define tramline-exp10
  (make-admin-study
   #:models '()
   #:admin admin
   tramline-exp10/no-admin))
