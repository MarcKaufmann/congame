#lang conscript

(provide
 tramline-exp11)

(defvar contribution)
(defvar/instance contributions)
(defvar counter)

(defstep (init)
  (set! counter 3)
  (skip))

(defstep (intro)
  @md{# Introduction

 In this experiment, you represent the mayor of a suburb. There are three other mayors in your group, making a total of four. These four suburbs are connected by a tram line to the capital.

 Each suburb benefits equally from the tram line, and the more funds are allocated to the tram, the greater the benefits for all suburbs. However, you must also decide how much to allocate to beautifying your own suburb’s streets.

 Each suburb has a transportation budget of 10. Your task is to decide how much of this budget to allocate to the tram line (“T”) and how much to keep for street beautification (“O”). Your suburb’s payoff is:

 **Benefit (Bᵢ) = Oᵢ + 0.4(T₁ + T₂ + T₃ + T₄)**,
 where **Oᵢ = 10 – Tᵢ.**

 After all contribution decisions have been made, all mayors are informed about every other mayor’s contributionto the tram line, and his own suburb’s resulting benefit.

 Thereafter, each mayor has another choice: they can choose to send big trucks over to the streets of any other mayor’s suburbs. Sending trucks is costly, and they destroy the streets of the suburbs they are sent to. Specifically, each mayor can invest an amount between 0 and 5 to destroy another mayor’s suburb’s streets. Say mayor A has invested X to destroy the streets of mayor B’s suburb. Then, the invested amount X is deducted from mayor A’s suburb’s benefit, of course. At the same time, the damage caused by the trucks can be quantified by three times the investment, i.e. 3 * X. As a result, the benefit of mayor B’s suburb is reduced by 3 * X.

 For example, the benefit **B₂** of Mayor 2 after all decisions have been made is:

 **B₂ = O₂ + 0.4(T₁ + T₂ + T₃ + T₄) - D₂→₁ - D₂→₃ - D₂→₄ - 3D₁→₂ - 3D₃→₂ - 3D₄→₂**

 That means that from the benefit of a mayor’s suburb we will deduct all his investments into destroying other suburbs’streets, and we will also deduct 3 times of whatever other mayors have invested into the destruction of his suburb’s streets.After decisions have been made, all mayors are informed about the destructions of their streets and the eventual benefit for their suburb

@button{Continue}})

(define instructions_template
  @md*{# Instructions
  The experiment will run over 3 different rounds.

 In each round:
 * You will interact with the same three participants throughout the experiment.
 * Each participant will decide how much of their 10 budget to allocate to the tram line. The remaining amount will automatically go to beautifying their own streets.
 * After all decisions are made, you will be informed of:
   - The total contributions to the tram line.
   - The benefit to your suburb.
 * After that you will choose whether you want to send trucks to another mayor's house with the total budget of 5.
 * Finally, you will be informed about your payoffs in this round.
 * After three rounds your total payoffs for the gane is calculated.

 Your goal is to maximize your suburb’s benefit across all rounds.})

(defstep (instructions)
  @md{@instructions_template
      @button{Continue}})

(defstep (assign-roles)
  (error 'not-implemented))

(defstep (decision)
  @md{# Contribution Decision

     How much of your 10 budget will you allocate to the tram line?

     The remaining amount will automatically go to street beautification.

     Enter the number between 0 and 10.

     @form{
      @(set! contribution (input-number #:min 0 #:max 10 "For the tram line I will allocate:"))

      @submit-button}
     @instructions_template})


(defstep (interimresults)
  ;;(define total-contributions (hash))
  ;; Total contributions to the tram line: @(~a total-contributions)
  ;; Your payoff:  @(~a (+ street-contribution (* 0.4 total-contributions)))
  (define street-contribution (- 10 contribution))
  @md{# Results before the trucks

      Your contribution to the tram line: @(~a contribution)

      Your contribution to the streets: @(~a street-contribution)

      Total contributions to the tram line:

      @button{Continue}})

(defvar trucks)

(define (input-trucks label)
  (input-number label #:min 0 #:max 5))

(defstep (truckschoice)
  ;;(define total-contributions (hash))
  ;; Total contributions to the tram line: @(~a total-contributions)
  ;; Your payoff:  @(~a (+ street-contribution (* 0.4 total-contributions)))
  (define street-contribution (- 10 contribution))
  @md{# Trucks Decision

      How much of your 5 budget will you allocate to sending trucks to other mayor's suburb's?

      @form{

            @div{
                 @set![trucks
                       (map-validator
                        (input-list
                         (list
                          (input-trucks "I will send trucks to mayor 1:")
                          (input-trucks "I will send trucks to mayor 2:")
                          (input-trucks "I will send trucks to mayor 3:")))
                        (lambda (loa)
                          (define total (apply + loa))
                          (if (<= total 5)
                              `(ok loa)
                              `(err ,@(for/list ([_ (in-list loa)])
                                        "Your answers must be within the budget of 5.")))))]}
            @submit-button}

      @instructions_template})

(defstep (results)
  ;;(define total-contributions (hash))
  ;; Total contributions to the tram line: @(~a total-contributions)
  ;; Your payoff:  @(~a (+ street-contribution (* 0.4 total-contributions)))
  @md{# Results before the trucks

      Your payoff: @(~a contribution)

      Total contributions:


      @button{Continue}})


(defstep (count)
  (set! counter (sub1 counter))
  (skip))

(defstep (finalresults)
  @md{# Thank you for participating

      What did you learn from the experiment?})

(defstep (end)
  @md{# Thank you for participating

      What did you learn from the experiment?})

(defstudy tramline-exp11
  [init --> intro
         --> instructions
         --> assign-roles
         --> decision
         --> interimresults
         --> truckschoice
         --> results
         --> count
         --> finalresults
         --> end]
  [end --> end])
