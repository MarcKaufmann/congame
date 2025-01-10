#lang conscript

; Questions and games I would like to play:
; - Comprehension questions about the grade game with various other preferences.
; - Find the dominant strategies for some games
; - Find the remaining options after iterated deletion
; - Exercise 16.1
; - Exercise 27.1
; - Pollak Iteration (problem 2 on PS1)
; - Pollak PS1, 3 a) and b)
; - Exercise 31.1
; - I could give them a bunch of 2x2 and 2x3 or 3x3 games to play, simply based on the payoff matrix
; - Splitting the dollar?
; - Find mixed strategy equilibria
; - Provide a strategy that will play against other's in the class - i.e., I will run this strategy against others.
; - Autograde th PSes of Johannes! Sometimes just yes/no, sometimes with more intermediate questions. Start easy for me (simply "yes/no" or the numbers that give the Nash Equilibrium) then add more subgrading and explanations/help. Have Dilnovoz augment it.
; - Expert mixed equilibrium question
; - Auctions: second-price auction, find all equilibria, or at least some other equilibria.
;   - Provide various equilibria satisfying some properties.

; The problems are taken from Johannes Hoelzemann(JH)'s tutorials, which are mostly taken from Introduction to Game Theory (IGT) by Martin J. Osborne.

; Problem 1 of Tutorial 1

(require conscript/game-theory
         racket/match
         hash-view)

(provide
 assignment1)

(hash-view problem (description form answer score))

(with-namespace xyz.trichotomy.congame-gtai.assignment1
  (defvar* p1t1/q1)
  (defvar* p1t1/q2)
  (defvar* p1t1/q3)
  (defvar* test)
  (defvar* index))

(define (yes/no)
  (radios '(("yes" . "Yes")
            ("no"  . "No"))
          ""))

(define problem1/tutorial1
  (problem
    @md*{Two animals are fighting over some prey. Each can be passive or aggressive. Each prefers to be aggressive if its opponent is passive, and passive if its opponent is aggressive; given its own stance, it prefers the outcome in which its opponent is passive to that in which its opponent is aggressive.

         1. Formulate this situation as a strategic game.
         2. If you call the aggressive action Fink and the passive action Quiet, is the resulting game the same as the Prisoner’s Dilemma? (That is, are the players’ preferences in the resulting game the same as their preferences in the Prisoner’s Dilemma?) If not, how do they differ?
         3. Find the pure-strategy Nash equilibria (if any) of the game.}

    (lambda ()
      @form{
        @div{
          1. Formulate this situation as a strategic game.

          @set![p1t1/q1 (input-list
                         (list
                          @input-number{Payoff to player 1 from "Fink-Fink"}
                          @input-number{Payoff to player 1 from "Fink-Quiet"}
                          @input-number{Payoff to player 1 from "Quiet-Fink"}
                          @input-number{Payoff to player 1 from "Quiet-Quiet"}))]}


        @div{
          2. Is this game the same as the Prisoner's Dilemma?

          @set![p1t1/q2 (yes/no)]}

        @div{
          3. Which of the following are pure-strategy Nash equilibria?
          @set![p1t1/q3 (input-list
                         (list
                          @checkbox[#:required? #f]{Fink-Fink}
                          @checkbox[#:required? #f]{Fink-Quiet}
                          @checkbox[#:required? #f]{Quiet-Fink}
                          @checkbox[#:required? #f]{Quiet-Quiet}))]}

        @submit-button
       })

    (lambda ()
      @md*{
        1. What are payoffs consistent with that game?

        Your answer:

        - "Fink-Fink": @(~a (first p1t1/q1))
        - "Fink-Quiet": @(~a (second p1t1/q1))
        - "Quiet-Fink": @(~a (third p1t1/q1))
        - "Quiet-Quiet": @(~a (fourth p1t1/q1))

        2. Is this game the same as the Prisoner's Dilemma?

        Your answer: @(~a p1t1/q2)

        3. Which of the following are pure-strategy Nash equilibria?

        Your answer:

        @`(ul
           ,@(for/list ([answer p1t1/q3]
                        [ap '("Fink-Fink" "Fink-Quiet" "Quiet-Fink" "Quiet-Quiet")])
               (li (format "~a: ~a" answer ap))))

      })

    (list
     (lambda ()
       (match-define (list a b c d)
         p1t1/q1)
       (+ (if (and (> c a) (> b d)) 50 0)
          (if (and (> b a) (> d c)) 50 0)))

     (lambda ()
       (if (string=? p1t1/q2 "no") 100 0))

     (lambda ()
       (for/sum ([answer p1t1/q3]
                 [correct-answer '(#f #t #t #f)])
         (if (equal? answer correct-answer) 25 -25)))
     )))


(define problem2/tutorial1
  (problem

    @md*{Two people enter a bus. Two adjacent cramped seats are free. Each person must decide whether to sit or stand. Sitting alone is more comfortable than sitting next to the other person, which is more comfortable than standing.

         1. Suppose that each person cares only about her own comfort. Model the situation as a strategic game. Is this game the same as the Prisoner’s Dilemma (except for the names of the actions)? Find its Nash equilibrium (equilibria?).
         2. Suppose that each person is altruistic, ranking the outcomes according to the other person’s comfort, but, out of politeness, prefers to stand than to sit if the other person stands. Model the situation as a strategic game. Is this game the Prisoner’s Dilemma (except for the names of the actions)? Find its Nash equilibrium (equilibria?).
         3. Compare the people’s comfort in the equilibria of the two games.
        }

    (lambda ()
      @form{

        @set![test @input-text{Something}]

        @submit-button
      })

    (lambda ()
      @md*{Your answers})

    (list
     (lambda () 0))))

(define problems
  (vector
   problem1/tutorial1
   problem2/tutorial1))

(defstep (overview)
  @md{# Assignment 1

      @`(div

         ,@(for/list ([p problems]
                      [i (in-range 1 (add1 (vector-length problems)))])
             @md*{## Problem @(~a i)

                  @(problem-description p)

                  @button[(λ () (set! index i))]{
                    @(format "Submit Answers to Problem ~a" i)
                  }
                 }))

      @button[#:to-step-id 'grade-submission]{Finalize your Submission}
      })

(defstep (submission)
  (define p
    (vector-ref problems (sub1 index)))
  @md{# Problem @(~a index)

      ## Description

      @(problem-description p)

      ## Submit Answer

      @((problem-form p))})

(defstep (overview-after-submission)
  @md{# Assignment 1

      @button[#:to-step-id 'overview]{Reopen Assignment to Change Your Submission}

      # Your Submission

      @`(div

         ,@(for/list ([p problems]
                      [i (in-range 1 (add1 (vector-length problems)))])
             @md*{## Problem @(~a i)

                  @(problem-description p)

                  @((problem-answer p))
                 }))})

(defstep (grade-submission)
  (define scores
    (for/list ([p problems])
      (for/list ([s (problem-score p)])
       (s))))
  ; FIXME: Replace by skipping, since I shouldn't display the score until later.
  @md{# The scores

      @`(ul
         ,@(for/list ([p scores]
                      [i (in-range 1 (add1 (length scores)))])

             (li (format "Scores on Problem ~a:" i)
               (apply ol
                 (for/list ([s p]
                            [j (in-range 1 (add1 (length p)))])
                   (li (format "Score on question ~a: ~a" j s)))
                 ))))

      @button{Continue}})

(defstudy assignment1
  [overview --> submission --> overview]
  [grade-submission --> overview-after-submission --> overview-after-submission])
