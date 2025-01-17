#lang conscript

;; For later (maybe):
;;
;; - Find all NE in a game where iterated gives a unique answer, but with many actions, so they should use iterated deletion. Check Pollak
;; - Question on 2nd price auction and finding an equilibrium where the person who cares less gets the good.
;; Play chicken game? Provide the game form.

; Quiz structure:
; - 2 comprehension questions
; - 4 easy questions
; - 2 harder questions
; - 2 games against bots

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
         conscript/survey-tools
         racket/format
         racket/match
         hash-view
         gregor)

(provide
 assignment1)

(with-namespace xyz.trichotomy.congame-gtai.assignment1
  ;; fq problem - fight for prey
  (defvar* fq-scores)

  ;; gg problem
  (defvar* gg-scores)

  ;; mixed strategy
  (defvar* ms-scores)

  ;; Opening and closing assignment
  (defvar*/instance assignment-open?)

  ;; Track total scores
  (defvar*/instance scores)

  ;; Track if score was already put to identity
  (defvar* score-put?)
  )

(defvar ms1)
(defvar ms2)

;; Score weights of problems
(define problem-weights
  (hash 'ms 0.33
        'fq 0.33
        'gg 0.34))

;; Score weights of subproblems

(define ms-weights
  (list 0.75 0.25))
(define fq-weights
  (list 0.25 0.25 0.5))
(define gg-weights
  (list 0.2 0.2 0.2 0.2 0.2))

(define inspect-shirk-game
  (hash 'actions1 '(T B)
        'actions2 '(L R)
        'outcomes1 (hash
                    '(T . L) 0
                    '(T . R) 3
                    '(B . L) 1
                    '(B . R) 1)
        'outcomes2 (hash
                    '(T . L) -1
                    '(T . R) -3
                    '(B . L) 0
                    '(B . R) 1)))

(define (input-probability label)
  (input-number #:min 0 #:max 1 #:step 0.001 label))

(defstep (ms-question)
  @md{# Mixed Strategy Equilibrium

      Consider the following game:

      @(outcome-matrix inspect-shirk-game)

      @form{
        @md*{
          ### Question 1

          Find a mixed strategy equilibrium for the game. Provide the probabilities up to the third decimal after the comma.

          @set![ms1 (input-list
                     (list
                      (input-probability "Probability of player  1 playing T")
                      (input-probability "Probability of player 2 playing L")))]

          ### Question 2

          @set![ms2 @input-number[#:min 0 #:max 10]{How many Nash equilibria are there, both pure and mixed? (You can go up to 10 - if there are more than 10, including if there are infinite Nash equilibria, then choose 10.)}]

          @submit-button}}

      @button[#:to-step-id 'problem-overview]{Cancel}})

(defstep (ms-compute-score)
  (define pT 0.333)
  (define pL 0.667)
  (define given-pT (first ms1))
  (define given-pL (second ms1))
  (define delta
    (+ (abs (- pT given-pT)) (abs (- pL given-pL))))
  (define raw-scores
    (list
     (if (> 0.01 delta) 100 0)
     (if (= ms2 1) 100 0)))
  (set! ms-scores
        (map * ms-weights raw-scores))
  (skip))

(defstep (ms-overview)
  @md{# Mixed Strategy: Your Answers

      1. Find a mixed strategy

      You gave the following probabilities (for T and L):
      @(format "(~a, ~a)" (first ms1) (second ms1))

      2. How many Nash equilibria are there (the max you could choose was 10, which stands for "10 or more").

      Your answer: @(~a ms2)

      @button{Continue}})

(defstudy ms-study
  [ms-question --> ms-compute-score --> ms-overview --> ,(lambda () done)])

(defvar p1t1/q1)
(defvar p1t1/q2)
(defvar ff)
(defvar fq)
(defvar qf)
(defvar qq)
(defvar gg1)
(defvar gg2)
(defvar gg3)
(defvar gg4)
(defvar gg5)

(define (yes/no [label ""])
  (map-result
   (radios '(("yes" . "Yes")
            ("no" . "No"))
          label)
   (lambda (r)
     (string=? r "yes"))))

(defstep (fq-init)
  (when (undefined? fq-scores)
    ; FIXME
    (set! fq-scores 3))
  (skip))

(defstep (fq-questions)
   @md{# Fighting over Prey

       Two animals are fighting over some prey. Each can be passive or aggressive. Each prefers to be aggressive if its opponent is passive, and passive if its opponent is aggressive; given its own stance, it prefers the outcome in which its opponent is passive to that in which its opponent is aggressive.

       @form{
        @md*{
          #### 1. Formulate this situation as a strategic game.

          @set![p1t1/q1 (input-list
                         (list
                          @input-number{Payoff to player 1 from "Fink-Fink"}
                          @input-number{Payoff to player 1 from "Fink-Quiet"}
                          @input-number{Payoff to player 1 from "Quiet-Fink"}
                          @input-number{Payoff to player 1 from "Quiet-Quiet"}))]

          #### 2. Is this game a Prisoner's Dilemma?

          If you call the aggressive action Fink and the passive action Quiet, is the resulting game the same as the Prisoner’s Dilemma? (That is, are the players’ preferences in the resulting game the same as their preferences in the Prisoner’s Dilemma?) If not, how do they differ?

          @set![p1t1/q2 (yes/no)]

          #### 3. What are the pure-strategy Nash equilibria (if any) of the game?

          Check all action profiles that are Nash equilibria.

          @div{
            @set![ff @checkbox[#:required? #f]{Fink-Fink}]}
          @div{
            @set![fq @checkbox[#:required? #f]{Fink-Quiet}]}
          @(div (set! qf @checkbox[#:required? #f]{Quiet-Fink}))
          @(div (set! qq @checkbox[#:required? #f]{Quiet-Quiet}))

          @submit-button}}

       @button[#:to-step-id 'fq-overview]{Cancel}})


(defstep (fq-compute-score)
  (match-define (list a b c d)
    p1t1/q1)
  (define raw-scores
    (list
     (+ (if (and (> c a) (> b d)) 50 0)
        (if (and (> b a) (> d c)) 50 0))
     (if (equal? p1t1/q2 #f) 100 0)
     (for/sum ([answer (list ff fq qf qq)]
               [correct-answer '(#f #t #t #f)])
       (if (equal? answer correct-answer) 25 -25))))
  (set! fq-scores (map * fq-weights raw-scores))
  (skip))

(defstep (fq-overview)
  @md{# Fighting over Prey: Your Answers

      1. What are payoffs consistent with that game?

      @(if (undefined? p1t1/q1)
           @md*{You have not yet provided an answer.}
           @md*{
             Your answer:

             - "Fink-Fink": @(~a (first p1t1/q1))
             - "Fink-Quiet": @(~a (second p1t1/q1))
             - "Quiet-Fink": @(~a (third p1t1/q1))
             - "Quiet-Quiet": @(~a (fourth p1t1/q1))
           })

      2. Is this game the same as the Prisoner's Dilemma?

      @(if (undefined? p1t1/q2)
           @md*{You have not yet provided an answer.}
           @md*{Your answer: @(if p1t1/q2 "yes" "no")})

      3. Which of the following are pure-strategy Nash equilibria?

      Your answer:

      @(if (undefined? p1t1/q2)
           @md*{You have not yet provided an answer.}
           @md*{
             @`(ul
               ,@(for/list ([answer (list ff fq qf qq)]
                            [ap '("Fink-Fink" "Fink-Quiet" "Quiet-Fink" "Quiet-Quiet")])
                   (li (format "~a: ~a" ap (if answer "yes" "no")))))
           })

      @(if (assignment-closed?)
           ""
           @button[#:to-step-id 'fq-question]{Change your Answers})

      @button{Back to other Problems}})

(defstudy fq-study
  [fq-init --> fq-questions --> fq-compute-score --> fq-overview --> ,(lambda () done)])

(define grade-game-outcomes
  (hash
   '(α . α) '(B- . B-)
   '(α . β) '(A  . C+)
   '(β . α) '(C+ . A )
   '(β . β) '(B+ . B+)))

(define (gg-selfish-utility ap)
  (match ap
   [(cons 'α  'α)  0]
   [(cons 'α  'β)  3]
   [(cons 'β  'α) -1]
   [(cons 'β  'β)  1]))

(define (gg-angels-utility ap)
  (match ap
   [(cons 'α  'α)  0]
   [(cons 'α  'β) -1]
   [(cons 'β  'α) -3]
   [(cons 'β  'β)  1]))

(define grade-game-form
  (hash
   'actions1 '(α β)
   'actions2 '(α β)
   'outcomes1 (for/hash ([(ap out) (in-hash grade-game-outcomes)])
                (values ap (car out)))
   'outcomes2 (for/hash ([(ap out) (in-hash grade-game-outcomes)])
                (values ap (cdr out)))))

(define gg-opts
  '((a . "(α, α)")
    (b . "(α, β)")
    (c . "(β, α)")
    (d . "(β, β)")))

(define (display-gg-opts ggs)
  (cond [(undefined? ggs)
         @md*{You have not yet provided an answer.}]
        [else
         (define (name-to-ap n)
           (case n
             [(a) "(α, α)"]
             [(b) "(α, β)"]
             [(c) "(β, α)"]
             [(d) "(β, β)"]))
         @md*{
              @`(ul
                 ,@(for/list ([ap '(a b c d)])
                     (li (format "~a: ~a"
                                 (name-to-ap ap)
                                 (if (member (symbol->string ap) ggs) "yes" "no")))))}]))

(define (given-answers ggs)
  (for/list ([n '(a b c d)])
    (if (member (symbol->string n) ggs) #t #f)))

(define (gg-score correct ggs)
  (for/sum ([c correct]
            [g (given-answers ggs)])
    (if (equal? c g) 25 0)))

(define (select-gg-opts)
  (make-multiple-checkboxes gg-opts))

(define (assignment-closed?)
  ; If undefined, it means that admin didn't set, so I assume it is open.
  (not (if-undefined assignment-open? #t)))

(defstep (gg-question)
  (cond [(assignment-closed?)
         (skip)]
        [else

         @md{# Grade Game Problem

             Consider the grade game from class with the following outcome-matrix:

             @(outcome-matrix grade-game-form)

             This problem asks you to state what will happen when different types of players play this game. Unless otherwise stated, the players are rational (meaning they don't play strictly dominated actions) and there is common knowledge of rationality.

             For each of the situations described below, check all the action profiles that are possible (meaning you cannot rule them out) under the above assumptions. **Note:** the above assumptions do not imply that a person plays Nash equilibrium.

             ### Payoff matrix for selfish players

             @(payoff-matrix/sym grade-game-form gg-selfish-utility)

             ### Payoff matrix for indignant angels

             @(payoff-matrix/sym grade-game-form gg-angels-utility)

             @form{
                   @style{
                     label {
                       display: block;
                     }
                     .group {
                       border: none;
                     }
                   }

                   @md*{
                        #### 1. Two selfish players play the game.

                        Check all possible action profiles:

                        @set![gg1 @(select-gg-opts)]

                        #### 2. Two indignant angels play the game.

                        Check all possible action profiles:

                        @set![gg2 @(select-gg-opts)]

                        #### 3. Two indignant angels play the game

                        Moreover, each thinks that the other player is selfish. Check all possible action profiles:

                        @set![gg3 @(select-gg-opts)]

                        #### 4. Two selfish players play the game

                        Morever, each confuses action α for action β. Check all possible action profiles:

                        @set![gg4 @(select-gg-opts)]

                        #### 5. Player 1 is selfish, player 2 is an indignant angel

                        Moreover, neither player believes the other player to be rational. Check all possible action profiles:

                        @set![gg5 @(select-gg-opts)]

                        @submit-button
                        }}

             @button[#:to-step-id 'gg-overview]{Cancel}}]))

(defstep (gg-compute-score)
  (define raw-scores
    (list
     (gg-score (list #t #f #f #f) gg1)
     (gg-score (list #t #t #t #t) gg2)
     (gg-score (list #t #f #f #f) gg3)
     (gg-score (list #f #f #f #t) gg4)
     (gg-score (list #t #t #f #f) gg5)))
  (set! gg-scores (map * gg-weights raw-scores))
  (skip))

(defstep (gg-cancel)
  (skip))

(defstep (gg-overview)
  @md{# Grade Game: Your Answers

      1. Two selfish players play the game:

      @display-gg-opts[gg1]

      2. Two indignant angels play the game:

      @display-gg-opts[gg2]

      3. Two indignant angels play the game, each thinking that the other is selfish.

      @display-gg-opts[gg3]

      4. Two selfish players play the game, but each of them confuses action α for action β.

      @display-gg-opts[gg4]

      5. Player 1 is selfish, player 2 is an indignant angel, but neither player believes the other player to be rational.

      @display-gg-opts[gg5]

      @(if (assignment-closed?)
           ""
           @button[#:to-step-id 'gg-question]{Change your Answers})

      @button{Back to other Problems}
      })

(defvar active-problem)

(defstep (gg-init)
  (when (undefined? gg-scores)
    (set! gg-scores
          (build-list 5 (lambda (x) 0))))
  (skip))

(defstudy grade-game-quiz
  [gg-init --> gg-question --> gg-compute-score --> gg-overview --> ,(lambda () done)])

(define (~points k)
  (~r (* 100 (hash-ref problem-weights k)) #:precision 0))

(defstep (problem-overview)
  (define total-score
    (for/sum ([ss (list ms-scores fq-scores gg-scores)]
              [k '(ms fq gg)])
      (if (undefined? ss)
          0
          (* (hash-ref problem-weights k) (apply + ss)))))
  (with-study-transaction
    (set! scores
          (hash-set scores
                    (current-participant-id)
                    total-score)))
  ; TODO: I could just always put, but that seems wasteful.
  (when (and (assignment-closed?)
             (not score-put?))
    (put/identity 'total-score total-score))
  @md{# Problems (Total: 100 points)

      @(if (assignment-closed?)
           @md*{## Scores

                Your overall score is: @(~r total-score #:precision 0)

                @button[#:to-step-id 'show-scores]{Go to Scores}}
           @div{})

      ## Mixed Strategy (@(~points 'ms) Points)

      Find the mixed strategy equilbria of a game.

      @button[(λ () (set! active-problem 'ms-game))]{Go to "Mixed Strategy"}

      ## Grade Game Problem (@(~points 'gg) Points)

      Variations on the grade game from class.

      @button[(λ () (set! active-problem 'grade-game))]{Go to "Grade Game"}

      ## Fighting over Prey (@(~points 'fq) Points)

      @button[(λ () (set! active-problem 'fq-game))]{Go to "Fighting over Prey"}

      })

(defstep (show-scores)
  (define (display-scores scs label k)
    (define w (hash-ref problem-weights k))
    (cond [(undefined? scs)
           @md*{## @(~a label) (0 Points)

                You didn't provide any answers to this part, so your score is 0.}]
          [else
           (define total
             (apply + (map (lambda (x) (* x w)) scs)))

           @md*{## @(~a label) (@(~r total #:precision 1) Points)

                @`(ul
                   ,@(for/list ([i (in-range (length scs))]
                                [s scs])
                       (li (format "Problem ~a: score ~a" (add1 i) (~r (* s w) #:precision 1)))))}]))

  (if assignment-open?
      @md{# Scores

          Scores are only available once the submission is closed.

          @button{Back}}
      @md{# Scores

          @(display-scores ms-scores "Mixed Strategy" 'ms)

          @(display-scores fq-scores "Fighting over Prey" 'fq)

          @(display-scores gg-scores "Grade Game" 'gg)

          @button{Back to Problems}}))

(defstep (init)
  (when (undefined? score-put?)
    (set! score-put? #f))
  (with-study-transaction
    (when (undefined? scores)
      (set! scores (hash))))
  (skip))

(defstudy assignment1/no-admin
  [init --> problem-overview --> ,(lambda ()
                           (case active-problem
                             [(grade-game) 'grade-game-quiz]
                             [(fq-game) 'fq-study]
                             [(ms-game) 'ms-study]))]
  [grade-game-quiz --> problem-overview]
  [fq-study --> problem-overview]
  [ms-study --> problem-overview]
  [show-scores --> problem-overview])

;; Admin

(require conscript/admin)

(defstep (admin)
  (with-study-transaction
    (when (undefined? assignment-closed?)
      (set! assignment-open? #t)))
  @md{# Admin

      ## Open/Close Assignment

      @(if assignment-open?
           @button[(lambda () (set! assignment-open? #f))]{Close Assignment}
           @button[(lambda () (set! assignment-open? #t))]{Reopen Assignment})

      ## Scores

      @`(ul
         ,@(for/list ([(k v) (in-hash (if-undefined scores (hash)))])
             (li (format "Participant ~a: Score ~a" k (~r v #:precision 0)))))
      })

(define assignment1
  (make-admin-study
   #:models `()
   #:admin admin
   assignment1/no-admin))
