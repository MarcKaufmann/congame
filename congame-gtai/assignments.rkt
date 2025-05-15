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

(require conscript/form0
         conscript/game-theory
         racket/format
         racket/match
         threading)

(provide
 assignment1)

(with-namespace xyz.trichotomy.congame-gtai.assignment1
  ;; Opening and closing assignment
  (defvar*/instance assignment-open?)

  ;; Track total scores
  (defvar*/instance scores)

  ;; Track if score was already put to identity
  (defvar* score-put?)

  ;;;; Assignment 1
  ;; fq problem - fight for prey
  (defvar* fq-scores)

  ;; gg problem
  (defvar* gg-scores)

  ;; mixed strategy
  (defvar* ms-scores)

  ;;;; Assignment 2
  ;; ms2 problem - mixed strategy 2
  (defvar* ms2-scores)

  ;; ms3 problem - mixed strategy 3
  (defvar* ms3-scores)

  ;; ms4 problem - mixed strategy 4
  (defvar* ms4-scores)

  ;; Assignment3
  ;; ext1 problem - extensive form game 1
  (defvar* ext1-scores)

  ;; ext2 problem - extensive form game 2
  (defvar* ext2-scores)


  )

(defvar ms1)
(defvar ms2)

;; Score weights of problems
(define problem-weights
  (hash ; Assignment 1: sum to 1
        'ms 0.33
        'fq 0.33
        'gg 0.34

        ; Assignment 2: sum to 1
        'ms2 0.30
        'ms3 0.30
        'ms4 0.40

        ; Assignment 3: sum to 1
        'ext1 0.60
        'ext2 0.40
        ))

;; Score weights of subproblems

(define fq-weights
  (list 0.25 0.25 0.5))

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

(define probability-form
  (form*
   ([probabilities
     (ensure
      binding/list
      (list-of-length 2)
      (list-of
       (ensure
        binding/number
        (required)
        (range/inclusive 0.0 1.0))))]
    [n-equilibria
     (ensure
      binding/number
      (required)
      (range/inclusive 0 10))])
   (list probabilities n-equilibria)))

(define (make-probability-form-on-submit set-probabilities! set-n-equilibria!)
  (match-lambda
    [(list probabilities n-equilibria)
     #:when assignment-open?
     (set-probabilities! probabilities)
     (set-n-equilibria! n-equilibria)
     (skip)]
    [_
     (skip 'submission-closed)]))

(define widget-probability
  (widget-number #:attributes '((min "0") (max "1") (step "0.001"))))

(define (input-probability label)
  (input-number label #:attributes '((min "0") (max "1") (step "0.001"))))

(define (input-range label #:min min #:max max)
  (input-number label #:attributes `((min ,(~a min)) (max ,(~a max)))))

(define (input-spe label #:max [max 200])
  (input-number label #:attributes `((min "0") (max ,(~a max)) (step "0.01"))))

(defstep (ms-question)
  (define on-submit
    (make-probability-form-on-submit
     (λ (probabilities) (set! ms1 probabilities))
     (λ (n-equilibria) (set! ms2 n-equilibria))))

  (define (render rw)
    @md*{### Question 1

         Find a mixed strategy equilibrium for the game. Provide the probabilities up to the third decimal after the comma.

         @rw["probabilities"
             (widget-list
              (lambda (re)
                @md*{@re[@input-probability{Probability of player 1 playing T}]
                     @re[@input-probability{Probability of player 2 playing L}]}))]

         ### Question 2

         @rw["n-equilibria"
             @input-range[#:min 0 #:max 10]{How many Nash equilibria are there, both pure and mixed? (You can go up to 10 - if there are more than 10, including if there are infinite Nash equilibria, then choose 10.)}]

         @(if assignment-open? submit-button @div{})})

  @md{# Mixed Strategy Equilibrium!!!!

      Consider the following game:

      @(outcome-matrix inspect-shirk-game)

      @form[probability-form on-submit render]
      @button[#:to-step-id 'ms-overview]{Cancel}})

(defstep (ms-compute-score)
  (define weights
    (list 0.75 0.25))
  (define pT 0.333)
  (define pL 0.667)
  (define given-pT (first ms1))
  (define given-pL (second ms1))
  (define delta
    (+ (abs (- pT given-pT)) (abs (- pL given-pL))))
  (define raw-scores
    (list
     (if (> 0.015 delta) 100 0)
     (cond [(= ms2 1) 100]
           [(= ms2 0) 50]
           [else 0])))
  (set! ms-scores
        (map * weights raw-scores))
  (skip))

(defstep (ms-overview)
  @md{# Mixed Strategy: Your Answers

      1. Find a mixed strategy

      You gave the following probabilities (for T and L):
      @(if (undefined? ms1) "not yet answered"
           (format "(~a, ~a)" (first ms1) (second ms1)))

      2. How many Nash equilibria are there (the max you could choose was 10, which stands for "10 or more").

      @(if (undefined? ms2) "not yet answered"
           @md*{Your answer: @(~a ms2)})

      @button{Continue}})

(defstep ((check-assignment-open? k))
  (if assignment-open? (skip) (skip k)))

(defstep (submission-closed)
  @md{# Submisssion Closed

      Your answers couldn't be registered, since submissions are closed.

      @button{Continue}})

(defstep (ms-init)
  (when (undefined? ms-scores)
    (set! ms-scores '(0 0)))
  (skip))

(defstudy ms-study
  [ms-init --> ms-question --> ms-compute-score --> ms-overview --> [ms-compute-score2 ms-compute-score] --> ,(lambda () done)]
  [submission-closed --> ms-overview])

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

(defstep (fq-init)
  (when (undefined? fq-scores)
    (set! fq-scores '(0 0 0)))
  (skip))

(define fq-form
  (form*
   ([payoffs
     (ensure
      binding/list
      (list-of-length 4)
      (list-of
       (ensure
        binding/number
        (required))))]
    [prisoners-dilemma?
     (ensure
      binding/text
      (required)
      (one-of
       '(("yes" . #t)
         ("no"  . #f))))]
    [fink-fink? binding/boolean]
    [fink-quiet? binding/boolean]
    [quiet-fink? binding/boolean]
    [quiet-quiet? binding/boolean])
   (list payoffs prisoners-dilemma? fink-fink? fink-quiet? quiet-fink? quiet-quiet?)))

(defstep (fq-question)
  (define on-submit
    (match-lambda
      [(list payoffs prisoners-dilemma? fink-fink? fink-quiet? quiet-fink? quiet-quiet?)
       (set! p1t1/q1 payoffs)
       (set! p1t1/q2 prisoners-dilemma?)
       (set! ff fink-fink?)
       (set! fq fink-quiet?)
       (set! qf quiet-fink?)
       (set! qq quiet-quiet?)
       (skip)]))

  (define (render rw)
    @md*{
      #### 1. Formulate this situation as a strategic game.

      @rw["payoffs"
          (widget-list
           (lambda (re)
             @md*{@re[@input-number{Payoff to player 1 from "Fink-Fink"}]
                  @re[@input-number{Payoff to player 1 from "Fink-Quiet"}]
                  @re[@input-number{Payoff to player 1 from "Quiet-Fink"}]
                  @re[@input-number{Payoff to player 1 from "Quiet-Quiet"}]}))]

      #### 2. Is this game a Prisoner's Dilemma?

      If you call the aggressive action Fink and the passive action
      Quiet, is the resulting game the same as the Prisoner’s Dilemma?
      (That is, are the players’ preferences in the resulting game the
      same as their preferences in the Prisoner’s Dilemma?)

      @rw["prisoners-dilemma?"
          @radios['(("yes" . "Yes")
                    ("no"  . "No"))
                  ""]]

      #### 3. What are the pure-strategy Nash equilibria (if any) of the game?

      Check all action profiles that are Nash equilibria.

      @rw["fink-fink?" @checkbox{Fink-Fink}]
      @rw["fink-quiet?" @checkbox{Fink-Quiet}]
      @rw["quiet-fink?" @checkbox{Quiet-Fink}]
      @rw["quiet-quiet?" @checkbox{Quiet-Quiet}]

      @(if assignment-open? submit-button @div{})})

   @md{# Fighting over Prey

       Two animals are fighting over some prey. Each can be passive or aggressive. Each prefers to be aggressive if its opponent is passive, and passive if its opponent is aggressive; given its own stance, it prefers the outcome in which its opponent is passive to that in which its opponent is aggressive.

       @form[fq-form on-submit render]
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
  [fq-init --> fq-question
           --> fq-compute-score
           --> fq-overview
           --> ,(lambda () done)]
  [submission-closed --> fq-overview])

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
  (if (undefined? ggs)
      0
      (for/sum ([c correct]
                [g (given-answers ggs)])
        (if (equal? c g) 25 0))))

(define (assignment-closed?)
  ; If undefined, it means that admin didn't set, so I assume it is open.
  (not (if-undefined assignment-open? #t)))

(defstep (gg-question)
  (define gg-formlet
    (ensure
     binding/list
     (list-of
      (ensure
       binding/symbol
       (required)
       (one-of (for/list ([opt (in-list gg-opts)])
                 ;; gg1 and co expect a list of strings
                 (match-define (cons value _) opt)
                 (cons value (~a value))))))))

  (define gg-form
    (form*
      ([gg1 gg-formlet]
       [gg2 gg-formlet]
       [gg3 gg-formlet]
       [gg4 gg-formlet]
       [gg5 gg-formlet])
      (list gg1 gg2 gg3 gg4 gg5)))

  (define (on-submit ggs)
    (cond [assignment-open?
           (eprintf "ggs: ~s~n" ggs)
           (match-define (list g1 g2 g3 g4 g5) ggs)
           (set! gg1 g1)
           (set! gg2 g2)
           (set! gg3 g3)
           (set! gg4 g4)
           (set! gg5 g5)
           (skip)]

          [else
           (skip 'submission-closed)]))

  (define (render rw)
    @md*{@style{
           label {
             display: block;
           }
           .group {
             border: none;
           }
         }


         #### 1. Two selfish players play the game.

         Check all possible action profiles:

         @rw["gg1" @checkboxes[gg-opts]]

         #### 2. Two indignant angels play the game.

         Check all possible action profiles:

         @rw["gg2" @checkboxes[gg-opts]]

         #### 3. Two indignant angels play the game

         Moreover, each thinks that the other player is selfish. Check all possible action profiles:

         @rw["gg3" @checkboxes[gg-opts]]

         #### 4. Two selfish players play the game

         Morever, each confuses action α for action β. Check all possible action profiles:

         @rw["gg4" @checkboxes[gg-opts]]

         #### 5. Player 1 is selfish, player 2 is an indignant angel

         Moreover, neither player believes the other player to be rational. Check all possible action profiles:

         @rw["gg5" @checkboxes[gg-opts]]


         @(if assignment-open? submit-button @div{})})

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

             @form[gg-form on-submit render]

             @button[#:to-step-id 'gg-overview]{Cancel}}]))

(defstep (gg-compute-score)
  (define gg-weights
    (list 0.2 0.2 0.2 0.2 0.2))
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
          (build-list 5 (lambda (_) 0))))
  (skip))

(defstudy grade-game-quiz
  [gg-init --> gg-question
           --> gg-compute-score
           --> gg-overview
           --> ,(lambda () done)]
  [submission-closed --> gg-overview])

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
  #;(when (and (assignment-closed?)
               (not score-put?))
      (put/identity 'total-score total-score))
  (put/identity 'total-score total-score)
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

(defstep ((show-scores lop))
  (define (display-scores get-scs label k)
    (define scs (get-scs))
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

          @`(div
             ,@(for/list ([p lop])
                 @(display-scores (first p) (second p) (third p))))
          @button{Back to Problems}}))

(defstep show-scores1
  (show-scores
   (list
    (list (λ () ms-scores) "Mixed Strategy" 'ms)
    (list (λ () fq-scores) "Fighting over Prey" 'fq)
    (list (λ () gg-scores) "Grade Game" 'gg))))

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
  [[show-scores show-scores1] --> problem-overview])

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

;; Assignment 2

;; Problem 1

(defvar ms2-ne)
(defvar ms2-n-pure-eqba)

(defstep (ms2-init)
  (when (undefined? ms2-scores)
    (set! ms2-scores '(0 0)))
  (skip))

(define ms2-game
  (hash
   'actions1 '(T B)
   'actions2 '(L R)
   'outcomes1 (hash '(T . L) 6
                    '(T . R) 0
                    '(B . L) 3
                    '(B . R) 6)

   'outcomes2 (hash '(T . L) 0
                    '(T . R) 6
                    '(B . L) 2
                    '(B . R) 0)))

; FIXME: The 'Cancel' button is a pain to remember to always add. The rest of
; the plumbing also feels needlessly painful. Try to refactor better, without
; hardcoding too much.
(defstep (ms2-question)
  (define on-submit
    (make-probability-form-on-submit
     (λ (probabilities) (set! ms2-ne probabilities))
     (λ (n-equilibria) (set! ms2-n-pure-eqba n-equilibria))))

  (define (render rw)
    @md*{
      ### Question 1

      Find the mixed strategy Nash equilibrium of the following game
      which is not a pure-strategy equilibrium. (Note: there is only one
      in this case - that is not a general feature.)


      @rw["probabilities"
          (widget-list
           (lambda (re)
             @md*{@re[@input-probability{Probability of player 1 playing T}]
                  @re[@input-probability{Probability of player 2 playing L}]}))]

      ### Question 2


       @rw["n-equilibria"
           @input-range[#:min 0 #:max 4]{How many pure-strategy equilibria are there for this game?}]

       @(if assignment-open? submit-button @div{})})

  @md{# Find the Mixed Strategy Nash Equilibria

      Consider the following game:

      @(outcome-matrix ms2-game)

      @form[probability-form on-submit render]
      @button[#:to-step-id 'ms2-overview]{Cancel}})

; FIXME: refactor with ms1-compute-score eventually
(defstep (ms2-compute-score)
  (define weights
    (list 0.75 0.25))
  (define pT 0.250)
  (define pL 0.667)
  (define given-pT (first ms2-ne))
  (define given-pL (second ms2-ne))
  (define delta
    (+ (abs (- pT given-pT)) (abs (- pL given-pL))))
  (define raw-scores
    (list
     (if (> 0.01 delta) 100 0)
     (if (= ms2-n-pure-eqba 0) 100 0)))
  (set! ms2-scores
        (map * weights raw-scores))
  (skip))

(defstep (ms2-overview)
  @md{# Mixed Strategy: Your Answers

      1. Find a mixed strategy equilibrium (that is not an equilibrium in pure strategies).

      You gave the following probabilities (for T and L):
      @(if (undefined? ms2-ne) "not yet answered"
           (format "(~a, ~a)" (first ms2-ne) (second ms2-ne)))

      2. How many pure-strategy Nash equilibria are there? (0 to 4)

      @(if (undefined? ms2-n-pure-eqba) "not yet answered"
           @md*{Your answer: @(~a ms2-n-pure-eqba)})

      @button{Continue}})

(defstudy ms2-study
  [ms2-init --> [check-assignment (check-assignment-open? 'ms2-overview)] --> ms2-question --> ms2-compute-score --> ms2-overview --> ,(lambda () done)]
  [submission-closed --> ms2-overview])

;; Problem 2

(defvar ms3-ne-comp-p)
(defvar ms3-ne-p)
(defvar ms3-ne-comp-q)
(defvar ms3-ne-q)
(defvar ms3-n-pure-eqba)

(defstep (ms3-init)
  (when (undefined? ms3-scores)
    (set! ms3-scores '(0 0)))
  (skip))

(define ms3-game
  (hash
   'actions1 '(T B)
   'actions2 '(L R)
   'outcomes1 (hash '(T . L) 2
                    '(T . R) 0
                    '(B . L) 2
                    '(B . R) 1)

   'outcomes2 (hash '(T . L) 2
                    '(T . R) 0
                    '(B . L) 0
                    '(B . R) 1)))

(defstep (ms3-question)
  (define comp-opts
    `((""   . " ")
      ("="  . "=")
      (">=" . ">=")
      ("<=" . "<=")))
  (define comp-formlet
    (ensure
     binding/text
     (required)
     (one-of (cdr comp-opts))))
  (define p-formlet
    (ensure
     binding/number
     (required)
     (range/inclusive 0.0 1.0)))
  (define ms3-form
    (form*
     ([p-comp comp-formlet]
      [p p-formlet]
      [q-comp comp-formlet]
      [q p-formlet]
      [n (ensure
          binding/number
          (required)
          (range/inclusive 0 4))])
     (list p-comp p q-comp q n)))

  (define (on-submit lst)
    (cond [assignment-open?
           (match-define (list p-comp p q-comp q n) lst)
           (set! ms3-ne-comp-p p-comp)
           (set! ms3-ne-p p)
           (set! ms3-ne-comp-q q-comp)
           (set! ms3-ne-q q)
           (set! ms3-n-pure-eqba n)
           (skip)]

          [else
           (skip 'submission-closed)]))

  (define (render rw)
    @md*{
      @div{
        Find the range p of mixed strategy Nash of the following
        game (don't search for pure strategy equilibria that do not
        fall within this range).}

      @div{
        The probability \(p\) that player 1 plays T is given by \(p\)
        @rw["p-comp" @widget-select[comp-opts]]
        @rw["p" widget-probability].}

      @div{
        @apply[div @rw["p-comp" (@widget-errors)]]
        @apply[div @rw["p" (@widget-errors)]]}

      @div{
        The probability \(q\) that player 2 plays \(L\) is given by \(q\)
        @rw["q-comp" @widget-select[comp-opts]]
        @rw["q" widget-probability].}

      @div{
        @apply[div @rw["q-comp" (@widget-errors)]]
        @apply[div @rw["q" (@widget-errors)]]}

      @rw["n" @input-number{How many pure-strategy equilibria are there for this game?}]

      @submit-button})

  @md{# Find the Mixed Strategy Nash Equilibria

      Consider the following game: \\(p\\)

      @(outcome-matrix ms3-game)

      @form[ms3-form on-submit render]
      @button[#:to-step-id 'ms3-overview]{Cancel}})

; FIXME: refactor with ms1-compute-score eventually
(defstep (ms3-compute-score)
  (define weights
    (list 0.75 0.25))
  (define p 0.333)
  (define p-comp ">=")
  (define q-comp "=")
  (define q 1.000)
  (define given-p ms3-ne-p)
  (define given-p-comp ms3-ne-comp-p)
  (define given-q ms3-ne-q)
  (define given-q-comp ms3-ne-comp-q)
  (define delta
    (+ (abs (- p given-p)) (abs (- q given-q))))
  (define raw-scores
    (list
     (+ (if (> 0.01 delta) 50 0)
        (if (and (string=? p-comp given-p-comp) (string=? q-comp given-q-comp)) 50 0))
     (if (= ms3-n-pure-eqba 2) 100 0)))
  (set! ms3-scores
        (map * weights raw-scores))
  (skip))

(defstep (ms3-overview)
  @md{# Mixed Strategy: Your Answers

      1. Find the range of mixed strategy Nash of the following game (don't search for pure strategy equilibria that do not fall within this range).

      Your answer:

      @(if (undefined? ms3-ne-p)
           "not yet answered"
           @md*{
             @p{The probability \(p\) that player 1 plays \(T\) is given by \(p\) @(~a ms3-ne-comp-p) @(~a ms3-ne-p).}
             @p{The probability \(q\) that player 2 plays \(L\) is given by \(q\) @(~a ms3-ne-comp-q) @(~a ms3-ne-q).}})

      2. How many pure-strategy Nash equilibria are there? (0 to 4)

      @(if (undefined? ms3-n-pure-eqba) "not yet answered"
           @md*{Your answer: @(~a ms3-n-pure-eqba)})

      @button{Continue}})

(defstudy ms3-study
  [ms3-init --> [check-assignment (check-assignment-open? 'ms3-overview)] --> ms3-question --> ms3-compute-score --> ms3-overview --> ,(lambda () done)]
  [submission-closed --> ms3-overview])

;; Problem 3

(defvar ms4-dominated-action)
(defvar ms4-ne1)
(defvar ms4-ne2)
(defvar ms4-ne3)
(defvar ms4-ne4)

(defstep (ms4-init)
  (when (undefined? ms4-scores)
    (set! ms4-scores '(0 0)))
  (skip))

(define ms4-game
  (hash
   'actions1 '(T M B)
   'actions2 '(L R)
   'outcomes1 (hash '(T . L) 2
                    '(T . R) 2
                    '(M . L) 3
                    '(M . R) 0
                    '(B . L) 0
                    '(B . R) 7)

   'outcomes2 (hash '(T . L) 3
                    '(T . R) 3
                    '(M . L) 1
                    '(M . R) 0
                    '(B . L) 0
                    '(B . R) 2)))

(defstep (ms4-question)
  (define dominated-action-opts
    (for/list ([v (in-list '("T" "M" "B"))])
      (cons v v)))
  (define p-formlet
    (ensure
     binding/number
     (range/inclusive 0.0 1.0)))

  (define ms4-form
    (form*
     ([da (ensure
           binding/text
           (required)
           (one-of dominated-action-opts))]
      [p1 (ensure p-formlet (required))]
      [q1 (ensure p-formlet (required))]
      [p2 p-formlet]
      [q2 p-formlet]
      [p3 p-formlet]
      [q3 p-formlet]
      [p4 p-formlet]
      [q4 p-formlet])
     (list da p1 q1 p2 q2 p3 q3 p4 q4)))

  (define (on-submit lst)
    (cond [assignment-open?
           (match-define (list da p1 q1 p2 q2 p3 q3 p4 q4) lst)
           (set! ms4-dominated-action da)
           (set! ms4-ne1 (cons p1 q1))
           (set! ms4-ne2 (cons p2 q2))
           (set! ms4-ne3 (cons p3 q3))
           (set! ms4-ne4 (cons p4 q4))
           (skip)]

          [else
           (skip 'submission-closed)]))

  (define (render rw)
    @md*{### Find the dominated action

         @rw["da" @radios[dominated-action-opts]{Which action by player 1 is strictly dominated by a mixed strategy?}]

        ### Find all mixed strategy NE

        Find as many mixed strategy NE as you can find (up to 4). Denote by X and Y the two non-dominated strategies of player 1 ordered by T before M before B -- e.g., if action T was dominated, then X is M and Y is B. Then report each NE by first providing the probability \(p\) that player 1 plays X, and then the probability \(q\) that player 2 plays L.

        **Note:** Provide all numbers up to the third digit, i.e., if the answer is 1/3, then write 0.333.

        1. Probability that player 1 plays X: @rw["p1" @widget-probability]; probability that player 2 plays L: @rw["q1" widget-probability].
        @apply[div @rw["p1" @widget-errors[]]]
        @apply[div @rw["q1" @widget-errors[]]]
        2. Probability that player 1 plays X: @rw["p2" @widget-probability]; probability that player 2 plays L: @rw["q2" widget-probability].
        @apply[div @rw["p2" @widget-errors[]]]
        @apply[div @rw["q2" @widget-errors[]]]
        3. Probability that player 1 plays X: @rw["p3" @widget-probability]; probability that player 2 plays L: @rw["q3" widget-probability].
        @apply[div @rw["p3" @widget-errors[]]]
        @apply[div @rw["q3" @widget-errors[]]]
        4. Probability that player 1 plays X: @rw["p4" @widget-probability]; probability that player 2 plays L: @rw["q4" widget-probability].
        @apply[div @rw["p4" @widget-errors[]]]
        @apply[div @rw["q4" @widget-errors[]]]

        @|submit-button|})

  @md{# Find the Mixed Strategy Nash Equilibria

      Consider the following game:

      @(outcome-matrix ms4-game)

      @form[ms4-form on-submit render]
      @button[#:to-step-id 'ms4-overview]{Cancel}
      })

(define (near x y [delta 0.01])
   (> delta (abs (- (if x x +inf.0) (if y y -inf.0)))))

; FIXME: refactor with ms1-compute-score eventually
(defstep (ms4-compute-score)
  (define (true? x)
    (if x #t #f))
  (define weights '(0.20 0.80))
  (define (near-ne ne1 ne2)
    (and (near (car ne1) (car ne2))
         (near (cdr ne1) (cdr ne2))))
  (define nes
    (list ms4-ne1 ms4-ne2 ms4-ne3 ms4-ne4))
  (define true-nes
    '((1 . 1) (0 . 0) (0.667 . 0.700)))
  (define (is-x-in-l? x l)
    (if (not x)
        (member #f l)
        (findf (lambda (y) (near-ne x y)) (filter true? l))))
  (eprintf "true-nes: ~a; given-nes: ~a" true-nes nes)
  (define raw-scores
    (list
     (if (string=? ms4-dominated-action "T") 100 0)
     (for/sum ([true-ne true-nes])
       (if (is-x-in-l? true-ne nes) 100/3 0))))

  (set! ms4-scores
        (map * weights raw-scores))
  (skip))

(defstep (ms4-overview)
  (define nes-given
    (list ms4-ne1 ms4-ne2 ms4-ne3 ms4-ne4))

  @md{# Mixed Strategy: Your Answers

      1. Find player 1's dominated action.

      Your answer:

      @(if (undefined? ms4-dominated-action)
           "not yet answered"
           (format "Action ~a" ms4-dominated-action))

      2. Find all the mixed strategy Nash Equilibria (up to 4)

      @(if (ormap undefined? nes-given) "not yet answered"
           @md*{
            Your answer:

            @`(ul
               ,@(for/list ([ne nes-given]
                            #:when (or (car ne) (cdr ne)))
                   (li (format "(~a, ~a)" (car ne) (cdr ne)))))})

      @button{Continue}})

(defstudy ms4-study
  [ms4-init --> [check-assignment (check-assignment-open? 'submission-closed)] --> ms4-question --> ms4-compute-score --> ms4-overview --> ,(lambda () done)]
  [submission-closed --> ms4-compute-score])


; FIXME: Refactor problem-overview and assignment2-overview
; In that case, I should pass in the problem-weights as an argument.
; Some of the hard-coded skips in buttons, e.g., #:to-step-id 'show-score
; are hard to add to a function that can be reused.
(defstep (assignment2-overview)
  (define total-score
    (for/sum ([ss (list ms2-scores ms3-scores ms4-scores)]
              [k '(ms2 ms3 ms4)])
      (if (undefined? ss)
          0
          (* (hash-ref problem-weights k) (apply + ss)))))
  (with-study-transaction
    (set! scores
          (hash-set scores
                    (current-participant-id)
                    total-score)))
  ; TODO: I could just always put, but that seems wasteful.
  (when (assignment-closed?)
    (put/identity 'total-score total-score))

  @md{# Problems (Total: 100 points)

      @(if (assignment-closed?)
           @md*{## Scores

                Your overall score is: @(~r total-score #:precision 0)

                @button[#:to-step-id 'show-scores]{Go to Scores}}
           @div{})

      ## Notes

      - You can submit and resubmit answers as often as you want before the deadline.
      - You can see your previously submitted answers by going to the problem, and instead of clicking on "Submit" clicking on "Cancel"
      - After the deadline, you will see your score
      - When inputting numbers, provide up to 3 decimals of precision so the answers can be compared sufficiently precisely

      ## Mixed Strategy 1 (@(~points 'ms2) Points)

      @button[(λ () (set! active-problem 'ms2-game))]{Go to "Mixed Strategy 1"}

      ## Mixed Strategy 2 (@(~points 'ms3) Points)

      @button[(λ () (set! active-problem 'ms3-game))]{Go to "Mixed Strategy 2"}

      ## Mixed Strategy 3 (@(~points 'ms4) Points)

      @button[(λ () (set! active-problem 'ms4-game))]{Go to "Mixed Strategy 3"}
      })

(defstep show-scores2
  (show-scores
   (list
    (list (λ () ms2-scores) "Mixed Strategy 1" 'ms2)
    (list (λ () ms3-scores) "Mixed Strategy 2" 'ms3)
    (list (λ () ms4-scores) "Mixed Strategy 3" 'ms4)
    )))

(defstudy assignment2/no-admin
  [init --> [assignment-overview assignment2-overview] --> ,(lambda ()
                           (case active-problem
                             [(ms2-game) 'ms2-study]
                             [(ms3-game) 'ms3-study]
                             [(ms4-game) 'ms4-study]
                             ))]
  [ms2-study --> assignment-overview]
  [ms3-study --> assignment-overview]
  [ms4-study --> assignment-overview]
  [[show-scores show-scores2] --> assignment-overview])

(provide
 assignment2)

(define assignment2
  (make-admin-study
   #:models `()
   #:admin admin
   assignment2/no-admin))

;; Assignment 3

;; Problem 1
(defvar ext1-nes)
(defvar ext1-wses)

(define ext1-ne-opts
  (let ([->up (lambda (x)
                (string-upcase (symbol->string x)))])
    (for*/list ([a1 '(l r)]
                [a2 '(l r)]
                [a3 '(l r)])
      (cons (string->symbol (format "~a~a~a" a1 a2 a3))
            (format "(~a, ~a, ~a)" (->up a1) (->up a2) (->up a3))))))

(defstep (ext1-init)
  (when (undefined? ext1-scores)
    (set! ms-scores '(0 0)))
  (when (undefined? ext1-wses)
    (set! ext1-wses (hash 'counter 0)))
  (when (undefined? ext1-nes)
    (set! ext1-nes null))
  (skip))

(define wse-choices
  '((""  . "--action--")
    ("L" . "L")
    ("R" . "R")))

(define wse-form
  (let ([wse
         (ensure
          binding/text
          (required)
          (one-of (cdr wse-choices)))])
    (form*
     ([wse1 wse]
      [wse2 wse]
      [wse3 wse]
      [wse3-belief
       (ensure
        binding/number
        (required)
        (range/inclusive 0 10))])
     (list wse1 wse2 wse3 wse3-belief))))

(defstep (add-wse)
  (define on-submit
    (match-lambda
      [(list wse1 wse2 wse3 wse3-belief)
       (define new-wse
         (list wse1 wse2 wse3 wse3-belief))
       (define counter
         (hash-ref ext1-wses 'counter))
       (set! ext1-wses
             (hash-set
              (hash-set ext1-wses counter new-wse)
              'counter
              (add1 counter)))]))

  (define (render rw)
    @md*{Provide another WSE:

         @rw["wse1" @select[wse-choices]{Action of Player 1}]
         @rw["wse2" @select[wse-choices]{Action of Player 2}]
         @rw["wse3" @select[wse-choices]{Action of Player 3}]
         @rw["wse3-belief" @input-probability{Probability with which player 3 believes that history "(L)" occurred when they have to make a decision.}]
         @submit-button})

  (cond
    [(not assignment-open?)
     @md{# Assignment is closed

         The assignment is closed, you can no longer submit answers.

         @button {Continue}}]

    [else
     @md{# Add WSE

         @form[wse-form on-submit render]
         @button{Cancel}}]))

(define (display-wses [remove? #t])
  (define (remove-wse i)
    (set! ext1-wses
          (hash-remove ext1-wses i)))
  @`(table
     ,@(for/list ([(i wse) (in-hash (hash-remove ext1-wses 'counter))])
         (tr (td (format
              "WSE ~a: strategy profile (~a, ~a, ~a) with P3 putting probability ~a on history \"(L)\""
              i (first wse) (second wse) (third wse) (fourth wse)))
             @(if remove?
                  (td @button[(λ () (remove-wse i)) #:to-step-id 'ext1-question]{Remove this WSE})
                  @td{})))))

(define-static-resource ext1-game-screenshot "ext1-game.png")

(define (img-ext1-game)
  @div[#:class "screenshot"]{
    @img[#:src (resource-uri ext1-game-screenshot) #:alt "The Game"]})

(defstep (ext1-question)
  (define ext1-form
    (dyn:form
     list
     (for/list ([(opt idx) (in-indexed (in-list ext1-ne-opts))])
       (match-define (cons value _) opt)
       (cons
        (string->symbol (format "ne~a" idx))
        (ensure
         binding/boolean
         (lambda (v)
           (ok (if v (symbol->string value) ""))))))))

  (define (on-submit nes)
    (cond
      [assignment-open?
       (set! ext1-nes nes)
       (skip)]
      [else
       (skip 'ext1-overview)]))

  (define (render rw)
    @md*{
      @style{
        label {
          display: block;
        }
        .group {
          border: none;
        }
      }

      Consider the following game:

      @(img-ext1-game)

      ## Find all the pure strategy Nash equilibria

      For each of the following pure strategy profiles, check whether
      it is or is not a Nash equilibrium of the game:

      @(apply div (for/list ([(opt idx) (in-indexed (in-list ext1-ne-opts))])
                    (match-define (cons _ label) opt)
                    (rw
                     (format "ne~a" idx)
                     (checkbox label))))
      @submit-button})

  @md{# Extensive Form Game with Imperfect Information

      @form[ext1-form on-submit render]

      ## Find all weak sequential equilibria

      Find all the pure strategy weak sequential equilibria of the game.
      (Hint: Every WSE must be a NE.)

      Note that for every equilibrium, in addition to the strategy
      profile you also have to specify the belief system, i.e., the
      beliefs over the history that has occurred for each information
      set with more than one history. For each different action profile
      that is part of a WSE, specify the highest belief p such that this
      is a WSE - p is player 3' belief that the history of arriving at
      their information set is the history "(L)".

      @button[#:to-step-id 'add-wse]{Add a new WSE}
      @display-wses[]
      @button[#:to-step-id 'ext1-overview]{Cancel}})

(defstep (ext1-compute-score)
  (define ext1-weights '(0.5 0.5))
  (define wses
    (hash-remove ext1-wses 'counter))
  (eprintf "wses: ~a; nes: ~a~n~n" wses ext1-nes)
  (define true-nes-found
    (+ (if (member "lrl" ext1-nes) 1 0)
       (if (member "rrr" ext1-nes) 1 0)))
  (eprintf "true-nes-found: ~a~n" true-nes-found)
  (define wrong-nes-found
    (- (length ext1-nes) true-nes-found))
  (define ne-score
    (max (- (* 50 true-nes-found)
            (* 25 wrong-nes-found))
         0))
  (define (correct-wse? x)
    (eprintf "correct-wse? x: ~a~n~n" x)
    (and (equal? (first x) "R")
         (equal? (second x) "R")
         (equal? (third x) "R")
         (near (fourth x) 0.333)))
  (eprintf "values of WSES: ~a~n~n~n" (hash-values wses))
  (define wse-true-found
    (findf correct-wse? (hash-values wses)))
  (eprintf "Didn't get here...~n~n~n")
  (define wse-wrong-found
    (- (hash-count wses) (if wse-true-found 1 0)))
  (define wse-score
    (max (- (* 100 (if wse-true-found 1 0))
            (* 30 wse-wrong-found))
         0))
  (eprintf "Scores - ne-scores: ~a; wse-scores: ~a~n" ne-score wse-score)
  (set! ext1-scores (map * ext1-weights (list ne-score wse-score)))
  (skip))

(defstep (ext1-overview)
  (define (ext1-answer->ap a)
    (string-join
     (map string
          (string->list
           (string-upcase a)))
     ", "))
  @md{# Your answers to the questions

      ## Find the pure strategy Nash equilibria of the game

      Your answers:

      @(if (undefined? ext1-nes)
          "No answer provided."
          @`(ul
             ,@(for/list ([a ext1-nes])
                 (li (ext1-answer->ap a)))))

      ## Find the pure strategy weak sequential equilibria

      @display-wses[#f]

      @button{Continue}})

(defstudy ext1-study
  [ext1-init --> [check (check-assignment-open? 'ext1-overview)] --> ext1-question --> ext1-compute-score --> ext1-overview --> ,(lambda () done)]
  [add-wse --> ext1-compute-score]
  [submission-closed --> ext1-overview])

;; Problem 2

(defvar ext2-spes)

(defstep (ext2-init)
  (when (undefined? ext2-scores)
    (set! ext2-scores '(0)))
  (when (undefined? ext2-spes)
    (set! ext2-spes (hash 'counter 0)))
  (skip))

(define (display-spes [remove? #t])
  (define (remove-spe i)
    (set! ext2-spes
          (hash-remove ext2-spes i)))
  @`(table
     ,@(for/list ([(i a) (in-hash (hash-remove ext2-spes 'counter))])
         (tr
          (td (format "SPE ~a: union suggests wage ~a, firm chooses labor ~a" i (first a) (second a)))
          (if remove?
              @td{@button[(λ () (remove-spe i)) #:to-step-id 'ext2-question]{Remove this SPE}}
              @td{})))))

(defstep (ext2-question)
  @md{# Firm and Union Game

      A firm's output is \\(L (100 - L)\\) when it uses \\(L \leq 50\\) units of labor, and \\(2500\\) when it uses \\(L > 50\\) units of labor. The price of output is \\(1\\). A union that represents workers presents a wage demand (a nonnegative number \\(w\\)), which the firm either accepts or rejects. If the firm accepts the demand, it chooses the number \\(L\\) of workers to employ (which you should take to be a continuous variable, not an integer); if it rejects the demand, no production takes place \\((L = 0)\\). The firm's preferences are represented by its profit; the union's preferences are represented by the value of \\(w L\\).

      Find the subgame perfect equilibrium (equilibria?) of the game.

      @button[#:class "add-new" #:to-step-id 'add-spe]{Add new SPE}

      @display-spes[]

      @button{Go back to problems}})

(defstep (add-spe)
  (define spe-form
    (form*
     ([spe
       (ensure
        binding/list
        (list-of*
         (ensure binding/number (required) (range/inclusive 0 200))
         (ensure binding/number (required) (range/inclusive 0 100))))])
     spe))

  (define (on-submit spe)
    (cond
      [assignment-open?
       ; NOTE: Should I wrap these in a transaction?
       (define c (hash-ref ext2-spes 'counter))
       (set! ext2-spes
             (~> ext2-spes
                 (hash-set c spe)
                 (hash-set 'counter (add1 c))))
       (skip)]

      [else
       (skip 'submission-closed)]))

  (define (render rw)
    @md*{@rw["spe"
             (widget-list
              (lambda (re)
                @md*{@re[@input-spe{Wage demanded by union.}]
                     @re[@input-spe[#:max 100]{Labor chosen by firm.}]}))]

         @|submit-button|})

  @md{# Add SPE

      For each SPE, provide the wage of the union, and the labor asked by the firm. Note: when the firm would reject the offer, just report that the firm asks for 0 labor.
      @form[spe-form on-submit render]
      @button{Back}})

(defstep (ext2-compute-score)
  (define spes
    (hash-values
     (hash-remove ext2-spes 'counter)))
  (define (true-spe? x)
    (and (near 50 (first x))
         (near 25 (second x))))
  (define n-true-spes-found
    (if (findf true-spe? spes) 1 0))
  (define n-wrong-spes-found
    (- (length spes) n-true-spes-found))
  (set! ext2-scores
        (list (max
               (- (* 100 n-true-spes-found)
                  (* 33 n-wrong-spes-found))
               0)))
  (skip))

(defstep (ext2-overview)
  @md{# Your answers

      @display-spes[#f]

      @button{Continue}})

(defstudy ext2-study
  [ext2-init --> [check (check-assignment-open? 'ext2-overview)] --> ext2-question --> ext2-compute-score --> ext2-overview --> ,(lambda () done)]
  [add-spe --> ext2-question]
  [submission-closed --> ext2-overview])

;; Overview
(defstep (assignment3-overview)
  (define total-score
    (for/sum ([ss (list ext1-scores ext2-scores)]
              [k '(ext1 ext2)])
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

      ## Notes

      - You can submit and resubmit answers as often as you want before the deadline.
      - You can see your previously submitted answers by going to the problem, and instead of clicking on "Submit" clicking on "Cancel"
      - After the deadline, you will see your score
      - When inputting numbers, provide up to 3 decimals of precision so the answers can be compared sufficiently precisely

      ## Extensive Form Game 1 (@(~points 'ext1) Points)

      @button[(λ () (set! active-problem 'ext1-game))]{Go to "Extensive Form Game 1"}

      ## Extensive Form Game 2(@(~points 'ext2) Points)

      @button[(λ () (set! active-problem 'ext2-game))]{Go to "Extensive Form Game 2"}

      })

(defstep show-scores3
  (show-scores
   (list
    (list (λ () ext1-scores) "Extensive Form Game 2" 'ext1)
    (list (λ () ext2-scores) "Extensive Form Game 2" 'ext2)
    )))

(defstudy assignment3/no-admin
  [init --> [assignment-overview assignment3-overview]
        --> ,(lambda ()
               (case active-problem
                 [(ext1-game) 'ext1-study]
                 [(ext2-game) 'ext2-study]))]
  [ext1-study --> assignment-overview]
  [ext2-study --> assignment-overview]
  [[show-scores show-scores3] --> assignment-overview])

(provide
 assignment3)

(define assignment3
  (make-admin-study
   #:models `()
   #:admin admin
   assignment3/no-admin))
