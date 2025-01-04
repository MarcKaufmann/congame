#lang conscript/with-require

(require congame-web/components/study-bot
         conscript/survey-tools
         data/monocle
         racket/match
         "grade-game.rkt")

(provide
 grade-game-lecture)

; Variables
; In addition, grade-game provides
; - `choices` as a global instance var.
; - `grade-game-form`: the actions + outcome matrix for grade game.

(with-namespace xyz.trichotomy.congame.grade-game
  (defvar*/instance completed-phases)
  (defvar*/instance choice-explanations)
  (defvar*/instance remaining-phases)
  (defvar*/instance results)
  (defvar* phase)
  (defvar* score))
(defvar why)

(define phases
  '(basic selfish angels))

(defstep (initialize-lecture)
  (when (undefined? choices)
    (set! choices (hash)))
  (when (undefined? results)
    (set! results (hash)))
  (when (undefined? score)
    (set! score 0))
  (skip))

; TODO: update for lecture (not game) instructions.
(defstep (instructions)
  (define actions (hash-ref grade-game-form 'actions))
  @md{# Grade Game

      ## Instructions

      You will be paired with a random person from this class, and neither of you will find out with whom you were paired.

      Both of you can take an action by choosing either @(~a (first actions)) or @(~a (second actions)), which determine the outcomes as follows:

      @`(ul
         ,@(for*/list ([a1 actions]
                       [a2 actions])
             (define outcome
               (hash-ref grade-game-form (cons a1 a2)))
             (li (format "If you choose ~a and your pair chooses ~a, then you get a grade ~a and your pair a grade ~a." a1 a2 (car outcome) (cdr outcome)))))

      @button{Continue}})

(defstep (lecture-end)
  @md{# The End

      Thanks for participating today.})

; The Variations of the Grade Game

(defstep (match-waiter)
  @md{# Please Wait

      Please wait while another participant joins the queue.

      @refresh-every[5]})

(defstep matchmake
  (let ([matchmaker (make-matchmaker 2)])
    (lambda ()
      (matchmaker match-waiter))))

(define (autofill-ask-why)
  (bot:autofill 'test))

(defstep (ask-why)
  @md{# Choice Explanation

      @form[#:bot ([test (#:why "Bot does what bot told")])]{
        @div{
          @set![why (input-text "Why did you choose the way that you did?")]}
        @submit-button}})

#;(define (make-grade-games)
  (for/study ([p '(basic selfish)])
    (defvar phase)

    (defstep (init)
      (set! phase p)
      (skip))

    (defstep (show-phase)
      @md{# Current Phase: @(~a phase)

          @button{Continue}})

    (defstudy grade-game/phase
      [init --> show-phase --> ,(lambda () done)])

    grade-game/phase))

(define ((take-owner-to step-id))
  (if (current-participant-owner?)
      (skip step-id)
      (skip)))

(defstep (admin/lecture)
  (define (finish-current-phase)
    (with-study-transaction
      (set! completed-phases
            (cons (car remaining-phases) completed-phases))
      (set! remaining-phases
            (cdr remaining-phases))))

  (define (init-game)
    (when (undefined? remaining-phases)
      (set! remaining-phases phases))
    (when (undefined? completed-phases)
      (set! completed-phases '())))

  @md{# Admin

      ## Phase

      @(cond [(undefined? remaining-phases)
              @md*{
                The game hasn't started yet.
                @button[init-game]{Start Game}
              }]

             [(null? remaining-phases)
              @md*{All phases are complete.}]

             [else
              @md*{
                The current phase is @(~a (car remaining-phases)).

                @button[finish-current-phase]{Finish Current Phase}}])})

; TODO: Should this be made visible at the transition level?
(defstep (wait-for-next-phase-or-end)
  (cond [(null? remaining-phases)
         (skip 'lecture-end)]

        [(not (member phase completed-phases))
         @md{# Wait until the next phase starts

             @refresh-every[5]}]

        [else
         (set! phase (car remaining-phases))
         (skip phase)]))

(define (reset-group-and-done)
  (reset-current-group)
  done)

; Some games differ because they are asymmetric.

; Games differ in payoffs, thus in instructions and payoffs.

; Configurations

(define grade-game-form
  (hash
   'actions '(α β)
   '(α . α) '(B- . B-)
   '(α . β) '(A  . C+)
   '(β . α) '(C+ . A )
   '(β . β) '(B+ . B+)))

(define (selfish-utility ap)
  (match ap
   [(cons 'α  'α)  0]
   [(cons 'α  'β)  3]
   [(cons 'β  'α) -1]
   [(cons 'β  'β)  1]))

(define (angels-utility ap)
  (match ap
   [(cons 'α  'α)  0]
   [(cons 'α  'β) -1]
   [(cons 'β  'α) -3]
   [(cons 'β  'β)  1]))

; Steps
(define (make-choice)
  (define (α)
    (make-choice! 'α))
  (define (β)
    (make-choice! 'β))

  @md*{# Make Your Choice

       @style{
         .choice-button {
           width: 33%;
           font-size: 1.5rem;
         }
       }

       @button[#:id "α" #:class "choice-button" α]{α}
       @button[#:id "β" #:class "choice-button" β]{β}})

(defstep (make-choice/basic)
  @md{
    @(make-choice)

    ## Outcome Matrix

    @(outcome-matrix grade-game-form)})

(defstep (make-choice/selfish)
  @md{
    @(make-choice)

    ## Payoff Matrix

    @(payoff-matrix grade-game-form selfish-utility)})

(defstep (make-choice/angels)
  @md{
    @(make-choice)

    ## Payoff Matrix

    @(payoff-matrix grade-game-form angels-utility)})

(defstep (wait-for-other-player)
  (if (= (hash-count ((&hash-ref* (get-current-group)) choices)) 1)
      @md{# Please Wait

          Please wait for the other participant to make their choice...

          @refresh-every[5]}
      (skip)))

; TODO: Note that I am overwriting this when people refresh the page. For PD that's innocuous.
(define (store-results! v)
  (with-study-transaction
    (parameterize ([current-hash-maker hash])
      (set! results
            ((&opt-hash-ref*
              phase
              (get-current-group))
             results
             v)))))

(defstep ((display-result [utility #f]))
  (define actions (chosen-action-profile))
  (define (update-score!)
    (if utility
        (set! score (+ score (utility actions)))
        (set! score (add1 score))))
  (define outcome
    (outcomes grade-game-form actions))
  (store-results!
   (hash 'self (car actions)
         'other (cdr actions)
         'outcome outcome
         'payoff (and utility (utility actions))))

  @md{# Result

      The outcome is @~a[outcome].

      @(if utility
        @md*{Your payoff from this is @(~a (utility actions)).}
        @div{})

      @button[update-score!]{Continue}})

(defstudy grade-game/basic
  [matchmake
   --> make-choice/basic
   --> wait-for-other-player
   --> [display-result/basic (display-result)]
   --> ,reset-group-and-done])

(defstep (intro/selfish)
  @md{# Selfish Players

      Now suppose both players - you and the other - have selfish preferences, so that each cares only about their own grade and wants to get the highest grade.

      ## Payoff matrix

      This can be represented by the following payoff matrix:

      @(payoff-matrix grade-game-form selfish-utility)

      @button{Continue}})

(defstep (intro/angels)
  @md{# Indignant Angels

      Now suppose both players - you and the other - are indignant angels, so that if the two players choose differently, they feel worse:

      - The player who gets a higher grade feels guilty.
      - The player who gets a lower grade feels indignant.

      ## Payoff matrix

      This can be represented by the following payoff matrix:

      @(payoff-matrix grade-game-form angels-utility)

      @button{Continue}})

(defstudy grade-game/selfish
  [intro/selfish
   --> matchmake
   --> make-choice/selfish
   --> wait-for-other-player
   --> [display-result/selfish (display-result selfish-utility)]
   --> ,reset-group-and-done])

(defstudy grade-game/angels
  [intro/angels
   --> matchmake
   --> make-choice/angels
   --> wait-for-other-player
   --> [display-result/angels (display-result angels-utility)]
   --> ,reset-group-and-done])

(define (wait-for-start)
  (cond [(undefined? remaining-phases)
         @md{# Wait until game starts

             @refresh-every[5]}]

        [(not (null? remaining-phases))
         (set! phase (car remaining-phases))
         (skip phase)]

        [else
         (skip 'lecture-end)]))

(defstep (store-identity)
  (put/identity 'grade-game-lecture score)
  (skip))

(defstudy grade-game-lecture
  [initialize-lecture
   --> (check-admin (take-owner-to 'admin/lecture))
   --> instructions
   --> wait-for-start
   --> wait-for-start]

  [wait-for-start --> wait-for-start]

  [(basic grade-game/basic) --> ,(lambda () 'ask-why)]

  [(selfish grade-game/selfish) --> ,(lambda () 'ask-why)]

  [(angels grade-game/angels) --> ,(lambda () 'ask-why)]

  [{ask-why (with-bot ask-why autofill-ask-why)} --> wait-for-next-phase-or-end --> store-identity]

  [store-identity --> lecture-end --> lecture-end]

  [admin/lecture --> admin/lecture])

;;; BOTS
;;; TODO: Should be for general 2x2 games

(provide
 make-grade-game-bot
 grade-game-model)

(with-namespace xyz.trichotomy.congame.grade-game
  (defvar* bot-behavior)
  (defvar* bot-group-id))
(defvar bot-spawned?)
(defvar bot-spawn-deadline)

(defstep (waiter-for-bot)
  (unless (if-undefined bot-spawn-deadline #f)
    (set! bot-spawn-deadline (+ (current-seconds) 10)))
  (unless (or
           (current-user-bot?)
           (if-undefined bot-spawned? #f)
           (< (current-seconds) bot-spawn-deadline))
    (spawn-bot
     (make-grade-game-bot
      (make-grade-game-spawn-model
       (get-current-group))))
    (set! bot-spawned? #t))

  @md{# Please Wait

      Please wait while another participant joins the queue.

      @refresh-every[5]})

(define (bot-group-ok? group-id)
  ;; humans always paired with bots:
  #;
  (and
   (current-user-bot?)
   (equal? bot-group-id group-id))
  ;; humans raced against humans paired with bots if bot already spawned:
  #;
  (if (current-user-bot?)
      (equal? bot-group-id group-id)
      (not bot-spawned?))
  ;; humans raced against humans paired with humans even if bot spawned:
  (or
   (not (current-user-bot?))
   (equal? bot-group-id group-id)))


(define make-grade-game-bot
  (bot:study->bot grade-game-lecture))

(define (grade-game-model k proc)
  (match k
    [`(*root* instructions)
     (set! bot-behavior (random-ref '(α β)))
     (proc)]
    [`(*root* ask-why)
     (proc)]
    [`(*root* lecture-end)
     (bot:completer)]
    [(list '*root* (or 'basic 'selfish 'angels) r)
     (case r
       [(make-choice/basic)
        (bot:click bot-behavior)]
       [(make-choice/selfish)
        (bot:click bot-behavior)]
       [(make-choice/angels)
        (bot:click bot-behavior)]
       [(matchmake wait-for-other-player)
        (sleep 1)]
       [else
        (proc)])]
    [`(*root*
       ,(or 'wait-for-start
            'wait-for-next-phase-or-end))
     (sleep 1)]

    [`(*root* matchmake)
     (sleep 1)]
    [`(*root* make-choice)
     (bot:click bot-behavior)]
    [`(*root* wait)
     (sleep 1)]
    [_ (proc)]))

(define ((make-grade-game-spawn-model group-to-join) k proc)
  (match k
    [`(*root* intro)
     (set! bot-group-id group-to-join)
     (grade-game-model k proc)]
    [_
     (grade-game-model k proc)]))
