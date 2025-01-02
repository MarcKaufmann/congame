#lang conscript

(require congame-web/components/study-bot
         conscript/survey-tools
         data/monocle
         racket/match)

(provide
 make-grade-game-bot
 grade-game-model)


(with-namespace xyz.trichotomy.congame.grade-game
  (defvar*/instance choices))
(defvar choice)

(define (&my-choice)
  (parameterize ([current-hash-maker hash])
    (&opt-hash-ref*
     phase
     (get-current-group)
     (current-participant-id))))

(define (make-choice! choice)
  (with-study-transaction
    (set! choices ((&my-choice) choices choice))))

(defstep (initialize)
  (when (undefined? choices)
    (set! choices (hash)))
  (skip))

(define grade-game-form
  (hash
   'actions '(α β)
   '(α . α) '(B- . B-)
   '(α . β) '(A  . C+)
   '(β . α) '(C+ . A )
   '(β . β) '(B+ . B+)))

(define (outcome-matrix game-form)
  (define actions
    (hash-ref game-form 'actions))
  (define a1 (first actions))
  (define a2 (second actions))
  (define (outcome a b)
    (define r
      (hash-ref game-form (cons a b)))
    (format "~a, ~a" (car r) (cdr r)))

  @html*{
    @style{
      table.outcome-matrix {
        border-collapse: collapse;
        text-align: center;
      }
      .outcome-matrix thead th {
        border: none;
        padding: 1rem 2rem;
        font-weight: bold;
      }
      .outcome-matrix tbody th {
        text-align: left;
        padding: 1rem 2rem;
      }
      .outcome-matrix td {
        border: 1px solid #000;
        padding: 8px 16px;
        font-size: 1rem;
        width: 6rem;
      }
    }

    @table[#:class "outcome-matrix"]{
      @thead{
        @tr{
          @th{} @th{@(~a a1)} @th{@(~a a2)}}}
      @tbody{
        @tr{
          @th{@(~a a1)} @td{@(outcome a1 a1)} @td{@(outcome a1 a2)}}
        @tr{
          @th{@(~a a2)} @td{@(outcome a2 a1)} @td{@(outcome a2 a2)}}}}})

(define (instructions [title "Instructions"])
  (define actions (hash-ref grade-game-form 'actions))

  @md*{## @title

       You will be paired with a random person from this class, and neither of you will find out with whom you were paired.

       Both of you can take an action by choosing either @(~a (first actions)) or @(~a (second actions)), which determine the outcomes as follows:

       @`(ul
          ,@(for*/list ([a1 actions]
                        [a2 actions])
              (define outcome
                (hash-ref grade-game-form (cons a1 a2)))
              (li (format "If you choose ~a and your pair chooses ~a, then you get a grade ~a and your pair a grade ~a." a1 a2 (car outcome) (cdr outcome)))))})

(defstep (intro)
  @md{# Grade Game

      @(instructions)

      ## Outcome Matrix for Phase @(string-titlecase (~a phase))

      @(outcome-matrix grade-game-form)

      @button{Continue}})


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

(defstep (waiter)
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

(defstep matchmake
  (let ([matchmaker (make-matchmaker 2)])
    (lambda ()
      (matchmaker waiter))))

(defstep (make-choice)
  (define (α)
    (make-choice! 'α))
  (define (β)
    (make-choice! 'β))

  @md{# Make Your Choice

      @style{
        .choice-button {
          width: 33%;
          font-size: 1.5rem;
        }
      }

      @button[#:id "α" #:class "choice-button" α]{α}
      @button[#:id "β" #:class "choice-button" β]{β}

      @toggleable-xexpr["Show/Hide Instructions" (instructions)]})

(defstep (wait)
  (if (= (hash-count ((&hash-ref* phase (get-current-group)) choices)) 1)
      @md{# Please Wait

          Please wait for the other participant to make their choice...

          @refresh-every[5]}
      (skip)))

(defvar result)
(defvar current-round-data)

(defstep (display-result)
  (define-values (my-choice other-choice)
    (for/fold ([my-choice #f]
               [other-choice #f])
              ([(k v) (in-hash ((&opt-hash-ref* phase (get-current-group)) choices))])
      (if (equal? k (current-participant-id))
          (values v other-choice)
          (values my-choice v))))
  (set! result
        (car (hash-ref grade-game-form (cons my-choice other-choice))))

  ;; NOTE: `current-round-data` is for repeated version of PD
  (set! current-round-data
        (hash 'self my-choice
              'other other-choice
              'result result))

  @md{# Result

      You get a grade of @~a[result].

      @button{Continue}})

;;;; REPEATED PRISONER'S DILEMMA

(defvar round)
(defvar answers)

(defstep (store-round)
  (set! answers
        (cons current-round-data (if-undefined answers '())))
  (reset-current-group)
  (skip))

(defstep (the-end/repeated)
  @md{# The End

      The results:

      @`(ol
          ,@(for/list ([a (reverse answers)])
              (li (format "(~a, ~a): you got ~a years"
                          (hash-ref a 'self)
                          (hash-ref a 'other)
                          (hash-ref a 'result)))))
      })

(provide
 repeated-grade-game)

(defstudy repeated-grade-game
  [intro --> matchmake
         --> make-choice
         --> wait
         --> display-result
         --> store-round
         --> ,(lambda ()
                (define n 3)
                (set! round (add1 (if-undefined round 0)))
                (if (< round n)
                    'matchmake
                    'the-end))]
  [(the-end the-end/repeated) --> the-end])

;;; PD with Admin

(define ((take-owner-to step-id))
  (if (current-participant-owner?)
      (skip step-id)
      (skip)))

(defstep (admin)
  (define outcomes
    (for/list ([(_ v) (in-hash choices)])
      (hash-values v)))
  (define results
    (for/fold ([rs (hash)])
              ([o outcomes])
      (hash-update rs o add1 0)))

  @md{# Admin

      ## Results

      @`(ul
         ,@(for/list ([(k v) results])
             (li (format "Outcome ~a occurred ~a times" k v))))})

;; LECTURE 1

(provide
 grade-game/lecture)

(with-namespace xyz.trichotomy.congame.grade-game
  (defvar*/instance completed-phases)
  (defvar*/instance choice-explanations)
  (defvar*/instance remaining-phases)
  (defvar* phase))
(defvar why)

(define phases
  '(basic selfish))

(defstep (initialize-lecture)
  (when (undefined? choices)
    (set! choices (hash)))
  (skip))

#;(defstep ((wait-until-phase phase))
  ; NOTE: random wait time so that it is less likely that players get rematched with the same person next time.
  ; TODO: This is hacky, but good enough for now. Have proper rematching.
  (define wait-time
    (+ 4.5 (random)))
  (cond [(member phase completed-phases)])
  @md{# Wait until the start of the next phase

      @refresh-every[wait-time]})

(defstep (lecture-end)
  @md{# The End

      Thanks for participating today.})

(defstep (ask-why)
  @md{# Choice Explanation

      @form{
        @div{
          @set![why (input-text "Why did you choose the way that you did?")]}
        @submit-button}})

(define (make-grade-games)
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

(defstep (wait-for-next-phase-or-end)
  (cond [(undefined? remaining-phases)
         @md{# Wait until game starts

             @refresh-every[5]}]

        [(null? remaining-phases)
         (skip 'lecture-end)]

        [(equal? phase (car remaining-phases))
         @md{# Wait until the next phase starts

             @refresh-every[5]}]

        [else
         (set! phase (car remaining-phases))
         (skip)]))

(defstudy grade-game
  [intro --> matchmake --> make-choice --> wait --> display-result --> ,(lambda ()
                                                                          (reset-current-group)
                                                                          done)])

(defstudy grade-game/lecture
  [initialize-lecture
   --> (check-admin (take-owner-to 'admin/lecture))
   --> wait-for-next-phase-or-end]

  [wait-for-next-phase-or-end
   --> grade-game
   --> ask-why
   --> wait-for-next-phase-or-end]

  [lecture-end --> lecture-end]

  [admin/lecture --> admin/lecture])

;;; BOTS

(define make-grade-game-bot
  (bot:study->bot grade-game))

(define (grade-game-model k proc)
  (match k
    [`(*root* intro)
     (set! bot-behavior (random-ref '(α β)))
     (proc)]
    [`(*root* matchmake)
     (sleep 1)]
    [`(*root* make-choice)
     (bot:click bot-behavior)]
    [`(*root* display-result)
     (bot:completer)]
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
