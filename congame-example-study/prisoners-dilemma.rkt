#lang conscript

(require congame-web/components/study-bot
         conscript/survey-tools
         data/monocle
         racket/match)

(provide
 prisoners-dilemma
 make-prisoners-dilemma-bot
 prisoners-dilemma-model)

(defvar/instance choices)
(defvar choice)

(define (&my-choice)
  (parameterize ([current-hash-maker hash])
    (&opt-hash-ref*
     (get-current-group)
     (current-participant-id))))

(define (make-choice! choice)
  (with-study-transaction
    (set! choices ((&my-choice) (if-undefined choices (hash)) choice))))

(defstep (intro)
  @md{# Prisoner's Dilemma

      @button{Continue...}})

(with-namespace xyz.trichotomy.congame.prisoners-dilemma
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
     (make-prisoners-dilemma-bot
      (make-prisoners-dilemma-spawn-model
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
  ;; match with bots based on bots-group-ok?
  #;(let ([matchmaker (make-matchmaker 2 bot-group-ok?)])
    (lambda ()
      (matchmaker waiter)))
  (let ([matchmaker (make-matchmaker 2)])
    (lambda ()
      (matchmaker waiter))))

(defstep (make-choice)
  (define (cooperate)
    (make-choice! 'cooperate))
  (define (defect)
    (make-choice! 'defect))

  @md{# Make Your Choice

      @button[#:id "cooperate" cooperate]{Cooperate}
      @button[#:id "defect" defect]{Defect}})

(defstep (wait)
  (if (= (hash-count (hash-ref choices (get-current-group))) 1)
      @md{# Please Wait

          Please wait for the other participant to make their choice...

          @refresh-every[5]}
      (skip)))

(defvar result)
(defvar current-round)

(defstep (display-result)
  (define-values (my-choice other-choice)
    (for/fold ([my-choice #f]
               [other-choice #f])
              ([(k v) (in-hash (hash-ref choices (get-current-group)))])
      (if (equal? k (current-participant-id))
          (values v other-choice)
          (values my-choice v))))
  (set! result
        (match* (my-choice other-choice)
          [('cooperate 'cooperate) 1]
          [('cooperate 'defect) 20]
          [('defect 'defect) 5]
          [('defect 'cooperate) 0]))
  ;; NOTE: `current-round` is for repeated version of PD
  (set! current-round
        (hash 'self my-choice
              'other other-choice
              'result result))

  @md{# Result

      You get @~a[result] years of prison.

      @button{Continue}})

(defstudy prisoners-dilemma
  [intro --> matchmake --> make-choice --> wait --> display-result]
  [display-result --> display-result])

;;;; REPEATED PRISONER'S DILEMMA

(defvar round)
(defvar answers)

(defstep (store-round)
  (set! answers
        (cons current-round (if-undefined answers '())))
  (reset-current-group)
  (skip))

(defstep (the-end)
  @md{# The End

      The results:

      @`@(ol
          ,@(for/list ([a (reverse answers)])
              (li (format "(~a, ~a): you got ~a years"
                          (hash-ref a 'self)
                          (hash-ref a 'other)
                          (hash-ref a 'result)))))
      })

(provide
 repeated-prisoners-dilemma)

(defstudy repeated-prisoners-dilemma
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
  [the-end --> the-end])

(define make-prisoners-dilemma-bot
  (bot:study->bot prisoners-dilemma))

(define (prisoners-dilemma-model k proc)
  (match k
    [`(*root* intro)
     (set! bot-behavior (random-ref '(cooperate defect)))
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

(define ((make-prisoners-dilemma-spawn-model group-to-join) k proc)
  (match k
    [`(*root* intro)
     (set! bot-group-id group-to-join)
     (prisoners-dilemma-model k proc)]
    [_
     (prisoners-dilemma-model k proc)]))
