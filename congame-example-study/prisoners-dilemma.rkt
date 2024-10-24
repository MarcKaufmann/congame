#lang conscript

(require conscript/survey-tools
         racket/match)

(provide
 prisoners-dilemma
 make-prisoners-dilemma-bot
 prisoners-dilemma-model)

;; For next time:
;; * Multiple participants & current owner in conscript/local
;; * Add bot support to conscript (default actions on all steps, figure out models)

(defvar*/instance choices dilemma-choices)
(defvar choice)

(define (make-choice! choice)
  (with-study-transaction
    (if (undefined? choices)
        (set! choices (hash (get-current-group) (hash (current-participant-id) choice)))
        (set! choices (hash-update
                       choices (get-current-group)
                       (λ (ht) (hash-set ht (current-participant-id) choice))
                       hash)))))

(defstep (intro)
  @md{# Prisoner's Dilemma

      @button{Continue...}})

(defstep (waiter)
  @md{# Please Wait

      Please wait while another participant joins the queue.

      @refresh-every[5]})

(defstep matchmake
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

(defstep (display-result)
  (define-values (my-choice other-choice)
    (for/fold ([my-choice #f]
               [other-choice #f])
              ([(k v) (in-hash (hash-ref choices (get-current-group)))])
      (if (equal? k (current-participant-id))
          (values v other-choice)
          (values my-choice v))))
  (set! choice
        (match* (my-choice other-choice)
          [('cooperate 'cooperate) 1]
          [('cooperate 'defect) 20]
          [('defect 'defect) 5]
          [('defect 'cooperate) 0]))

  @md{# Result

      You get @~a[choice] years of prison.})

(defstudy prisoners-dilemma
  [intro --> matchmake --> make-choice --> wait --> display-result]
  [display-result --> display-result])

(define make-prisoners-dilemma-bot
  (bot:study->bot prisoners-dilemma))

(defvar* behavior behavior)

(define (prisoners-dilemma-model k proc)
  (match k
    [`(*root* intro)
     (set! behavior (random-ref '(cooperate defect)))
     (proc)]
    [`(*root* matchmake)
     (sleep 1)]
    [`(*root* make-choice)
     (bot:click behavior)]
    [`(*root* display-result)
     (bot:completer)]
    [`(*root* wait)
     (sleep 1)]
    [_ (proc)]))
