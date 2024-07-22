#lang conscript

(require buid
         conscript/survey-tools
         racket/match)

(provide
 prisoners-dilemma)

(defvar*/instance pending-group matchmaking:pending-group)
(defvar*/instance ready-groups matchmaking:known-groups)
(defvar* current-group matchmaking:current-group)

(defvar*/instance choices dilemma-choices)
(defvar choice)

(define (make-choice! choice)
  (with-study-transaction
    (if (undefined? choices)
        (set! choices (hash current-group (hash (current-participant-id) choice)))
        (set! choices (hash-set
                       (hash-ref choices current-group)
                       (current-participant-id) choice)))))

(defstep (intro)
  @md{# Prisoner's Dilemma

      @button{Continue...}})

(defstep (matchmake)
  (if (member current-group (if-undefined ready-groups null))
      (skip)
      (with-study-transaction
        (cond
          [(not (undefined? current-group))
           (void)]
          [(if-undefined pending-group #f)
           (set! current-group pending-group)
           (set! ready-groups (cons pending-group (if-undefined ready-groups null)))
           (set! pending-group #f)]
          [else
           (set! current-group (buid))
           (set! pending-group current-group)])
        @md{# Please Wait

            Please wait while another participant joins the queue.

            @refresh-every[5]})))

(defstep (make-choice)
  (define (cooperate)
    (make-choice! 'cooperate))
  (define (defect)
    (make-choice! 'defect))

  @md{# Make Your Choice

      @button[cooperate]{Cooperate}
      @button[defect]{Defect}})

(defstep (wait)
  (if (= (hash-count choices) 1)
       @md{# Please Wait

           Please wait for the other participant to make their choice...

           @refresh-every[5]}
       (skip)))

(defstep (display-result)
  (define-values (my-choice other-choice)
    (for/fold ([my-choice #f]
               [other-choice #f])
              ([(k v) (in-hash (hash-ref choices current-group))])
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
