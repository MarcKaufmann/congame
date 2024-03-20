#lang conscript

(require racket/list
         racket/random
         conscript/survey-tools)

(provide
 several-payments-random-task)

(define completion-fee 1.00)

(defstep (welcome)
  (define (initialize)
    (put 'payments '(2 3 4)))

  @md{# Welcome

      @button[initialize]{Next}})

(defstep (how-many-tasks)
  (define payments
    (get 'payments))
  (define payment
    (first payments))
  (define (on-submit #:n-tasks n-tasks)
    (define old-answers
      (get 'answers '()))
    (put 'answers
         (cons
           (list payment n-tasks)
           old-answers))
    (put 'payments
         (rest payments)))

  @md{# Tasks

      @form[#:action on-submit]{
        @input-number[#:n-tasks #:min 0 #:max 40]{@md*{How many tasks are you willing
        to do for @(~a payment)?}}

        @submit-button}})

(define (initialize-tasks)
  (put 'remaining-tasks (get 'tasks-that-count)))

(define (display-payment)
  (define maybe-choice-that-counts
    (get 'choice-that-counts #f))

  (define choice-that-counts
    (cond [maybe-choice-that-counts
           maybe-choice-that-counts]

          [else
           (define r
             (random-ref (get 'answers)))
           (put 'choice-that-counts r)
           r]))

  (define payment-that-counts
    (first choice-that-counts))

  (define tasks-that-count
    (second choice-that-counts))

  (put 'payment-that-counts payment-that-counts)
  (put 'tasks-that-count tasks-that-count)

  @md{# The Chosen Payment

      The following choice was picked randomly picked as the choice that counts:

      - Tasks to do: @(~a tasks-that-count)
      - Payment: @(~a payment-that-counts)

      @button[initialize-tasks]{Continue to tasks}
})

(define (tasks)
  (define n
    (get 'tasks-that-count))

  ; Starting point for slider
  (define s
    (number->string (random 100)))

  @md{@slider-js
      # Do @n Slider Tasks

      @form{
        @div[#:class "slider"]{
          @input-range[
            #:slider
            #:attributes `([value ,s])
        ] @span{Value: @output{}}
        }
        @submit-button
      }

      @button[process-submission]{Next}})

(define (process-submission)
  (define answer
     (get 'slider))

  (define old-score
    (get 'score 0))

  (define new-score
    (if (= answer 50)
      (add1 old-score)
      old-score))

  (put 'score new-score)

  (put 'remaining-tasks
       (sub1 (get 'remaining-tasks)))

  (skip))

(define (end)
  (define payment
    (get 'payment-that-counts))

  (define total-payment
    (+ payment completion-fee))

  (define score
    (get 'score))

  @md{# The End

      Thanks, you will receive the payment of @(~pound total-payment).

      You got @(~a score) tasks right.})

(defstudy several-payments-random-task
  [welcome --> how-many-tasks
           --> ,(lambda ()
                  (if (null? (get 'payments))
                      'display-payment
                      'how-many-tasks))]

  [display-payment --> ,(lambda ()
                          (if (> (get 'tasks-that-count) 0) 'tasks 'end))]

  [tasks --> process-submission
         --> ,(lambda ()
                (if (> (get 'remaining-tasks) 0)
                    'tasks
                    'end))]

  [end --> end])
