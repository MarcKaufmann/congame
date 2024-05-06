#lang conscript

(require conscript/survey-tools
         racket/list
         racket/random)

(provide
 several-payments-random-task)

(defvar payments)
(defvar answers)
(defvar slider)
(defvar score)
(defvar remaining-tasks)
(defvar tasks-that-count)
(defvar payment-that-counts)
(defvar choice-that-counts)
(define completion-fee 1.00)

(defstep (welcome)
  (define (initialize)
    (set! payments '(2 3 4)))

  @md{# Welcome

      @button[initialize]{Next}})

(defstep (how-many-tasks)
  (define payment
    (first payments))
  (define (on-submit #:n-tasks n-tasks)
    (set! answers
          (cons
           (list payment n-tasks)
           (if-undefined answers null)))
    (set! payments (rest payments)))

  @md{# Tasks

      @form[#:action on-submit]{
        @input-number[#:n-tasks #:min 0 #:max 40]{@md*{How many tasks are you willing
        to do for @(~a payment)?}}

        @submit-button}})

(define (initialize-tasks)
  (set! remaining-tasks tasks-that-count))

(define (display-payment)
  (define the-choice
    (if-undefined
     choice-that-counts
     (let ([r (random-ref answers)])
       (begin0 r
         (set! choice-that-counts r)))))

  (set! payment-that-counts (first the-choice))
  (set! tasks-that-count (second the-choice))

  @md{# The Chosen Payment

      The following choice was picked randomly picked as the choice that counts:

      - Tasks to do: @(~a tasks-that-count)
      - Payment: @(~a payment-that-counts)

      @button[initialize-tasks]{Continue to tasks}})

(define (tasks)
  ; Starting point for slider
  (define s
    (number->string (random 100)))

  @md{@slider-js
      # Do @tasks-that-count Slider Tasks

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
  (when (= slider 50)
    (set! score (add1 (if-undefined score 0))))
  (set! remaining-tasks (sub1 remaining-tasks))
  (skip))

(define (end)
  (define total-payment
    (+ payment-that-counts completion-fee))
  (when (undefined? score)
    (set! score 0))

  @md{# The End

      Thanks, you will receive the payment of @(~pound total-payment).

      You got @(~a score) tasks right.})

(defstudy several-payments-random-task
  [welcome --> how-many-tasks
           --> ,(lambda ()
                  (if (null? payments)
                      'display-payment
                      'how-many-tasks))]

  [display-payment --> ,(lambda ()
                          (if (> tasks-that-count 0) 'tasks 'end))]

  [tasks --> process-submission
         --> ,(lambda ()
                (if (> remaining-tasks 0)
                    'tasks
                    'end))]

  [end --> end])
