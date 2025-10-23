#lang conscript

;; TODO: Port? to form0

(require conscript/form
         hash-view
         racket/random)

(provide
 decoding-task)

;;;; A single task.
;;;; `answer-correct?` "returns" whether the task was correctly solved.
;;;; This generates the tasks itself, there is no control over the sequences asked for.

(provide
 answer-correct?)

(defvar* answer-correct? answer-correct-id)

(defvar answer)
(hash-view task (text encoding))
(defvar current-task)

(defstep (init)
  (define a-z "abcdefghijklmnopqrstuvwxyz")
  (define code-letters (random-sample a-z 10 #:replacement? #f))
  (define encoding
    (for/hash ([i (in-range 10)]
               [c code-letters])
    (values c i)))
  (set! answer-correct? undefined)
  (set! current-task
        (task (random-sample code-letters 12) encoding))
  (skip))

(defstep (decoding-task)
  (define encoding
    (task-encoding current-task))
  (define code-letters
    (hash-keys encoding))
  @md{# Decoding Table

      @table{
             @tbody{
                    @(apply
                      tr
                      (cons
                       (td "Text sequence")
                       (for/list ([c code-letters])
                         (td (string c)))))
                    @(apply
                      tr
                      (cons
                       (td "Corresponds to")
                       (for/list ([c code-letters])
                         (td (~a (hash-ref encoding c))))))}}

      @div{
           Text sequence: @span[#:class "to-decode"]{@(apply string (task-text current-task))}}

      @form{
            @div{Your answer: @(set! answer (input-number))}
            @submit-button}})

(define (correct-answer t)
  (string->number
   (string-join
    (for/list ([c (task-text t)])
      (number->string
       (hash-ref (task-encoding t) c)))
    "")))

(defstep (check-task)
  (eprintf "correct answer of current task ~a is: ~a" current-task (correct-answer current-task))
  (set! answer-correct?
    (= answer (correct-answer current-task)))
  (skip))

(defstudy do-one-task
  [init --> decoding-task
        --> check-task
        --> ,(lambda () done)])

;;;; Repetition of Tasks

(provide
 do-tasks)

(defvar n)

(defstep (init-all)
  (set! n 5)
  (set! answer-correct? undefined)
  (skip))

(define transition-for-tasks-to-do
  (lambda ()
    (when (equal? answer-correct? #t)
      (set! n (sub1 n)))
    (eprintf "n is now ~a" n)
    (if (> n 0)
        'do-one-task
        'the-end)))

#;(defstep (update-task)
  (when answer-correct?
    (set! n (sub1 n)))
  (cond [answer-correct?
         @md{# Good Job!

             You have still @(~a n) tasks left to do.

             @button{Continue}}]

        [else
         @md{# You SUCK!

             You have still @(~a n) tasks left to do. LOL!

             @button{Continue}}]))

(defstep (the-end)
  @md{# The End})

(defstudy do-tasks
  [init-all --> ,transition-for-tasks-to-do]
  [do-one-task --> ,transition-for-tasks-to-do]
  [the-end --> the-end])
