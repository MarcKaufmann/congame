#lang conscript

;; Simple Multi-Participant Study
;; 
;; Treatment Assignment:
;;   - Balanced randomization: 2 control, 2 treatment per group of 4
;; 
;; Tasks:
;;   - 2 simple arithmetic tasks (can be correct or incorrect)
;; 
;; Payment:
;;   - Control: $1 baseline + $0.20 per correct task
;;   - Treatment: Matched with another treatment participant
;;     - Winner (higher score): $1 + $2.40
;;     - Loser (lower score): $1
;;     - Tie: 50-50 random winner selection

(require conscript/form0
         conscript/survey-tools
         racket/list
         racket/match)

(provide simple-treatment-study)

;; =============================================================================
;; TREATMENT ASSIGNMENT
;; =============================================================================

(defvar/instance treatments)
(defvar is-treatment?)

(defstep (assign-treatment)
  ; Balanced: out of every 4 participants, assign 2 to treatment, 2 to control
  (with-study-transaction
      (when (or (undefined? treatments) (null? treatments))
        (set! treatments (shuffle '(#t #t #f #f))))
    (set! is-treatment? (first treatments))
    (set! treatments (rest treatments)))
  (skip))

;; =============================================================================
;; INSTRUCTIONS
;; =============================================================================
(define questions-and-answers
  '(("What is 7 + 5?" "12")
    ("What is six less than fifteen?" "9")
    ("Which day is nine days after Tuesday?" "Thursday")
    ("What word starts with “A” and is the enemy of a protagonist?" "Antagonist")))

(define quiz-length (length questions-and-answers))

(define instructions-control
  @md*{You are in the **control group**.

    You will answer @(~a quiz-length) simple questions. Your payment will be:
    - $1 base payment
    - $0.20 for each answer you get correct

    Good luck!})

(define instructions-treatment
  @md*{You are in the **treatment group**.

    You will answer @(~a quiz-length) simple questions. After finishing, you will be matched 
    with another participant in the treatment group. Your payment will depend on both 
    your scores:

    - **Winner** (higher score): $1 + $2.40 = $3.40
    - **Loser** (lower score): $1.00
    - **Tie**: 50-50 chance of winning

    **Important:** After you complete the tasks, you may need to wait briefly while 
    we match you with another participant. Please be patient and wait for another
    participant to complete their tasks so the two of you can be matched.})

(defstep (instructions)
  @md{# Instructions

    @(if is-treatment? instructions-treatment instructions-control)

    @button{Begin}})

;; =============================================================================
;; TASKS
;; =============================================================================

(defvar task-responses)
(defvar current-response)

(defvar score)
(defvar payment)

(defstep (init-tasks)
  (set! score 0)
  (set! payment 0)
  (set! task-responses '())
  (skip))

;; Factory for making similar tasks
(define quiz-task-funcs
  (for/list ([task (in-list questions-and-answers)])
    (match-define (list question answer) task)
    (define-values (task-form task-onsubmit)
      (form+submit
       [current-response (ensure binding/text (required))]))
    (define (renderer rw)
      @md*{
        @rw["current-response" @input-text{Your answer}]

        @submit-button})
    (list task-form task-onsubmit renderer)))

(define (task-step-content task-n)
  (define idx (sub1 task-n))
  (match-define (list t-form t-onsubmit t-render) (list-ref quiz-task-funcs idx))
  (match-define (list question _answer) (list-ref questions-and-answers idx))
  @md{
    # Task @(~a task-n)

    @question

    @form[t-form t-onsubmit t-render]})

(defstep (quiz-tasks)
  (define task-number (add1 (length task-responses)))
  (task-step-content task-number))

(defstep (show-score)
  (set! score
        (for/sum ([(answer idx) (in-indexed task-responses)])
          (match-define (list _q correct-answer) (list-ref questions-and-answers idx))
          (if (string=? (string-downcase answer)
                        (string-downcase correct-answer)) 1 0)))
  
  @md{# Your Score

    You answered **@(~a score) out of @(~a quiz-length) ** tasks correctly.

    @button{Continue}})

;; =============================================================================
;; CONTROL GROUP PAYMENT
;; =============================================================================

(defstep (control-payment)
  (set! payment (+ 1.00 (* score 0.20)))
  @md{# Your Payment

    Your payment is:
    - $1.00 base
    - @(~$ (* score 0.20)) for @(~a score) correct task(s)
    
    **Total: @(~$ payment)**

    Thank you for participating!})

;; =============================================================================
;; TREATMENT GROUP: MATCHMAKING & RESULTS
;; =============================================================================

(defvar opponent-score)
(defvar did-win?)

(define matchmaker (make-matchmaker 2))

;; Wait for a match using the matchmaking module
(defstep (wait-for-match)
  @md{# Waiting for Match

    You have finished the tasks. We are now matching you with another participant...

    Please wait.

    @refresh-every[5]})

(defstep (pair-with-someone)
  (matchmaker wait-for-match))

(defstep (record-score-for-group)
  (store-my-result-in-group! 'score score)
  (skip))

(defstep (get-opponent-score)
  (define other-score (first (current-group-member-results 'score)))
  (cond
    [other-score
     (set! opponent-score other-score)
     ; Determine winner
     (set! did-win?
           (or (and (= score opponent-score)
                    (> (random 2) 0))
               (> score opponent-score)))     
     (skip)]
    [else
     @md{# Please wait

       Waiting to learn your opponent’s score…

       @refresh-every[2]}]))

(defstep (treatment-results)
  (set! payment (+ 1.0 (if did-win? 2.4 0)))
  
  @md{# Match Results

    **Your score:** @(~a score) out of @(~a quiz-length)
    **Opponent's score:** @(~a opponent-score) out of @(~a quiz-length)
    
    **You @(if did-win? "🎉 WON!" "lost this round.")**
    
    Your payment is:
    - $1.00 base
    - @(if did-win? "$2.40 for winning" "$0.00 (you lost)")
    
    **Total: @(~$ payment)**

    Thank you for participating!})

;; =============================================================================
;; MAIN STUDY FLOW
;; =============================================================================

(defstudy simple-treatment-study
  [assign-treatment
   --> instructions
   --> init-tasks
   --> quiz-tasks
   --> ,(lambda ()
          (set! task-responses (append task-responses (list current-response)))
          (cond
            [(= (length task-responses) quiz-length) 'show-score] ; finished the quiz!
            [else 'quiz-tasks]))]
  [show-score
   --> ,(lambda ()
          (if is-treatment? 'pair-with-someone 'control-payment))]
  
  [control-payment --> control-payment]
  
  [pair-with-someone
   --> record-score-for-group
   --> get-opponent-score
   --> treatment-results]
  [treatment-results --> treatment-results])