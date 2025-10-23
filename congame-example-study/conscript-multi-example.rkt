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
         racket/list)

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

(define instructions-control
  @md*{You are in the **control group**.

    You will complete 2 simple arithmetic tasks. Your payment will be:
    - $1 base payment
    - $0.20 for each task you get correct

    Good luck!})

(define instructions-treatment
  @md*{You are in the **treatment group**.

    You will complete 2 simple arithmetic tasks. After finishing, you will be matched 
    with another participant in the treatment group. Your payment will depend on both 
    your scores:

    - **Winner** (higher score): $1 + $2.40 = $3.40
    - **Loser** (lower score): $1
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

(defvar task1-response)
(defvar task2-response)
(defvar score)

(defstep (init-tasks)
  (set! score 0)
  (skip))

(define-values (task1-form task1-onsubmit)
  (form+submit
   [task1-response (ensure binding/text (required))]))

(define (render-task1 rw)
  @md*{@rw["task1-response" @input-number{Your answer}]
    @|submit-button|})

(define-values (task2-form task2-onsubmit)
  (form+submit
   [task2-response (ensure binding/text (required))]))

(define (render-task2 rw)
  @md*{@rw["task2-response" @input-number{Your answer}]
    @|submit-button|})

;; Task 1: What is 7 + 5?
(defstep (task1)
  @md{# Task 1

    What is 7 + 5?

    @form[task1-form render-task1]})

;; Task 2: What is 15 - 6?
(defstep (task2)
  @md{# Task 2

    What is 15 - 6?

    @form[task2-form render-task2]})

(defstep (show-score)
  (set! score (+ (if (= task1-response 12) 1 0)
                 (if (= task2-response 9) 1 0)))

  @md{# Your Score

    You answered **@(~a score) out of 2** tasks correctly.

    @button{Continue}})

;; =============================================================================
;; CONTROL GROUP PAYMENT
;; =============================================================================

(defstep (control-payment)
  (define payment (+ 1.00 (* score 0.20)))
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
  (define other-score (first (other-group-member-results 'score)))
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
  (define payment (+ 1.0 (if did-win? 2.4 0)))
  
  @md{# Match Results

    **Your score:** @(~a score) out of 2
    **Opponent's score:** @(~a opponent-score) out of 2
    
    **You @(if did-win? "🎉 WON!" "lost this round.")**
    
    Your payment is:
    - $1.00 base
    - @(if did-win? "$2.40 for winning" "$0 (you lost)")
    
    **Total: $@(~$ payment)**

    Thank you for participating!})

;; =============================================================================
;; MAIN STUDY FLOW
;; =============================================================================

(defstudy simple-treatment-study
  [assign-treatment
   --> instructions
   --> init-tasks
   --> task1
   --> task2
   --> show-score
   --> ,(lambda ()
          (if is-treatment? 'pair-with-someone 'control-payment))]
  
  [control-payment --> control-payment]
  
  [pair-with-someone
   --> get-opponent-score
   --> treatment-results])