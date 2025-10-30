#lang conscript

(require conscript/survey-tools
         racket/match)

(defvar my-choice)
(defvar their-choice)
(defvar prison-sentence)

(provide prisoners-dilemma)

(defstep (intro)
  @md{# Prisoner's Dilemma
 
    You are a suspect in a crime investigation. Your accomplice (another
    study participant) has also been arrested and is being held separately.
    Each of you has a choice: will you attempt to **cooperate** with your
    accomplice by staying silent, or will you **defect** and admit everything
    to the police, betraying your partner?
 
    * If you both choose to cooperate with each other, you’ll each get a 1-year
    prison sentence.
 
    * If you choose to defect and your partner tries to cooperate, you’ll go free
    and your partner will get a 20-year prison sentence.
 
    * If you both try to betray each other, you’ll each receive a 5-year prison
    sentence.
 
    @button{Continue...}})

(defstep (waiter)
  @md{# Please Wait
 
    Please wait while another participant joins the queue.
 
    @refresh-every[5]})
 
(define matchmaker (make-matchmaker 2))
 
(defstep (pair-with-someone)
  (matchmaker waiter))

;; Save the same value to my-choice study var as well as to group instance var
(define (store-my-choice! val)
  (set! my-choice val)
  (store-my-result-in-group! 'choice val))

(defstep (make-choice)
  (define (cooperate) (store-my-choice! 'cooperate))
  (define (defect) (store-my-choice! 'defect))
 
  @md{# Make Your Choice
 
    @button[#:id "cooperate" cooperate]{Cooperate}
    @button[#:id "defect" defect]{Defect}})

(defstep (wait)
  (if (= (current-group-results-count 'choice) 0)
      @md{# Please Wait
 
        Please wait for the other participant to make their choice...
 
        @refresh-every[5]}
      (skip)))

(defstep (display-result)
  (define their-choice (first (current-group-member-results 'choice)))
  (set! prison-sentence
        (match* (my-choice their-choice)
          [('cooperate 'cooperate) 1]
          [('cooperate 'defect) 20]
          [('defect 'defect) 5]
          [('defect 'cooperate) 0]))
 
  @md{# Result

    The other person chose to @~a[their-choice], while you chose to @~a[my-choice].
 
    You get @~a[prison-sentence] years of prison.})

(defstudy prisoners-dilemma
  [intro --> pair-with-someone --> make-choice --> wait --> display-result]
  [display-result --> display-result])