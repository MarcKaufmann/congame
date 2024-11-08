#lang conscript

(require racket/format) ; -> provides ~r

; Notes for fixing:
;
; - Provide warning using @button inside of forms.
; - Add initialization to defvar
; - input-*: require a single string at the end - is that necessary?
;
; For teaching:
;
; - Search Racket docs
;
; In this study:
; 0. welcome page
; 1. person provides their name and age
; 2. we display it on the next page
; 3. we ask them how many tasks to do for a random piece-rate wage
;   - We pick this wage once, we do not keep drawing a new one!
; 4. we display how much money they would get for working this much.

(defstep (instructions)
  @md{# Instructions

      ...})

(defstep (welcome)
  @md{# Welcome

      @p[#:style "color: red;"]{Styled red @(~a (+ 2 2))}.

      @button{Continue}})

(defvar name)
(defvar age)

(defstep (questionnaire)
  @md{# Questionnaire

      @form{
        @div{
          Your name: @input-text[#:name] @~error[#:name]}
        @div{
          Your age: @input-number[#:age] @~error[#:age]}
        @submit-button}})

(defstep (display-results)
  @md{# Display Results

      - Your name: @name
      - Your age: @(~a age) @(format "~a" age) @(format "Your age, Mr/Ms/Mrs ~a is ~a, yes it really is ~a" name age age)

      @button{Next}
      })

(defvar tasks)
(defvar piece-rate)

; 3. we ask them how many tasks to do for a random piece-rate wage
;   - We pick this wage once, we do not keep drawing a new one!
;   - Put the text above or to the left of the input field.

(defstep (choose-tasks)
  (when (undefined? piece-rate)
    (set! piece-rate (* 0.01 (random 0 100))))

  @md{# Choose Tasks

      @form{
        @div{
          How many tasks are you willing to do for @(~r piece-rate #:precision 2)?
          @div{
            @input-number[#:tasks] @~error[#:tasks]}}

        @submit-button}})

(define (reset-piece-rate)
  (set! piece-rate undefined))

(defvar task-payment)

(defstep (display-payment)
  (set! task-payment (* piece-rate tasks))

  @md{# Your Payment

      Your payment for performing @(~a tasks) is $@(~r task-payment #:precision 2).

      @button[reset-piece-rate]{Back to Start}})

(defvar consent?)

(defstep (consent)
  (define (on-submit #:consent? c)
    (set! consent? (string=? c "yes")))

  @md{# Consent Form
      @form[#:action on-submit]{
        @radios[
          #:consent?
          '(("yes" . "Yes")
            ("no" . "No"))
          ]{Do you consent to participate in this study?}
        @submit-button}})

(defstep (the-end)
  @md{# The End})

(defstudy temp-study
  [welcome --> consent
           --> ,(lambda ()
                  (if consent? 'questionnaire 'the-end))]

  [the-end --> the-end] ; Necessary so study knows about 'the-end

  [questionnaire --> display-results
                 --> choose-tasks
                 --> display-payment
                 --> welcome])



(defstudy simple-study
  ; Declare Transitions
  [welcome --> questionnaire
           --> display-results
           --> choose-tasks
           --> display-payment]

  [display-payment --> display-payment])


;; Markdown
;# This is a level 1 title
;
;## Level 2 title
;
;@h2[]Level 2 title Nr 2
;
;A paragraph
;
;- a list item
;- another item
;
;; HTML
;<h1 style="color: red;">Title</h1>
;<p id="my-id">First para</p>
;
;; Keyword argument
;(bla x #:something "red")
;bla(x, something="red")
;
;; Conscript HTML
;@html{
;  @h1[#:style "color: red;"]{Title}
;  @p[#:id "my-id"]
;
;  }
