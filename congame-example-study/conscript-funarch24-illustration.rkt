#lang conscript

(require racket/list
         racket/random)

(provide illustration)

(defvar name)
(defvar fav-pl)
(defvar use-continuations?)
(defvar know-reg-vs-del?)

(defvar*/instance use-continuations?/all use-continuations-id-abc)
(defvar*/instance know-reg-vs-del?/all know-reg-vs-del-id-abc)

(define (yes/no)
  (radios '(("yes" . "Yes")
            ("no"  . "No"))
          ""))

(defstep (survey)
  @md{# Survey

      @form{
        @div{
          @label{Name: @set![name (input-text)]}}
        @div{
          @label{Favorite programming language: @set![fav-pl (input-text)]}}
        @div{
          @label{Have you ever used continuations in an application outside of a computer science class?}
          @div{
            @set![use-continuations? (yes/no)]}}
        @div{
          @label{@md*{Do you know the difference between *regular* and *delimited* continuations?}}
          @div{
            @set![know-reg-vs-del? (yes/no)]}}
        @submit-button}})

(defvar pass-attention-check?)

(define (cons/instance a l)
  (with-study-transaction
    (if (undefined? l)
        (set! l (cons a null))
        (set! l (cons a l)))))

(define (yn->tf x)
  (case x
    [("yes") #t]
    [("no")  #f]))

(defstep (evaluate-survey)
  (set! pass-attention-check?
    (string=? (string-downcase fav-pl) "racket"))
  (define a (yn->tf use-continuations?))
  (with-study-transaction
    (if (undefined? use-continuations?/all)
        (set! use-continuations?/all (cons a null))
        (set! use-continuations?/all (cons a use-continuations?/all))))
  (define b (yn->tf know-reg-vs-del?))
  (with-study-transaction
    (if (undefined? know-reg-vs-del?/all)
        (set! know-reg-vs-del?/all (cons b null))
        (set! know-reg-vs-del?/all (cons b know-reg-vs-del?/all))))
  (skip))

(defstep (survey-feedback)

  @md{# Thank you @|name|

      @(if pass-attention-check?
           "My favorite programming language is Racket too!"
           (format "Your favorite programming language is ~a" @fav-pl))

      @button{Next}})

(defvar guess)
(defvar toss)

(defstep (heads-or-tails)
  (set! toss (random-ref '("h" "t")))

  @md{# Coin Toss

      @form{
        @label{We have tossed a digital coin. Guess whether it showed Heads or Tails:}

        @set![guess
              (radios '(("h" . "Heads")
                        ("t" . "Tails"))
                      "")]
        @submit-button}})

(defstep (result)
  (define correct-guess?
    (string=? toss guess))
  (define (ht ct)
    (case ct
      [("h") "Heads"]
      [("t") "Tails"]))

  @md{# Result

      The toss came up with @(ht toss) and you guessed @(ht guess).

      So you guessed @(if correct-guess? "right" "wrong").

      @button{Next}})

(defstudy coin-toss
  [heads-or-tails --> result
                  --> ,(lambda () done)])

(defvar completed?)
(defvar*/instance completed/all completed/all-id)

(defstep (the-end)
  ; TODO: Would it have been ok to `(set! completed?)` outside of study-transaction?
  (when (undefined? completed?)
    (with-study-transaction
      (if (undefined? completed/all)
          (set! completed/all 1)
          (set! completed/all (add1 completed/all)))
      (set! completed? #t)))
  @md{# The End

      Thank you for participating.})

(define (check-owner)
  (skip))

(define (admin)
  (define (count/total xs)
    (if (undefined? xs)
        "(No participants yet.)"
        (format "~a/~a"
                (apply + (map (lambda (x) (if x 1 0)) xs))
                (length xs))))

  @md{# Admin

      - Have used continuations: @(count/total use-continuations?/all)
      - Know difference between delimited and regular continuations: @(count/total know-reg-vs-del?/all)
      - Completed study: @(~a (if-undefined completed/all 0))})

(defstudy illustration
  [check-owner --> ,(lambda ()
                      (if (current-participant-owner?) 'admin 'survey))]
  [admin --> admin]
  [survey --> evaluate-survey
          --> survey-feedback
          --> coin-toss
          --> the-end]
  [the-end --> the-end])
