#lang conscript

(provide
 conscript-data-sharing-example)

(with-namespace xyz.trichotomy.congame-example-study.conscript-data-sharing
  (defvar* name)
  (defvar* actually-their-name?))

(defstep (intro-outer)
  @html{@h1{Hello}
        Welcome to the outer study.
        @button{Continue}})

(defstep (intro)
  @html{@h1{Hello}
        Welcome to the study.
        @button{Continue}})

(defstep (get-their-name)
  @html{@h1{What's your name?}
        @form{@label{Name: @input-text[#:name]}
              @submit-button}})

(defstep (check-their-name)
  (define (yes)
    (set! actually-their-name? #t))
  (define (no)
    (set! actually-their-name? #f))
  @html{@h1{Is your name @|name|?}
        @button[yes]{Yes}
        @button[no]{No}})

(defstep (outer-done)
  (if actually-their-name?
      @html{@p{Thanks @|name|! You're done.}}
      @html{@p{You are a liar and a scoundrel.}}))

(defstudy inner-study
  [intro --> check-their-name --> ,(λ () done)])

(defstudy conscript-data-sharing-example
  [intro-outer
   --> get-their-name
   --> inner-study
   --> outer-done]
  [outer-done --> outer-done])
