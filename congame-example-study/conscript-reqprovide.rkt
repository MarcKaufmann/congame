#lang conscript

(provide
 conscript-reqprovide-example)

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
    (put 'actually-their-name? #t))
  (define (no)
    (put 'actually-their-name? #f))
  @html{@h1{Is your name @get['name]?}
        @button[yes]{Yes}
        @button[no]{No}})

(defstep (outer-done)
  (if (get 'actually-their-name?)
      @html{@p{Thanks @get['name]! You're done.}}
      @html{@p{You are a liar and a scoundrel.}}))

(defstudy inner-study
  #:requires '(name)
  #:provides '(actually-their-name?)
  [intro --> check-their-name --> ,(λ () done)])

(defstep/study inner-study-step
  #:study inner-study
  #:require-bindings ([name name])
  #:provide-bindings ([actually-their-name? actually-their-name?]))

(defstudy conscript-reqprovide-example
  [intro-outer
   --> get-their-name
   --> {inner-study inner-study-step}
   --> outer-done]
  [outer-done --> outer-done])
