#lang racket/base

(require racket/port
         rackunit

         congame/components/dsl)

(provide
 dsl-tests)

(define (read+compile str)
  (call-with-input-string str
    (lambda (in)
      (port-count-lines! in)
      (read-syntax+compile "<string>" in))))

(define dsl-tests
  (test-suite
   "dsl"

   (test-case "read-syntax+compile"
     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[a]{
  a
  b

  c
}
DSL
                     ))
      '((define (a)
          (page
           (haml
            (.container
             (:p "a" " " "b")
             (:p "c")))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[a]{
  a
  b

  c

  @h1{Heading}
}
DSL
                     ))
      '((define (a)
          (page
           (haml
            (.container
             (:p "a" " " "b")
             (:p "c")
             (:h1 "Heading")))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[a]{
  a
  b

  @p{c}

  @h1{Heading}
}
DSL
                     ))
      '((define (a)
          (page
           (haml
            (.container
             (:p "a" " " "b")
             (:p "c")
             (:h1 "Heading")))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[a]{
  a
  b

  @p{
    c

    d
  }

  @h1{Heading}
}
DSL
                     ))
      '((define (a)
          (page
           (haml
            (.container
             (:p "a" " " "b")
             (:p "c" "\n" "\n" "d")
             (:h1 "Heading")))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[a]{
  a
  b

  c

  @h1{Heading}

  d
  e

  f
}
DSL
                     ))
      '((define (a)
          (page
           (haml
            (.container
             (:p "a" " " "b")
             (:p "c")
             (:h1 "Heading")
             (:p "d" " " "e")
             (:p "f")))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[a]{
  @h1{Hello, world!}
  How's it going?
  Pretty good?

  Yeah, good.
}
DSL
                     ))
      '((define (a)
          (page
           (haml
            (.container
             (:h1 "Hello, world!")
             (:p "How's it going?" " " "Pretty good?")
             (:p "Yeah, good.")))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[hello]{
  @h1{Hello!}
  @button{Continue...}
}

@step[done]{
  You're done!
}

@study[
  hello-study
  #:transitions
  [hello --> done]
  [done --> done]
]
DSL
                     ))
      '((define (hello)
          (page
           (haml
            (.container
             (:h1 "Hello!")
             (button void "Continue...")))))
        (define (done)
          (page
           (haml
            (.container
             (:p "You're done!")))))
        (define hello-study
          (make-study
           "hello-study"
           #:transitions (transition-graph
                          [hello --> done]
                          [done --> done])
           (list
            (cond
              ((study? hello) (make-step/study 'hello hello))
              ((or (step/study? hello) (step? hello)) hello)
              (else (make-step 'hello hello)))
            (cond
              ((study? done) (make-step/study 'done done))
              ((or (step/study? done) (step? done)) done)
              (else (make-step 'done done))))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@import[a b]
DSL
                     ))
      '((define b (study-mod-require 'a 'b))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[form-step]{
  @form{
    @h1{Section 1}
    @input-text[name]{What is your name?}
    @h1{Section 2}
    @input-number[age #:max 100 #:min 1]{How old are you?}
    @submit-button[]
  }
}
DSL
                     ))
      '((define (form-step)
          (page
           (haml
            (.container
             (formular
              (haml
               (:div
                (:h1 "Section 1")
                (#:name
                 (input-text
                  (haml "What is your name?")))
                (:h1 "Section 2")
                (#:age
                 (input-number
                  #:min 1
                  #:max 100
                  (haml "How old are you?")))
                (:button.button.next-button
                 ([:type "submit"])
                 "Submit")))
              (make-put-all-keywords void))))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[example-step]{
  @ol{
    @li{Test}
    @li{Foo}
  }
}
DSL
                     ))
      '((define (example-step)
          (page
           (haml
            (.container
             (:ol
              (:li "Test")
              (:li "Foo"))))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[example-step]{
  @ul{
    @li{Test}
    @li{Foo}
  }
}
DSL
                     ))
      '((define (example-step)
          (page
           (haml
            (.container
             (:ul
              (:li "Test")
              (:li "Foo"))))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[example-step]{
  @ul{
    @li{
      @ul{
        @li{Nested 1}
        @li{Nested 2}
      }
    }
    @li{Foo}
  }
}
DSL
                     ))
      '((define (example-step)
          (page
           (haml
            (.container
             (:ul
              (:li
               (:ul
                (:li "Nested 1")
                (:li "Nested 2")))
              (:li "Foo"))))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[example-step]{
  @a["http://example.com"]{example.com}
}
DSL
                     ))
      '((define (example-step)
          (page
           (haml
            (.container
             (:p (:a ([:href "http://example.com"]) "example.com"))))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[example-step]{
  @strong{Hello @em{world}!}
}
DSL
                     ))
      '((define (example-step)
          (page
           (haml
            (.container
             (:p
              (:strong "Hello " (:em "world") "!"))))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[example-step]{
  Hello @strong{world}!
}
DSL
                     ))
      '((define (example-step)
          (page
           (haml
            (.container
             (:p
              "Hello "
              (:strong "world")
              "!")))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[example-step]{
  @div{Hello @em{world}!}
}
DSL
                     ))
      '((define (example-step)
          (page
           (haml
            (.container
             (:div (:p "Hello " (:em "world") "!"))))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[example-step]{
  @div{
   Hello

   @em{world}!
  }
}
DSL
                     ))
      '((define (example-step)
          (page
           (haml
            (.container
             (:div
              (:p "Hello")
              (:p (:em "world") "!"))))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[example-step]{
  @div[#:class "example"]{
   Hello

   @em{world}!
  }
}
DSL
                     ))
      '((define (example-step)
          (page
           (haml
            (.container
             (:div
              ([:class "example"])
              (:p "Hello")
              (:p (:em "world") "!"))))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[step0]{@button{Continue}}
@step[step1]{@button{Continue}}
@step[step2]{@button{Continue}}
@step[done]{You're done.}

@study[
  hello-study
  #:transitions
  [step0 --> @cond[[@=[@get['some-var] "agree"] step1]
                   [@=[@get[#:instance 'some-other-var] "always-agree"] step1]
                   [@else step2]]]
  [step1 --> done]
  [step2 --> done]
  [done --> done]
]
DSL
                     ))
      '((define (step0) (page (haml (.container (button void "Continue")))))
        (define (step1) (page (haml (.container (button void "Continue")))))
        (define (step2) (page (haml (.container (button void "Continue")))))
        (define (done) (page (haml (.container (:p "You're done.")))))
        (define hello-study
          (make-study
           "hello-study"
           #:transitions (transition-graph
                          [step0 --> ,(λ ()
                                        (cond
                                          [(equal? (get 'some-var) "agree")
                                           (goto step1)]
                                          [(equal? (get/instance 'some-other-var) "always-agree")
                                           (goto step1)]
                                          [else
                                           (goto step2)]))]
                          [step1 --> done]
                          [step2 --> done]
                          [done --> done])
           (list
            (cond
              ((study? step0) (make-step/study 'step0 step0))
              ((or (step/study? step0) (step? step0)) step0)
              (else (make-step 'step0 step0)))
            (cond
              ((study? step1) (make-step/study 'step1 step1))
              ((or (step/study? step1) (step? step1)) step1)
              (else (make-step 'step1 step1)))
            (cond
              ((study? done) (make-step/study 'done done))
              ((or (step/study? done) (step? done)) done)
              (else (make-step 'done done)))
            (cond
              ((study? step2) (make-step/study 'step2 step2))
              ((or (step/study? step2) (step? step2)) step2)
              (else (make-step 'step2 step2))))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@import[racket/base println]

@action[quit]{
  @call[println "hello"]
}

@step[hello]{
  @button[#:action quit]{Quit}
}
DSL
                     ))
      '((define println (study-mod-require 'racket/base 'println))
        (define (quit) (println "hello"))
        (define (hello) (page (haml (.container (button quit "Quit")))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@import[racket/base println]

@step[step0]{@button{Continue}}
@step[step1]{@button{Continue}}
@step[step2]{@button{Continue}}
@step[done]{You're done.}

@study[
  hello-study
  #:transitions
  [step0 --> @lambda[
               @call[println "hello"]
               @cond[[@=[@get['some-var] "agree"] step1]
                     [@else step2]]]]
  [step1 --> done]
  [step2 --> done]
  [done --> done]
]
DSL
                     ))
      '((define println (study-mod-require 'racket/base 'println))
        (define (step0) (page (haml (.container (button void "Continue")))))
        (define (step1) (page (haml (.container (button void "Continue")))))
        (define (step2) (page (haml (.container (button void "Continue")))))
        (define (done) (page (haml (.container (:p "You're done.")))))
        (define hello-study
          (make-study
           "hello-study"
           #:transitions (transition-graph
                          [step0 --> ,(λ ()
                                        (println "hello")
                                        (cond
                                          [(equal? (get 'some-var) "agree")
                                           (goto step1)]
                                          [else
                                           (goto step2)]))]
                          [step1 --> done]
                          [step2 --> done]
                          [done --> done])
           (list
            (cond
              ((study? step0) (make-step/study 'step0 step0))
              ((or (step/study? step0) (step? step0)) step0)
              (else (make-step 'step0 step0)))
            (cond
              ((study? step1) (make-step/study 'step1 step1))
              ((or (step/study? step1) (step? step1)) step1)
              (else (make-step 'step1 step1)))
            (cond
              ((study? done) (make-step/study 'done done))
              ((or (step/study? done) (step? done)) done)
              (else (make-step 'done done)))
            (cond
              ((study? step2) (make-step/study 'step2 step2))
              ((or (step/study? step2) (step? step2)) step2)
              (else (make-step 'step2 step2))))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@template-ungrouped[a]{
  Hello @yield[]!
}

@template[b]{
  @template[a]{
    Hi world!
  }

  @template[a]{
    Hello

    there

    @h1{Friend}
  }
}

@step[hello]{
  @template[b]
}
DSL
                     ))
      '((define (a content-proc)
          (haml
           (:div
            (:p "Hello " ,@(content-proc) "!"))))
        (define (b content-proc)
          (haml
           (:div
            (a (λ () (list (haml "Hi world!"))))
            (a (λ () (haml "Hello" "\n" "\n" "there" "\n" "\n" (:h1 "Friend")))))))
        (define (hello)
          (page
           (haml
            (.container
             (b (λ () (error 'template "yielded without content")))))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@import[racket/base format]
@action[test]{
  @put['counter1 3]
  @put['counter2 @call[format "~a" "Hello"]]
  @put['sym 'test]
}
DSL
                     ))
      '((define format (study-mod-require 'racket/base 'format))
        (define (test)
          (put 'counter1 3)
          (put 'counter2 (format "~a" "Hello"))
          (put 'sym 'test))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@import[stdlib add1 make-step/study]
DSL
                     ))
      '((begin
          (define add1 (dynamic-require 'congame/components/dsl/stdlib 'add1))
          (define make-step/study (dynamic-require 'congame/components/dsl/stdlib 'make-step/study)))))

     (check-exn
      #rx"not a valid step id"
      (lambda ()
        (read+compile #<<DSL
@step[end]{Hello}
DSL
                      )))

     (check-exn
      #rx"not a valid study id"
      (lambda ()
        (read+compile #<<DSL
@study[
  end
  #:transitions
  [a --> b]]
DSL
                      )))

     (check-equal?
      (syntax->datum
       (read+compile
        #<<DSL
@action[pre-step-foo]{
  @put['x]{42}
}

@step[foo #:pre pre-step-foo]{
  Hello world
}
DSL
        ))
      '((define (pre-step-foo)
          (put 'x "42"))
        (define (foo)
          (pre-step-foo)
          (page
           (haml
            (.container
             (:p "Hello world")))))))

     (check-equal?
      (syntax->datum
       (read+compile
        #<<DSL
  @step[foo]{
  Hello world
}

   @step[bar]{
  Goodbye
}
DSL
        ))
      '((define (foo)
          (page
           (haml
            (.container
             (:p "Hello world")))))
        (define (bar)
          (page
           (haml
            (.container
             (:p "Goodbye"))))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests dsl-tests))
