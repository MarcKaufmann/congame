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
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:p "a" " " "b")
               (:p "c"))))))))

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
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:p "a" " " "b")
               (:p "c")
               (:h1 "Heading"))))))))

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
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:p "a" " " "b")
               (:p "c")
               (:h1 "Heading"))))))))

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
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:p "a" " " "b")
               (:p "c" "\n" "\n" "d")
               (:h1 "Heading"))))))))

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
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:p "a" " " "b")
               (:p "c")
               (:h1 "Heading")
               (:p "d" " " "e")
               (:p "f"))))))))

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
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:h1 "Hello, world!")
               (:p "How's it going?" " " "Pretty good?")
               (:p "Yeah, good."))))))))

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
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:h1 "Hello!")
               (button void "Continue..."))))))
        (define (done)
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:p "You're done!"))))))
        (define hello-study
          (make-study
           "hello-study"
           #:transitions (transition-graph
                          [hello --> done]
                          [done --> done])
           (list
            (->step 'hello hello)
            (->step 'done done))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@import[a b]
DSL
                     ))
      '((environment-set! *env* 'b (study-mod-require 'a 'b))))

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
          (let ([*env* (make-environment *env*)])
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
                (make-put-all-keywords void)))))))))

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
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:ol
                (:li "Test")
                (:li "Foo")))))))))

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
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:ul
                (:li "Test")
                (:li "Foo")))))))))

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
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:ul
                (:li
                 (:ul
                  (:li "Nested 1")
                  (:li "Nested 2")))
                (:li "Foo")))))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[example-step]{
  @a["http://example.com"]{example.com}
}
DSL
                     ))
      '((define (example-step)
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:p (:a ([:href "http://example.com"]) "example.com")))))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[example-step]{
  @strong{Hello @em{world}!}
}
DSL
                     ))
      '((define (example-step)
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:p
                (:strong "Hello " (:em "world") "!")))))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[example-step]{
  Hello @strong{world}!
}
DSL
                     ))
      '((define (example-step)
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:p
                "Hello "
                (:strong "world")
                "!"))))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@step[example-step]{
  @div{Hello @em{world}!}
}
DSL
                     ))
      '((define (example-step)
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:div ([:data-ignored ""])(:p "Hello " (:em "world") "!")))))))))

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
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:div ((:data-ignored ""))
                     (:p "Hello")
                     (:p (:em "world") "!")))))))))

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
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:div
                ([:data-ignored ""] [:class "example"])
                (:p "Hello")
                (:p (:em "world") "!")))))))))

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
  [step0 --> @(ev (lambda ()
                   (cond
                    [(= (get 'some-var) "agree") (goto step1)]
                    [(= (get/instance 'some-other-var) "always-agree") (goto step1)]
                    [else (goto step2)])))]
  [step1 --> done]
  [step2 --> done]
  [done --> done]
]
DSL
                     ))
      '((define (step0)
          (let ([*env* (make-environment *env*)])
            (page (haml (.container (button void "Continue"))))))
        (define (step1)
          (let ([*env* (make-environment *env*)])
            (page (haml (.container (button void "Continue"))))))
        (define (step2)
          (let ([*env* (make-environment *env*)])
            (page (haml (.container (button void "Continue"))))))
        (define (done)
          (let ([*env* (make-environment *env*)])
            (page (haml (.container (:p "You're done."))))))
        (define hello-study
          (make-study
           "hello-study"
           #:transitions (transition-graph
                          [step0 --> ,(interpret
                                       '(lambda ()
                                          (cond
                                            [(= (get 'some-var) "agree")
                                             (goto step1)]
                                            [(= (get/instance 'some-other-var) "always-agree")
                                             (goto step1)]
                                            [else
                                             (goto step2)]))
                                       *env*)]
                          [step1 --> done]
                          [step2 --> done]
                          [done --> done])
           (list
            (->step 'step0 step0)
            (->step 'step1 step1)
            (->step 'done done)
            (->step 'step2 step2))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@import[racket/base println]

@action[quit]{
  @(ev (println "hello"))
}

@step[hello]{
  @button[#:action quit]{Quit}
}
DSL
                     ))
      '((environment-set! *env* 'println (study-mod-require 'racket/base 'println))
        (define (quit)
          (let ([*env* (make-environment *env*)])
            (interpret '(println "hello") *env*)))
        (define (hello)
          (let ([*env* (make-environment *env*)])
            (page (haml (.container (button quit "Quit"))))))))

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
  [step0 --> @(ev (lambda ()
                    (println "hello")
                    (cond
                     [(= (get 'some-var) "agree") (goto step1)]
                     [else (goto step2)])))]
  [step1 --> done]
  [step2 --> done]
  [done --> done]
]
DSL
                     ))
      '((environment-set! *env* 'println (study-mod-require 'racket/base 'println))
        (define (step0)
          (let ([*env* (make-environment *env*)])
            (page (haml (.container (button void "Continue"))))))
        (define (step1)
          (let ([*env* (make-environment *env*)])
            (page (haml (.container (button void "Continue"))))))
        (define (step2)
          (let ([*env* (make-environment *env*)])
            (page (haml (.container (button void "Continue"))))))
        (define (done)
          (let ([*env* (make-environment *env*)])
            (page (haml (.container (:p "You're done."))))))
        (define hello-study
          (make-study
           "hello-study"
           #:transitions (transition-graph
                          [step0 --> ,(interpret
                                       '(lambda ()
                                          (println "hello")
                                          (cond
                                            [(= (get 'some-var) "agree")
                                             (goto step1)]
                                            [else
                                             (goto step2)]))
                                       *env*)]
                          [step1 --> done]
                          [step2 --> done]
                          [done --> done])
           (list
            (->step 'step0 step0)
            (->step 'step1 step1)
            (->step 'done done)
            (->step 'step2 step2))))))

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
          (let ([*env* (make-environment *env*)])
            (haml
             (:div
              (:p "Hello " ,@(content-proc) "!")))))
        (define (b content-proc)
          (let ([*env* (make-environment *env*)])
            (haml
             (:div
              (a (λ () (list (haml "Hi world!"))))
              (a (λ () (haml "Hello" "\n" "\n" "there" "\n" "\n" (:h1 "Friend"))))))))
        (define (hello)
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (b (λ () (error 'template "yielded without content"))))))))))

     (check-equal?
      (syntax->datum
       (read+compile #<<DSL
@import[racket/base format]
@action[test]{
  @(ev (put 'counter1 3))
  @(ev (put 'counter2 (format "~a" "Hello")))
  @(ev (put 'sym 'test))
}
DSL
                     ))
      '((environment-set! *env* 'format (study-mod-require 'racket/base 'format))
        (define (test)
          (let ([*env* (make-environment *env*)])
            (interpret '(put 'counter1 3) *env*)
            (interpret '(put 'counter2 (format "~a" "Hello")) *env*)
            (interpret '(put 'sym 'test) *env*)))))

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
  @(ev (put 'x 42))
}

@step[foo #:pre pre-step-foo]{
  Hello world
}
DSL
        ))
      '((define (pre-step-foo)
          (let ([*env* (make-environment *env*)])
            (interpret '(put 'x 42) *env*)))
        (define (foo)
          (let ([*env* (make-environment *env*)])
            (pre-step-foo)
            (page
             (haml
              (.container
               (:p "Hello world"))))))))

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
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:p "Hello world"))))))
        (define (bar)
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:p "Goodbye"))))))))


     (check-equal?
      (syntax->datum
       (read+compile
        #<<DSL
@step[a]{
  Hello world
}

@study[
 binding-transitions
 #:transitions
 [a --> [b a] --> b]
]
DSL
        ))
      '((define (a)
          (let ([*env* (make-environment *env*)])
            (page
             (haml
              (.container
               (:p "Hello world"))))))
        (define binding-transitions
          (make-study
           "binding-transitions"
           #:transitions
           (transition-graph (a --> b --> b))
           (list
            (->step 'a a)
            (->step 'b a)))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests dsl-tests))
