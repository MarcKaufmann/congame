#lang conscript

(provide
 conscript-bot-example
 conscript-bot-model
 make-conscript-bot)

(define (info-bot)
  (bot:continuer))

(defstep (info)
  @html{@h1{Hello!}
        Welcome to the study.
        @button{continue}})

(define (autofill-the-form)
  (bot:autofill 'example))

(defstep (the-form)
  (define (on-submit #:name n #:text t)
    (eprintf "name: ~s text: ~s~n" n t))

  @html{@h1{The Form}
        @form[#:action on-submit
              #:bot ([example (#:name "Marc")
                              (#:text "Hello")])]{
          @label{Name: @input-text[#:name] @~error[#:name]}
          @textarea[#:text]{Content:}
          @submit-button}})

(defstep (end)
  @html{@h1{Done}
        You're done.})

(defstudy conscript-bot-example
  [{info (with-bot info info-bot)}
   --> {the-form (with-bot the-form autofill-the-form)}
   --> end]
  [end --> end])

(define (conscript-bot-model _id bot)
  (bot))

(define make-conscript-bot
  (bot:study->bot conscript-bot-example))
