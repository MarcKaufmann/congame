#lang conscript

;; This tests the support for autofill in form0 and spawning.

(require congame-web/components/study-bot
         conscript/form0
         racket/match)

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
  (define f
    (form*
      ([name
        (ensure
         binding/text
         (required))]
       [text
        (ensure
         binding/text
         (required))])
      (list name text)))

  (define (on-submit data)
    (match-define (list n t) data)
    (when (equal? t "spawn")
      (spawn-bot (make-conscript-bot conscript-bot-model)))
    (eprintf "name: ~s text: ~s~n" n t))

  (define (render rw)
    @html*{@make-autofill[(hasheq
                           'example
                           (hasheq
                            'name "Marc"
                            'text "Hello"))]
           @rw["name" @input-text{Name:}]
           @rw["text" @textarea{Content:}]
           @|submit-button|})

  @html{@h1{The Form}
        @form[f on-submit render]})

(defstep (end)
  @html{@h1{Done}
        You're done.})

(defstudy conscript-bot-example
  [{info (with-bot info info-bot)}
   --> {the-form (with-bot the-form autofill-the-form)}
   --> {end (with-bot end bot:completer)}]
  [end --> end])

(define (conscript-bot-model _id bot)
  (bot))

(define make-conscript-bot
  (bot:study->bot conscript-bot-example))
