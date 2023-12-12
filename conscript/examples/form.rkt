#lang conscript

(provide
 conscript-form-example)

(defstep (info)
  @html{@h1{Hello!}
        Welcome to the study.
        @button{Continue}})

(defstep (the-form)
  (define (on-submit #:name n #:text t)
    (eprintf "name: ~s text: ~s~n" n t))

  @html{@h1{The Form}
        @form[#:action on-submit]{
          @label{Name: @input-text[#:name] @~error[#:name]}
          @textarea[#:text]{Content:}
          @submit-button}})

(defstep (done)
  @html{@h1{Done}
        You're done.})

(defstudy conscript-form-example
  [info --> the-form --> done]
  [done --> done])
