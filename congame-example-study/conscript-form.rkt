#lang conscript

(provide
 conscript-form-example)

(defstep (info)
  @html{@h1{Hello!}
        Welcome to the study.
        @button{Continue}})

(defstep (the-form)
  (define (on-submit #:name n #:text t #:checkboxes cs)
    (eprintf "name: ~s text: ~s checkboxes: ~s~n" n t cs))

  (define options
    '((a . "Option a")
      (b . "Option b")))
  (define (render-checkboxes options render-checkbox)
    `(div
      ()
      ,@(for/list ([o (in-list options)])
          (define v (car o))
          (define l (cdr o))
          (render-checkbox v l))))

  @html{@h1{The Form}
        @form[#:action on-submit]{
          @label{Name: @input-text[#:name] @~error[#:name]}
          @textarea[#:text]{Content:}
          @binding[#:checkboxes @make-checkboxes[options render-checkboxes]]
          @submit-button}})

(defstep (end)
  @html{@h1{The End}
        You're done.})

(defstudy conscript-form-example
  [info --> the-form --> end]
  [end --> end])
