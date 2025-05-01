#lang conscript/with-require

(require conscript/form0
         "conscript-defvar-consent.rkt")

(provide
 conscript-defvar-example)

(defstep (info)
  (define-values (f on-submit)
    (form+submit
     [name
      (ensure
       binding/text
       (required))]))

  (define (render rw)
    @html*{@rw["name" @input-text{Name:}]
           @|submit-button|})

  @html{@h1{Hello!}
        Welcome to the study.
        @form[f on-submit render]})

(defstep (done)
  @html{@h1{Done}
        You're done.})

(defstudy conscript-defvar-example
  [info --> consent --> done]
  [done --> done])
