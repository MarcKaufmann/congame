#lang conscript

(provide
 example)

(defvar name)
(defvar consented?) ;; should require that values are immutable to begin with

(defstep (info)
  @html{@h1{Welcome to the study}
        This is an introductory paragraph.

        This is a different paragraph because there are two newlines
        between it and the previous paragraph.

        @button{Continue}})

(defstep (tell-name)
  (define (on-submit #:name the-name)
    (set! name the-name))
  @html{@h1{What is your name?}
        @form[on-submit]{
          @label{Name: @text-field[name]}
          @submit-button[]
        }})

(defstep (give-consent name)
  (define (accepted)
    (set! consented? #t))
  (define (rejected)
    (set! consented? #f))
  @html{@h1{Hi @|name|!}

        Do you consent?

        @button[accepted]{Yes}
        @button[rejected]{No}})

(defstep (consented)
  @html{You consented. @button{Continue}})

(defstep (didnt-consent)
  @html{You didn't consent. @button{Continue}})

(defstep (done)
  @html{Thanks! You're done.})

(defstudy (consent name)
  [give-consent --> ,(λ () (if consented? 'consented 'didnt-consent))]
  [consented --> ,done]
  [didnt-consent --> ,done])

(defstudy (example)
  [info --> tell-name --> ,(consent name) --> done])
