#lang conscript

(require conscript/form)

(provide benchmark-study)

(defvar display-name)

(defstep (intro)
  @html{
    @h1{Introduction}
    Welcome to this study. Please click Continue to proceed.
    @button{Continue}})

(defstep (collect-name)
  (define (on-submit #:display-name name)
    (set! display-name name))
  @html{
    @h1{Your Name}
    @form[#:action on-submit]{
      @label{Name: @input-text[#:display-name] @~error[#:display-name]}
      @submit-button}})

(defstep (final)
  @html{
    @h1{Thank You!}
    You have submitted your name: @|display-name|.
    @button{Finish}})

(defstudy benchmark-study
  [intro --> collect-name --> final]
  [final --> final])
