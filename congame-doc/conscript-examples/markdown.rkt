#lang conscript

(provide
 tutorial)

(defstep (description)
  @md{
    # The study

    Welcome to our study. In this study, we will ask for

    - your first name
    - your age

    @button{Start Survey}})

(defstep (age-name-survey)
  @md{
    # Survey

    @form{
      @input-text[#:first-name]{What is your first name?}
      @input-number[#:age]{What is your age (in years)?}
      @submit-button}})

(defstep (thank-you)
  @md{
    # Thank you @get['first-name]

    Thank you for participating in our survey @get['first-name]!})

(defstudy tutorial
  [description --> age-name-survey
               --> thank-you]
  [thank-you --> thank-you])
