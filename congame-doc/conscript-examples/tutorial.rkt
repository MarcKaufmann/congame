#lang conscript

(provide
 tutorial)

(defstep (description)
  @html{
    @h1{The study}

    Welcome to our study. In this study, we will ask for

    @ul{
      @li{your first name}
      @li{your age}}

    @button{Start Survey}})

(defstep (age-name-survey)
  @html{
    @h1{Survey}

    @form{
      @input-text[#:first-name]{What is your first name?}
      @input-number[#:age]{What is your age (in years)?}
      @submit-button}})

(defstep (thank-you)
  @html{
    @h1{Thank you @get['first-name]}

    Thank you for participating in our survey @get['first-name]! You are the best @(number->string (get 'age))-year old person.})

(defstudy tutorial
  [description --> age-name-survey --> thank-you]
  [thank-you --> thank-you])
