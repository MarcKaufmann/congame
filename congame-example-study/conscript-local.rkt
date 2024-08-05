#lang conscript/local

(defvar* shared-var
  shared-var-unique-id)

(defvar name)
(defvar age)

(defstep (hello)
  @md{# Welcome to the study

      You are participant @~a[(current-participant-id)].

      @button{Continue}})

(defstep (set-shared-var)
  (set! shared-var 42)
  @md{# Press the Button to Continue

      @button{Continue}})

(defstep (get-info)
  @md{# Tell us about yourself

      The value of the shared var is @format["~a" shared-var].

      @form{@input-text[#:name]
            @input-number[#:age]
            @submit-button}})

(defstep (end)
  @md{# You're done

      Name: @name

      Age: @format["~a" @age]

      @button{Continue}})

(defstudy study-a
  [hello --> set-shared-var --> get-info --> end]
  [end --> ,(lambda () done)])

(defstep (outer-hello)
  @md{# Welcome to the Parent Study

      @button{Continue}})

(defstep (outer-end)
  @md{# You're done!

      Shared var: @format["~a" shared-var].})

(defstudy study-b
  [outer-hello --> study-a --> outer-end]
  [outer-end --> outer-end])
