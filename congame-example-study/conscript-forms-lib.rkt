#lang conscript

(require conscript/form0
         racket/match)

(provide
 conscript-forms-lib)

(defstep (intro)
  @md{# Forms

      This study shows how to use `forms-lib` with conscript.

      @button{Continue}})

(defvar name)
(defvar age)
(defvar checkbox-choices)
(defvar radio-choice)
(defvar select-choice)

(defstep (survey)
  (define choices
    '(("a" . "Choice A")
      ("b" . "Choice B")))

  (define choice->choice-sym
    (for/list ([choice (in-list choices)])
      (match-define (cons value _) choice)
      (cons value (string->symbol value))))

  (define choice-formlet
    (ensure
     binding/text
     (one-of choice->choice-sym)))

  (define-values (survey-form on-submit)
    (form+submit
     [name (ensure binding/text (required))]
     [age (ensure binding/number (required))]
     [checkbox-choices
      (ensure
       binding/list
       (list-of choice-formlet))]
     [radio-choice
      (ensure choice-formlet (required))]
     [select-choice
      (ensure choice-formlet (required))]))

  (define (render rw)
    @md*{@rw["name" (input-text)]
         @rw["age" (input-number)]
         @rw["checkbox-choices" (checkboxes choices)]
         @rw["radio-choice" (radios choices)]
         @rw["select-choice" (select (cons '("" . "Please select something") choices) "")]
         @|submit-button|})

  @md{# Survey

      @form[survey-form on-submit render]})

(defstep (display-info)
  @md{# Thanks

      Name: @name
      Age: @~a[age]
      Checkbox choices: @~a[checkbox-choices]
      Radio choice: @~a[radio-choice]})

(defstudy conscript-forms-lib
  [intro --> survey --> display-info --> ,(λ () done)])
