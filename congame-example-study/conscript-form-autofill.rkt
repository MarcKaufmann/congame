#lang conscript

(provide
 conscript-form-autofill
 conscript-form-autofill-bot
 make-conscript-form-autofill-bot)

(defstep (intro)
  @md{# Form Autofill

      @button{Continue}})

(defvar name)
(defvar age)
(defvar choice)

(defstep (get-info)
  @md{# Form

      @form[#:bot ([example (#:name "Bogdan")
                            (#:age 33)
                            (#:choice "A")])]{
            @label{Name: @set![name (input-text)]}
            @label{Age: @set![age (input-number)]}
            @set![choice @select[`(("A" . "a")
                                   ("B" . "b"))]{Choice}]

            @|submit-button|}})

(defstep (display-info)
  @md{# Info

      Name: @name
      Age: @~a[age]
      Choice: @|choice|})

(define (get-info/autofill)
  (bot:autofill 'example))

(defstudy conscript-form-autofill
  [intro --> {get-info (with-bot get-info get-info/autofill)}
         --> {display-info (with-bot get-info bot:completer)}
         --> display-info])

(define (conscript-form-autofill-bot _id bot)
  (bot))

(define make-conscript-form-autofill-bot
  (bot:study->bot conscript-form-autofill))
