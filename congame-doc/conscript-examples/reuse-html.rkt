#lang conscript

;; How to build pages that are similar.

;; Approach 1: define reusable html snippets. Important aspect: you create a
;; reusable html snipped with either html*{...} or md*{...} (use whichever you
;; find more convenient) - that is, don't use html{...} or md{...}, the versions
;; without *. Both html{...} and md{...} create a whole page, so it cannot be
;; plugged inside of another page.
(define common-instructions
  @md*{
    # Instructions

    In this study you will do several things:

    1. This
    2. and that
    3. and even this})

(defstep (instructions1)
  @md{
    @common-instructions

    @button{Continue}})

(defstep (instructions2)
  @md{
    @common-instructions

    You also should be doing this:

    4. More,
    5. and more,
    6. and even more!!

    @button{Continue}})

(define (end)
  @md{
    # Game Over})

(defstudy reuse-instructions
  [instructions1 --> instructions2
                 --> ,(lambda () done)])

;; Approach 2: define a function that returns a step with a variable part, and
;; then give different names in the transition graph of the study

;; Note the double parentheses around `hello-message m`!
(defstep ((hello-message m))
  @md{
      # Hello!

      We are so @m to have you here!

      @button{Continue}})

(defstudy reuse-welcome
  [[happy-hello (hello-message "happy")] --> [thrilled-hello (hello-message "thrilled")]
                                         --> [meh-hello (hello-message "meh")]
                                         --> ,(lambda () done)])

;; Both approach 1 and 2 assume that you know in advance what the variable part
;; is. So this does not work if, say, the variable part is something that the
;; participant submits themselves, such as their own name. For that, you have to
;; ask the participant, store it in the DB, and then display it as shown in the
;; tutorial.

(provide
 reuse-all)

(defstudy reuse-all
  [reuse-instructions --> reuse-welcome
                      --> end]
  [end --> end])
