#lang racket/base

(require (except-in forms form)
         "study.rkt")

(provide
 consent-study
 simple-study)

;; example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Step handlers are pure-ish.
;; Steps manage optional transitions, the default being `(next)'.

(define (info)
  `(div
    (h1 "Welcome to the study.")
    ,(button void "Continue")))

(define (give-consent)
  `(div
    (h1 "Do you consent?")
    ,(button
      (lambda ()
        (put 'consented? #t))
      "Yes")
    ,(button
      (lambda ()
        (put 'consented? #f))
      "No")))

(define name-form
  (form* ([name (ensure binding/text (required))])
    name))

(define (tell-name)
  `(div
    (h1 "Tell us your name!")
    ,(form
      name-form
      (lambda (name)
        (put 'name name))
      (lambda (rw)
        `(div
          ,(rw "name" (widget-text))
          ,@(rw "name" (widget-errors))
          (button ([type "submit"]) "Continue"))))))

(define consent-study
  (make-study
   #:requires '()
   #:provides '(consented?)
   (list
    (make-step 'info info)
    (make-step 'tell-name tell-name)
    (make-step 'give-consent-1
               give-consent
               (lambda ()
                 (if (get 'consented?)
                     next
                     done)))
    (make-step 'simple
               (lambda ()
                 (run-study simple-study)
                 (button void "Continue")))
    (make-step 'give-consent-2
               give-consent))))

(define simple-study
  (make-study
   (list
    (make-step 'info info))))
