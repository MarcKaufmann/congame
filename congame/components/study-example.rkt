#lang racket/base

(require "study.rkt")

(provide
 consent-study)

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

(define consent-study
  (make-study
   #:requires '()
   #:provides '(consented?)
   (list
    (make-step 'info info)
    (make-step 'give-consent-1
               give-consent
               (lambda ()
                 (if (get 'consented?)
                     next
                     done)))
    (make-step 'give-consent-2
               give-consent))))
