#lang racket/base

(require koyo/mail
         koyo/url
         racket/contract)

(provide
 mailer-send-next-phase-started-email)

(define/contract (mailer-send-next-phase-started-email m recpt study-name)
  (-> mailer? string? string? void?)
  (define action-url
    (make-application-url "dashboard"))

  ; FIXME: I have to pass in 'identity_url to this, taken from the DB.
  (mail-adapter-send-email-with-template
   (mailer-adapter m)
   #:to recpt
   #:from (mailer-sender m)
   #:template-alias "next-phase-started"
   #:template-model (mailer-merge-common-variables m
                      'action_url action-url
                      'name recpt
                      'study_name study-name
                      'username recpt)))
