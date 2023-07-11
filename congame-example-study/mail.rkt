#lang racket/base

(require koyo/mail
         koyo/url
         racket/contract)

(provide
 mailer-send-next-phase-started-email)

(define/contract (mailer-send-next-phase-started-email m recpt
                                                       #:study-name study-name
                                                       #:identity-url identity-url)
  (-> mailer? string?
      #:study-name string?
      #:identity-url string?
      void?)
  (define action-url
    (make-application-url "dashboard"))

  (mail-adapter-send-email-with-template
   (mailer-adapter m)
   #:to recpt
   #:from (mailer-sender m)
   #:template-alias "next-phase-started"
   #:template-model (mailer-merge-common-variables m
                      'action_url action-url
                      'name recpt
                      'study_name study-name
                      'username recpt
                      'identity_url identity-url)))
