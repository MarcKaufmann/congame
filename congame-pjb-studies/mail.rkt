#lang racket/base

(require koyo/mail
         koyo/url
         racket/contract)

(provide
 mailer-send-study-completed-email)

(define/contract (mailer-send-study-completed-email m recpt payment)
  (-> mailer? string? string? void?)
  (define action-url
    (make-application-url "study"))

  (mail-adapter-send-email-with-template
   (mailer-adapter m)
   #:to recpt
   #:from (mailer-sender m)
   #:template-alias "study-completed"
   #:template-model (mailer-merge-common-variables m
                      'action_url action-url
                      'name recpt
                      'username recpt
                      'payment payment)))
