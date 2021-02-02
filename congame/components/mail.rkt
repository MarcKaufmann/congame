#lang racket/base

(require koyo/mail
         koyo/url
         racket/contract
         racket/string
         congame/components/user)

(provide
 (all-from-out koyo/mail)
 mailer-send-welcome-email
 mailer-send-password-reset-email
 mailer-send-study-completed-email)

(define/contract (mailer-send-welcome-email m user)
  (-> mailer? user? void?)

  (define action-url
    (make-application-url "verify" (number->string (user-id user)) (user-verification-code user)))

  (mail-adapter-send-email-with-template
   (mailer-adapter m)
   #:to (user-username user)
   #:from (mailer-sender m)
   #:template-alias "welcome"
   #:template-model (mailer-merge-common-variables m
                      'action_url action-url
                      'name (user-username user)
                      'username (user-username user))))

(define/contract (mailer-send-study-completed-email m recpt payment)
  ; FIXME: Add information on the study that was completed, say the name (or so) of the study
  (-> mailer? string? string? void?)
  (define action-url
    (make-application-url "study"))
  (define login-url
    (make-application-url "login"))

  (mail-adapter-send-email-with-template
   (mailer-adapter m)
   #:to recpt
   #:from (mailer-sender m)
   #:template-alias "study-completed"
   #:template-model (mailer-merge-common-variables m
                      'action_url action-url
                      'name recpt
                      'username recpt
                      'payment payment
                      )))

(define/contract (mailer-send-password-reset-email m user token)
  (-> mailer? user? non-empty-string? void?)

  (define action-url
    (make-application-url "password-reset" (number->string (user-id user)) token))

  (mail-adapter-send-email-with-template
   (mailer-adapter m)
   #:to (user-username user)
   #:from (mailer-sender m)
   #:template-alias "password-reset"
   #:template-model (mailer-merge-common-variables m
                      'action_url action-url
                      'name (user-username user)
                      'username (user-username user))))

;; Local Variables:
;; eval: (put 'mailer-merge-common-variables 'racket-indent-function #'begin)
;; End:
