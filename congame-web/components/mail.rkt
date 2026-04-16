#lang racket/base

(require congame-web/components/user
         koyo/mail
         koyo/url
         racket/contract/base
         racket/contract/region
         racket/string)

(provide
 (all-from-out koyo/mail)
 mailer-send-password-reset-email
 mailer-send-welcome-email
 mailer-send-generic-participant-email)

(define/contract (mailer-send-welcome-email m user)
  (-> mailer? user? void?)

  (define action-url
    (make-application-url "verify" (number->string (user-id user)) (user-verification-code user)))

  (define login-url
    (make-application-url "login"))

  (mail-adapter-send-email-with-template
   (mailer-adapter m)
   #:to (user-username user)
   #:from (mailer-sender m)
   #:template-alias "welcome"
   #:template-model (mailer-merge-common-variables m
                      'action_url action-url
                      'name (user-username user)
                      'login_url login-url
                      'username (user-username user))))

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

(define/contract (mailer-send-generic-participant-email m user subject content)
  (-> mailer? user? string? string? void?)
  (mail-adapter-send-email
   (mailer-adapter m)
   #:to (user-username user)
   #:from (mailer-sender m)
   #:subject subject
   #:text-content content))

;; Local Variables:
;; eval: (put 'mailer-merge-common-variables 'racket-indent-function #'begin)
;; End:
