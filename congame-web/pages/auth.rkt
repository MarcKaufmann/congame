#lang racket/base

(require congame-web/components/auth
         congame-web/components/auth-token
         congame-web/components/mail
         congame-web/components/prolific
         congame-web/components/template
         congame-web/components/user
         congame-web/pages/forms
         (prefix-in core: congame/components/study)
         forms
         koyo/continuation
         koyo/database
         koyo/flash
         koyo/haml
         koyo/http
         koyo/l10n
         koyo/url
         racket/contract
         racket/match
         racket/string
         threading
         web-server/dispatchers/dispatch
         web-server/servlet)

(provide
 anon-login-page)

(define/contract ((anon-login-page auth db users) _req slug)
  (-> auth-manager? database? user-manager? (-> request? string? response?))
  (define the-instance (core:lookup-study-instance/by-slug db slug))
  (unless the-instance
    (next-dispatcher))
  (define the-user
    (user-manager-create-anon! users))
  (auth-manager-login!/nopass auth the-user)
  (core:enroll-participant! db (user-id the-user) (core:study-instance-id the-instance))
  (redirect-to (reverse-uri 'study-page slug)))


;; token login ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 token-login-page)

(define/contract ((token-login-page auth db) req token)
  (-> auth-manager? database? (-> request? string? response?))
  (define bindings (request-bindings/raw req))
  (define maybe-user
    (lookup-user-by-auth-token db token))
  (unless maybe-user
    (next-dispatcher))
  (auth-manager-login!/nopass auth maybe-user)
  (redirect-to (bindings-ref bindings 'return-url)))


;; login & logout ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 login-page
 logout-page)

(define/contract ((login-page auth) req)
  (-> auth-manager? (-> request? response?))

  (define return-url
    (bindings-ref (request-bindings/raw req) 'return (reverse-uri 'study-instances-page)))

  (define defaults
    (hash "username" (get-prolific-email)))

  (let loop ([req req])
    (send/suspend/dispatch/protect
     (lambda (embed/url)
       (define (render render-widget [error-message #f])
         (page
          #:subtitle (translate 'subtitle-log-in)
          (container
           (render-login-form (embed/url loop) render-widget error-message))))

       (match (form-run login-form req #:defaults defaults)
         [(list 'passed (list username password) render-widget)
          (with-handlers ([exn:fail:auth-manager:unverified?
                           (lambda _
                             (render render-widget (translate 'error-verify-email)))])
            (cond
              [(auth-manager-login! auth username password)
               (redirect-to return-url)]

              [else
               (render render-widget (translate 'error-invalid-credentials))]))]

         [(list _ _ render-widget)
          (render render-widget)])))))

(define/contract ((logout-page auth) req)
  (-> auth-manager? (-> request? response?))
  (auth-manager-logout! auth)
  (redirect-to (reverse-uri 'login-page)))

(define login-form
  (form* ([username (ensure binding/email (required))]
          [password (ensure binding/text (required))])
    (list username password)))

(define (render-login-form target render-widget [error-message #f])
  (haml
   (:form.form.form--login
    ([:action target]
     [:method "POST"])
    (when error-message
      (haml
       (:ul.form__errors
        (:li error-message))))

    (:h1.form__title (translate 'subtitle-log-in))

    (render-widget "username" (username-field))
    (render-widget "password" (password-field))

    (:button.button.button--primary
     ([:type "submit"])
     (translate 'action-log-in))

    ;; Remove signup button to avoid random signups which lead to spam complaints.
    #;(:a.button.button--secondary
     ([:href (reverse-uri 'signup-page)])
     (translate 'action-sign-up-no-account))

    (:a.button
     ([:href (reverse-uri 'request-password-reset-page)])
     (translate 'action-reset-password)))))


;; signup & verify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 signup-page
 verify-page)

(define/contract ((signup-page auth mailer users) req)
  (-> auth-manager? mailer? user-manager? (-> request? response?))
  (define defaults
    (hash "username" (get-prolific-email)))

  (send/suspend/dispatch/protect
   (lambda (embed/url)
     (define (render render-widget [error-message #f])
       (page
        #:subtitle (translate 'subtitle-sign-up)
        (container
         (render-signup-form (embed/url (signup-page auth mailer users)) render-widget error-message))))

     (match (form-run signup-form req #:defaults defaults)
       [(list 'passed (list username password) render-widget)
        (with-handlers ([exn:fail:user-manager:username-taken?
                         (lambda _
                           (render render-widget (translate 'error-username-taken)))])
          (define user (user-manager-create! users username password))
          (mailer-send-welcome-email mailer user)
          (post-signup-page (redirect/get/forget) username))]

       [(list _ _ render-widget)
        (render render-widget)]))))

(define (post-signup-page req username)
  (page
   #:subtitle (translate 'subtitle-signed-up)
   (haml
    (.container
     (:h1 (translate 'subtitle-signed-up))
     (:p (translate
          (if (prolific-username? username)
              'message-post-sign-up-prolific
              'message-post-sign-up)))
     (:p (if (prolific-username? username)
             (haml
              (:a ((:href "https://app.prolific.co/messages/inbox"))
                 "Message inbox on prolific"))
             ""))))))

(define/contract ((verify-page flashes users) req user-id verification-code)
  (-> flash-manager? user-manager? (-> request? integer? string? response?))
  (user-manager-verify! users user-id verification-code)
  (flash 'success (translate 'message-email-verified))
  (redirect-to (reverse-uri 'login-page)))

(define signup-form
  (form* ([username (ensure binding/email (required))]
          [password (ensure binding/text (required) (longer-than 7))])
    (list username password)))

(define (render-signup-form target render-widget [error-message #f])
  (haml
   (:form.form.form--signup
    ([:action target]
     [:method "POST"])

    (:h1.form__title (translate 'subtitle-sign-up))

    (when error-message
      (haml
       (:ul.form__errors
        (:li error-message))))

    (render-widget "username" (username-field))
    (render-widget "password" (password-field))

    (:button.button.button--primary
     ([:type "submit"])
     (translate 'action-sign-up))

    (:a.button.button--secondary
     ([:href (reverse-uri 'login-page)])
     (translate 'action-log-in-signed-up)))))


;; password reset ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 request-password-reset-page
 password-reset-page)

(define/contract ((request-password-reset-page flashes mailer users) req)
  (-> flash-manager? mailer? user-manager? (-> request? response?))
  (let loop ([req req])
    (send/suspend/dispatch/protect
     (lambda (embed/url)
       (match (form-run request-password-reset-form req)
         [(list 'passed username render-widget)
          (define-values (user token)
            (user-manager-create-reset-token! users
                                              #:username username
                                              #:ip-address (request-client-ip req)
                                              #:user-agent (or (and~> (request-headers/raw req)
                                                                      (headers-assq* #"user-agent" _)
                                                                      (header-value)
                                                                      (bytes->string/utf-8))
                                                               "Unknown")))

          (when (and user token)
            (mailer-send-password-reset-email mailer user token))

          (flash 'success (translate 'message-password-reset-requested))
          (redirect/get/forget/protect)
          (redirect-to (reverse-uri 'login-page))]

         [(list _ _ render-widget)
          (page
           #:subtitle (translate 'subtitle-request-password-reset)
           (haml
            (.container
             (render-request-password-reset-form (embed/url loop) render-widget))))])))))

(define request-password-reset-form
  (form* ([username (ensure binding/email (required))])
    username))

(define (render-request-password-reset-form target render-widget)
  (haml
   (:form.form.form--password-reset
    ([:action target]
     [:method "POST"])

    (:h1.form__title (translate 'subtitle-request-password-reset))

    (render-widget "username" (username-field))

    (:button.button.button--primary
     ([:type "submit"])
     (translate 'action-request-password-reset)))))

(define/contract ((password-reset-page flashes mailer users) req user-id token)
  (-> flash-manager? mailer? user-manager? (-> request? id/c non-empty-string? response?))
  (let loop ([req req])
    (send/suspend/dispatch/protect
     (lambda (embed/url)
       (match (form-run password-reset-form req)
         [(list 'passed password render-widget)
          (define reset?
            (user-manager-reset-password! users
                                          #:user-id user-id
                                          #:token token
                                          #:password password))

          (if reset?
              (flash 'success (translate 'message-password-reset-success))
              (flash 'error (translate 'message-password-reset-error)))

          (redirect-to (reverse-uri 'login-page))]

         [(list _ _ render-widget)
          (page
           #:subtitle (translate 'subtitle-reset-password)
           (haml
            (.container
             (render-password-reset-form (embed/url loop) render-widget))))])))))

(define password-reset-form
  (form* ([password (ensure binding/text (required) (longer-than 7))])
    password))

(define (render-password-reset-form target render-widget)
  (haml
   (:form.form.form--password-reset
    ([:action target]
     [:method "POST"])

    (:h1.form__title (translate 'subtitle-reset-password))

    (render-widget "password" (password-field))

    (:button.button.button--primary
     ([:type "submit"])
     (translate 'action-reset-password)))))
