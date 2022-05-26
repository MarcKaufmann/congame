#lang racket/base

(require congame/components/study
         (except-in forms form)
         koyo/continuation
         koyo/haml
         koyo/url
         racket/match
         web-server/dispatchers/dispatch
         web-server/servlet
         congame-web/components/auth
         (prefix-in tpl: "../components/template.rkt")
         congame-web/components/user)

(provide
 home-page
 study-instances-page
 study-page)

(define ((study-instances-page db) _req)
  (cond [(user-enrolled-via-identity? (current-user))
         (define identity-dashboard-url
           (user-identity-service-url (current-user)))
         (redirect/get/forget)
         (redirect-to identity-dashboard-url)]

        [else
         (send/suspend/dispatch/protect
          (lambda (embed/url)
            (tpl:page
             (haml
              (.container
               (:ul
                ,@(for/list ([i (in-list (list-active-study-instances db))])
                    (haml
                     (:li
                      (study-instance-name i)
                      " " 'mdash " "
                      (:a
                       ([:href (embed/url (enroll db i))])
                       (enroll/resume-message db i)))))))))))]))

(define (home-page _req)
  (tpl:page
   (haml
    (.container
     (:h1 "Total Insight Management")
     (:p "A place for online research studies")))))

(define enroll-form
  (form* ([enrollment-code (ensure binding/text (required))])
    enrollment-code))

(define (render-enrollment-form rw [error-message #f])
  (tpl:page
   (haml
    (.container
     (:form
      ([:action ""]
       [:method "POST"])
      (when error-message
        (haml (:p error-message)))
      (:label
       "Enrollment code:"
       (rw "enrollment-code" (widget-text))
       ,@(rw "enrollment-code" (widget-errors)))
      (:button
       ([:type "submit"])
       "Enroll"))))))

;; TODO: Add error
(define ((enroll db i) req)
  (define uid (user-id (current-user)))
  (define iid (study-instance-id i))
  (define (do-enroll)
    (enroll-participant! db uid iid)
    (redirect-to (reverse-uri 'study-page (study-instance-slug i))))
  (cond
    [(or (participant-enrolled? db uid iid)
         (user-has-role? (current-user) 'bot)
         (equal? "" (study-instance-enrollment-code i)))
     (do-enroll)]
    [else
     (match (form-run enroll-form req)
       [`(passed ,enrollment-code ,rw)
        (if (string=? enrollment-code (study-instance-enrollment-code i))
            (do-enroll)
            (render-enrollment-form rw "Invalid enrollment code."))]

       [`(,_ ,_ ,rw)
        (render-enrollment-form rw)])]))

(define (enroll/resume-message db i)
  (define uid (user-id (current-user)))
  (define iid (study-instance-id i))
  (if (participant-enrolled? db uid iid)
      "Resume"
      "Enroll"))

(define ((study-page db) req slug)
  (define uid (user-id (current-user)))
  (cond
    [(lookup-study db slug uid)
     => (match-lambda
          [(list s participant)
           (call-with-study-manager
            (make-study-manager
             #:database db
             #:participant participant)
            (lambda ()
              (run-study s req)))])]

    [else
     (next-dispatcher)]))
