#lang racket/base

(require congame/components/study
         koyo/continuation
         koyo/haml
         koyo/url
         racket/match
         web-server/dispatchers/dispatch
         web-server/http
         congame-web/components/auth
         (prefix-in tpl: congame-web/components/template)
         congame-web/components/user)
         ;"../components/auth.rkt"
         ;"../components/template.rkt"
         ;"../components/user.rkt")

(provide
 study-instances-page
 study-page)

(define ((study-instances-page db) _req)
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
                "Enroll")))))))))))

(define ((enroll db i) _req)
  (enroll-participant! db (user-id (current-user)) (study-instance-id i))
  (redirect/get/forget/protect)
  (redirect-to (reverse-uri 'study-page (study-instance-slug i))))

(define ((study-page db) req slug)
  (cond
    [(lookup-study db slug (user-id (current-user)))
     => (match-lambda
          [(list s participant)
           (unless (study-participant-completed? participant)
             (define manager
               (make-study-manager #:database db
                                   #:participant participant))
             (call-with-study-manager
              manager
              (lambda ()
                (run-study s req)))
             (mark-participant-completed! manager)
             (redirect/get/forget/protect))
           (tpl:page
            (haml
             (:div.container
              ([:data-study-done "yes"])
              (:h1 "Thank You!")
              (:p "You completed the study.")
              (:h3 "Completion code is: 817C6E38"))))])]

    [else
     (next-dispatcher)]))
