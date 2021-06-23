#lang racket/base

(require congame/components/study
         koyo/continuation
         koyo/haml
         koyo/url
         racket/match
         web-server/dispatchers/dispatch
         web-server/http
         "../components/auth.rkt"
         (prefix-in tpl: "../components/template.rkt")
         "../components/user.rkt")

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
              (run-study s req)))
           (raise-user-error 'study-page "reached past the end of the study~n  user-id: ~a~n  slug: ~a" uid slug)])]

    [else
     (next-dispatcher)]))
