#lang racket/base

(require koyo/continuation
         koyo/haml
         koyo/url
         racket/match
         web-server/dispatchers/dispatch
         web-server/http
         "../components/auth.rkt"
         "../components/study.rkt"
         "../components/template.rkt"
         "../components/user.rkt"
         "../studies/example.rkt")

(provide
 study-instances-page
 study-page)

(define ((study-instances-page db) _req)
  (send/suspend/dispatch/protect
   (lambda (embed/url)
     (page
      (haml
       (:ul
        ,@(for/list ([i (in-list (list-study-instances db))])
            (haml
             (:li
              (study-instance-name i)
              " " 'mdash " "
              (:a
               ([:href (embed/url (enroll db i))])
               "Enroll"))))))))))

(define ((enroll db i) _req)
  (enroll-participant! db (user-id (current-user)) (study-instance-id i))
  (redirect/get/forget/protect)
  (redirect-to (reverse-uri 'study-page (study-instance-slug i))))

(define ((study-page db) req slug)
  (cond
    [(lookup-study db slug (user-id (current-user)))
     => (match-lambda
          ((list s participant)
           (define manager
             (make-study-manager #:database db
                                 #:participant participant))
           (parameterize ([current-study-manager manager])
             (run-study s req)
             (page '(p "Yer done")))))]

    [else
     (next-dispatcher)]))
