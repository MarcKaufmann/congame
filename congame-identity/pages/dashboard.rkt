#lang racket/base

(require koyo/database
         koyo/haml
         koyo/url
         racket/contract
         web-server/dispatchers/dispatch
         web-server/http
         "../components/auth.rkt"
         "../components/congame-server.rkt"
         "../components/template.rkt")

(provide
 dashboard-page
 enroll-or-resume-page)

(define/contract ((dashboard-page db) _req)
  (-> database? (-> request? response?))
  (define instances-by-server
    (sort
     (for/list ([server (in-list (get-congame-servers db))])
       (cons server (congame-server-study-instances server (current-user))))
     #:key (compose1 congame-server-name car) string<?))

  (page
   (haml
    (.container
     ,@(for/list ([p (in-list instances-by-server)])
         (define server (car p))
         (define instances (cdr p))
         (haml
          (.server
           (:h1 (congame-server-name server))
           (:ul
            ,@(for/list ([instance (in-list instances)])
                (haml
                 (:li
                  (:a
                   ([:href (reverse-uri 'enroll-or-resume-page
                                        (congame-server-id server)
                                        (study-instance-id instance))])
                   (study-instance-name instance)))))))))))))

(define/contract ((enroll-or-resume-page db) _req server-id instance-id)
  (-> database? (-> request? integer? integer? response?))
  (define server (get-congame-server db server-id))
  (unless server
    (next-dispatcher))
  (define target-url
    (congame-server-enroll-user! server (current-user) instance-id))
  (displayln target-url)
  (flush-output)
  (redirect-to target-url))
