#lang racket/base

(require koyo/database
         koyo/haml
         koyo/url
         racket/contract
         racket/match
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
  (struct data (server tags instances))
  (define data-by-server
    (sort
     (for/list ([server (in-list (get-congame-servers db))])
       (data server
             (congame-server-tags server)
             (congame-server-study-instances server (current-user))))
     #:key (compose1 congame-server-name data-server) string<?))

  (page
   (haml
    (.container
     ,@(for/list ([d (in-list data-by-server)])
         (match-define (data server tags instances) d)
         (haml
          (.server
           (:h1 (congame-server-name server))
           (.section
            (:h4 "Tags")
            (:ul
             ,@(for/list ([name (in-list tags)])
                 (haml
                  (:li name)))))
           (.section
            (:h4 "Studies")
            (:ul
             ,@(for/list ([instance (in-list instances)])
                 (haml
                  (:li
                   (:a
                    ([:href (reverse-uri 'enroll-or-resume-page
                                         (congame-server-id server)
                                         (study-instance-id instance))])
                    (study-instance-name instance))))))))))))))

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
