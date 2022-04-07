#lang racket/base

(require koyo/database
         koyo/haml
         koyo/url
         racket/contract
         racket/format
         racket/match
         web-server/dispatchers/dispatch
         web-server/http
         "../components/auth.rkt"
         "../components/congame-server.rkt"
         "../components/template.rkt")

(provide
 dashboard-page
 enroll-or-resume-page
 homepage
 tag-study-instances-page)

(define/contract (homepage _req)
  (-> request? response?)
  (page
   (haml
    (.container
     (:h1 "Studies and Assignments")
     (:p "Sign up " (:a ([:href (reverse-uri 'signup-page)]) "here" ) " if you have received an invitation to participate in one of our studies or in a course using our platform for assignments.")))))

(struct data (server tags instances))

; FIXME: Only get the data we need, e.g. tags or tags and instances
(define (data-by-server db)
  (sort
   (for/list ([server (in-list (get-congame-servers db))])
     (data server
           (congame-server-tags server)
           (congame-server-study-instances server)))
   #:key (compose1 congame-server-name data-server) string<?))

(define/contract ((dashboard-page db) _req)
  (-> database? (-> request? response?))
  (page
   (haml
    (.container
     (:h1 "Studies and Assignments")
     ,@(for/list ([d (in-list (data-by-server db))])
         (match-define (data server tags _instances) d)
         (haml
          (.server
           (:h1 (congame-server-name server))
           (.section
            (:h4 "Tags")
            (:ul
             ,@(for/list ([(t name&instances) (in-hash tags)])
                 (haml
                  (:li (:a ([:href
                             (reverse-uri 'tag-study-instances-page t)])
                           (~a (hash-ref name&instances 'name)))))))))))))))

(define/contract ((tag-study-instances-page db) _req tag-id)
  (-> database? (-> request? integer? response?))
  (page
   (haml
    (.container
     ,@(for/list ([d (in-list (data-by-server db))])
         (match-define (data server tags instances) d)
         (define t (hash-ref tags tag-id))
         (define tag-instances
           (filter (λ (instance)
                     (member (study-instance-id instance)
                             (hash-ref t 'instances '())))
                   instances))
         (haml
          (.section
           ; FIXME: Also include name of congame server
           (:h4 (format "~a Studies" (hash-ref t 'name)))
           (:ul
            ,@(for/list ([instance (in-list tag-instances)])
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
    (congame-server-enroll-user! db server (current-user) instance-id))
  (displayln target-url)
  (flush-output)
  (redirect-to target-url))
