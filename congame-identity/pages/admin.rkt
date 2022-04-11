#lang racket/base

(require koyo/database
         koyo/haml
         racket/contract
         racket/format
         racket/list
         racket/string
         web-server/http
         "../components/auth.rkt"
         "../components/congame-server.rkt"
         "../components/study-instance.rkt"
         "../components/template.rkt"
         "../components/user.rkt"
         congame/components/export)

(provide (prefix-out admin: users-study-instance-data-page))

(define/contract ((users-study-instance-data-page db) _req)
  (-> database? (-> request? response?))
  (define instances (list-all-study-instance-data/admin db))
  (define users (list-all-usernames-and-ids db))
  ; FIXME: Getting the data each time is problematic. Bad performance, if study name changes on congame side it changes here, if study is inactive, we might not see it. On the other hand, it works for now.
  (define study-instance-info
    (for/hash ([server (in-list (get-congame-servers db))])
      (values (congame-server-url server)
              (congame-server-study-instances server (current-user)))))
  (define instance-ids-with-data
    (remove-duplicates
     (map (λ (i)
            (study-instance-data-instance-id i))
          instances)))
  (define congame-urls-with-data
    (remove-duplicates
     (map (λ (i)
            (study-instance-data-congame-url i))
          instances)))
  ; FIXME: display inefficiently runs through all instances for each server and instance id.
  (define (study-instances-for-url url)
    (define url1 url)
    (define url2 (string-replace url "127.0.0.1" "localhost"))
    (define url3 (string-replace url "localhost" "127.0.0.1"))
    (cond [(findf (λ (u)
                    (hash-has-key? study-instance-info u))
                  (list url1 url2 url3))
           => (λ (x) (hash-ref study-instance-info x))]
          [else
           (error "none of the following keys found: " (list url1 url2 url3))]))

  (page
   (haml
    (.container
     (:h1 "Study data")
     ,@(for*/list ([c-url congame-urls-with-data])
         (haml
          (:div
           (:h2 "Congame Server: " (~a c-url))
           ,@(for/list ([id instance-ids-with-data])
               ; FIXME: Instance name will still print headers for instances
               ; that don't exist for this congame server. Deal gracefully with
               ; multiple congame servers.
               (define instance-name
                 (study-instance-name
                  (findf
                   (λ (i)
                     (equal? (study-instance-id i) id))
                   (study-instances-for-url c-url))))
               (haml
                (:div
                 (:h3 "Study Instance: " (~a instance-name) (format " (ID: ~a)" id))
                 (:table.table
                  (:thead
                   (:tr
                    (:th "User")
                    (:th "Username")
                    (:th "Stack")
                    (:th "Key")
                    (:th "Value")))
                  (:tbody
                   ,@(for/list ([i instances]
                                #:when (and
                                        (equal? (study-instance-data-instance-id i) id)
                                        (equal? (study-instance-data-congame-url i) c-url)))
                       (define uid (study-instance-data-user-id i))
                       (haml
                        (:tr
                         (:td (~a uid))
                         (:td (~a (hash-ref users uid "#<Error: no user with this id found>")))
                         (:td (~a (study-instance-data-study-stack i)))
                         (:td (~a (study-instance-data-key i)))
                         (:td (~a (study-instance-data-value i))))))))))))))))))
