#lang racket/base

(require koyo/database
         koyo/haml
         racket/contract
         racket/format
         web-server/http
         "../components/congame-server.rkt"
         "../components/study-instance.rkt"
         "../components/template.rkt"
         "../components/user.rkt")

(provide
 (prefix-out admin: users-study-instance-data-page))

(define/contract ((users-study-instance-data-page db) _req)
  (-> database? (-> request? response?))
  (define users (list-all-usernames-and-ids db))
  ; (server-id instance-id) -> data
  (define grouped-data
    (for/fold ([res (hash)]
               #:result (for/hash ([(k v) (in-hash res)])
                          (values k (reverse v))))
              ([data (in-list (list-all-study-instance-data/admin db))])
      (define k
        (cons
         (study-instance-data-server-id data)
         (study-instance-data-instance-id data)))
      (hash-update res k (λ (ds) (cons data ds)) null)))
  (define servers-by-id
    (for/hash ([s (in-list (get-congame-servers db))])
      (values (congame-server-id s) s)))
  (define study-instances-by-server-id
    (for/hash ([server (in-hash-values servers-by-id)])
      (values (congame-server-id server)
              (congame-server-study-instances server))))
  (define congame-servers-with-data
    (map car (hash-keys grouped-data)))
  (page
   (haml
    (.container
     (:h1 "Study data")
     ,@(for*/list ([id (in-list congame-servers-with-data)]
                   [server (in-value (hash-ref servers-by-id id))])
         (haml
          (:div
           (:h2 "Congame Server: " (congame-server-name server))
           ,@(for*/list ([the-instance (in-list (hash-ref study-instances-by-server-id id null))]
                         [k (in-value (cons id (study-instance-id the-instance)))]
                         [data (in-value (hash-ref grouped-data k null))]
                         #:unless (null? data))
               (haml
                (:div
                 (:h3 "Study Instance: " (~a (study-instance-name the-instance)))
                 (:table.table
                  (:thead
                   (:tr
                    (:th "User")
                    (:th "Username")
                    (:th "Stack")
                    (:th "Key")
                    (:th "Value")))
                  (:tbody
                   ,@(for/list ([d (in-list data)])
                       (define uid (study-instance-data-user-id d))
                       (haml
                        (:tr
                         (:td (~a uid))
                         (:td (~a (hash-ref users uid "#<Error: no user with this id found>")))
                         (:td (~a (study-instance-data-study-stack d)))
                         (:td (~a (study-instance-data-key d)))
                         (:td (~a (study-instance-data-value d))))))))))))))))))
