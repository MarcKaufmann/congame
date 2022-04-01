#lang racket/base

(require koyo/database
         koyo/haml
         racket/contract
         racket/format
         racket/list
         racket/pretty
         web-server/http
         "../components/auth.rkt"
         "../components/template.rkt"
         "../components/user.rkt"
         "../components/study-instance.rkt"
         congame/components/export)

(provide (prefix-out admin: users-study-instance-data-page))

(define/contract ((users-study-instance-data-page db) _req)
  (-> database? (-> request? response?))
  (define instances (list-all-study-instance-data/admin db))
  (define instance-ids-with-data
    (remove-duplicates
     (map (λ (i)
            (study-instance-data-instance-id i))
          instances)))
  (page
   (haml
    (.container
     (:h1 "Study data")
     ,@(for/list ([id instance-ids-with-data])
         (haml
          (:div
           (:table.table
            (:caption "Study Instance" (~a id))
            (:thead
             (:tr
              (:th "User")
              (:th "Stack")
              (:th "Key")
              (:th "Value")))
            (:tbody
             ,@(for/list ([i instances]
                          #:when (equal? (study-instance-data-instance-id i) id))
                 (haml
                  (:tr
                   (:td (~a (study-instance-data-user-id i)))
                   (:td (~a (study-instance-data-study-stack i)))
                   (:td (~a (study-instance-data-key i)))
                   (:td (~a (study-instance-data-value i)))))))))))))))
