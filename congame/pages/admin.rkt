#lang racket/base

(require (for-syntax racket/base)
         deta
         gregor
         koyo/continuation
         koyo/database
         koyo/haml
         koyo/url
         (except-in forms form)
         racket/contract
         racket/format
         racket/match
         racket/port
         racket/pretty
         threading
         web-server/dispatchers/dispatch
         web-server/http
         "../components/registry.rkt"
         "../components/study.rkt"
         "../components/template.rkt"
         "../studies/all.rkt")

(provide
 studies-page
 create-study-page
 view-study-page
 create-study-instance-page
 edit-study-instance-page
 view-study-instance-page
 view-study-participant-page)

(define/contract ((studies-page db) _req)
  (-> database? (-> request? response?))
  (define studies (list-studies db))
  (page
   (container
    (haml
     (:section.studies
      (:h1 "Studies")
      (:h4
       (:a
        ([:href (reverse-uri 'admin:create-study-page)])
        "New Study"))
      (:ul.study-list
       ,@(for/list ([s (in-list studies)])
           (haml
            (:li
             (:a
              ([:href (reverse-uri 'admin:view-study-page (study-meta-id s))])
              (study-meta-name s)))))))))))

(define (slugify s)
  (~> s
      (string-downcase)
      (regexp-replace " +" _ "-")
      (regexp-replace* "[^a-z0-9-]" _ "")))

(define create-study-form
  (form* ([name (ensure binding/text (required))]
          [slug (ensure binding/text)]
          [study-id (ensure binding/text (required) (one-of (for/list ([id (in-hash-keys (get-registered-studies))])
                                                                (cons (~a id) id))))])
    (list name (or slug (slugify name)) study-id)))

(define ((field-group label [w (widget-text)]) name value errors)
  (haml
   (.field-group
    (:label label " " (w name value errors))
    ,@((widget-errors) name value errors))))

(define (render-study-form target rw)
  (haml
   (:form
    ([:action target]
     [:method "POST"])
    (rw "name" (field-group "Name"))
    (rw "slug" (field-group "Slug"))
    (rw "study-id" (field-group "Study ID"
                                (widget-select (cons
                                                (cons "" "Please select a study")
                                                (for/list ([id (in-hash-keys (get-registered-studies))])
                                                  (cons (~a id) (~a id)))))))
    (:button
     ([:type "submit"])
     "Create"))))

(define/contract ((create-study-page db) req)
  (-> database? (-> request? response?))
  (let loop ([req req])
    (send/suspend/dispatch/protect
     (lambda (embed/url)
       (match (form-run create-study-form req)
         [(list 'passed (list name slug id) _)
          (define the-study
            (with-database-connection [conn db]
              (insert-one! conn (make-study-meta
                                 #:name name
                                 #:slug slug
                                 #:racket-id id))))

          (redirect-to (reverse-uri 'admin:studies-page))]

         [(list _ _ rw)
          (page
           (container
            (haml
             (:section.create-study
              (:h1 "Create Study")
              (render-study-form (embed/url loop) rw)))))])))))

(define/contract ((view-study-page db) _req study-id)
  (-> database? (-> request? id/c response?))
  (unless (lookup-study-meta db study-id)
    (next-dispatcher))
  (define instances
    (list-study-instances db study-id))
  (page
   (container
    (haml
     (:section.studies
      (:h1 "Instances")
      (:h4
       (:a
        ([:href (reverse-uri 'admin:create-study-instance-page study-id)])
        "New Instance"))
      (:ul.study-list
       ,@(for/list ([s (in-list instances)])
           (haml
            (:li
             (:a
              ([:href (reverse-uri 'admin:view-study-instance-page study-id (study-instance-id s))])
              (study-instance-name s)))))))))))

(define study-instance-form
  (form* ([name (ensure binding/text (required))]
          [slug (ensure binding/text)]
          [status (ensure binding/text (required) (one-of '(("active" . active)
                                                            ("inactive" . inactive)
                                                            ("archived" . archived))))])
    (list name (or slug (slugify name)) status)))

(define (render-study-instance-form target rw [submit-label "Create"])
  (haml
   (:form
    ([:action target]
     [:method "POST"])
    (rw "name" (field-group "Name"))
    (rw "slug" (field-group "Slug"))
    (rw "status" (field-group "Status" (widget-select '(("active"   . "Active")
                                                        ("inactive" . "Inactive")
                                                        ("archived" . "Archived")))))
    (:button
     ([:type "submit"])
     submit-label))))

(define/contract ((create-study-instance-page db) req study-id)
  (-> database? (-> request? id/c response?))
  (let loop ([req req])
    (send/suspend/dispatch/protect
     (lambda (embed/url)
       (match (form-run study-instance-form req)
         [(list 'passed (list name slug status) _)
          (define the-study-instance
            (with-database-connection [conn db]
              (insert-one! conn (make-study-instance
                                 #:study-id study-id
                                 #:name name
                                 #:slug slug
                                 #:status status))))

          (redirect-to (reverse-uri 'admin:view-study-page study-id))]

         [(list _ _ rw)
          (page
           (container
            (haml
             (:section.create-study
              (:h1 "Create Study Instance")
              (render-study-instance-form (embed/url loop) rw)))))])))))

(define/contract ((edit-study-instance-page db) req study-id study-instance-id)
  (-> database? (-> request? id/c id/c response?))
  (let loop ([req req])
    (define the-instance (lookup-study-instance db study-instance-id))
    (unless the-instance
      (next-dispatcher))
    (send/suspend/dispatch/protect
     (lambda (embed/url)
       (define defaults
         (hash "name" (study-instance-name the-instance)
               "slug" (study-instance-slug the-instance)
               "status" (~a (study-instance-status the-instance))))
       (match (form-run study-instance-form req #:defaults defaults)
         [(list 'passed (list name slug status) _)
          (with-database-connection [conn db]
            (update-one! conn (~> the-instance
                                  (set-study-instance-name name)
                                  (set-study-instance-slug slug)
                                  (set-study-instance-status status))))
          (redirect-to (reverse-uri 'admin:view-study-instance-page study-id study-instance-id))]

         [(list _ _ rw)
          (page
           (container
            (haml
             (:section.edit-study-instance
              (:h1 "Edit Instance")
              (render-study-instance-form (embed/url loop) rw "Update")))))])))))

(define/contract ((view-study-instance-page db) _req study-id study-instance-id)
  (-> database? (-> request? id/c id/c response?))
  (define the-instance (lookup-study-instance db study-instance-id))
  (unless the-instance
    (next-dispatcher))
  (define participants
    (list-study-instance-participants/admin db study-instance-id))
  (page
   (container
    (haml
     (:section.study-instance
      (:h1 (study-instance-name the-instance))
      (:h4
       (:a
        ([:href (reverse-uri 'admin:edit-study-instance-page study-id study-instance-id)])
        "Edit"))
      (:table.table
       (:tr
        (:th "Slug")
        (:td (study-instance-slug the-instance)))
       (:tr
        (:th "Status")
        (:td (~a (study-instance-status the-instance)))))
      (:h2 "Participants")
      (:table.table
       (:thead
        (:tr
         (:th "Participant ID")
         (:th "Email")
         (:th "Completed?")
         (:th "Enrolled At")
         (:th "Progress")))
       (:tbody
        ,@(for/list ([p (in-list participants)])
            (haml
             (:tr
              (:td
               (:a
                ([:href (reverse-uri 'admin:view-study-participant-page study-id study-instance-id (study-participant/admin-id p))])
                (~a (study-participant/admin-id p))))
              (:td (study-participant/admin-email p))
              (:td (if (study-participant/admin-completed? p) "yes" "no"))
              (:td (~t (study-participant/admin-enrolled-at p) "YYYY-MM-dd hh:mm:ss"))
              (:td (~a (study-participant/admin-progress p)))))))))))))

(define/contract ((view-study-participant-page db) _req study-id study-instance-id participant-id)
  (-> database? (-> request? id/c id/c id/c response?))
  (define the-study
    (lookup-study-meta db study-id))
  (define the-instance
    (lookup-study-instance db study-instance-id))
  (define the-participant
    (lookup-study-participant/admin db participant-id))
  (unless (and the-study the-instance the-participant)
    (next-dispatcher))
  (send/suspend/dispatch/protect
   (lambda (embed/url)
     (page
      (container
       (haml
        (:section.study-participant
         (:h1 (study-participant/admin-email the-participant))
         (:h4 "Instance '" (study-instance-name the-instance) "' of study '" (study-meta-name the-study) "'")
         (:h4
          (:a
           ([:onclick "return confirm('Are you sure?')"]
            [:href (embed/url
                    (lambda (_req)
                      (clear-participant-progress! db participant-id)
                      (redirect/get/forget/protect)
                      (redirect-to (reverse-uri 'admin:view-study-participant-page study-id study-instance-id participant-id))))])
           "Clear Participant Progress"))
         (:table.table
          (:thead
           (:tr
            (:th "Stack")
            (:th "ID")
            (:th "First Put At")
            (:th "Last Put At")
            (:th "Value")))
          (:tbody
           ,@(for/list ([v (in-list (lookup-study-vars db participant-id))])
               (haml
                (:tr
                 (:td (:pre (~a (study-var-stack v))))
                 (:td (~a (study-var-id v)))
                 (:td (~t (study-var-first-put-at v) "YYYY-MM-dd hh:mm:ss"))
                 (:td (~t (study-var-last-put-at v) "YYYY-MM-dd hh:mm:ss"))
                 (:td (:pre
                       (with-output-to-string
                         (lambda ()
                           (pretty-print (study-var-value/deserialized v))))))))))))))))))
