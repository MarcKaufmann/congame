#lang racket/base

(require (for-syntax racket/base)
         deta
         koyo/continuation
         koyo/database
         koyo/haml
         koyo/url
         (except-in forms form)
         racket/contract
         racket/file
         racket/format
         racket/match
         racket/path
         racket/runtime-path
         threading
         web-server/http
         "../components/study.rkt"
         "../components/template.rkt")

(provide
 studies-page
 create-study-page
 view-study-page
 create-study-instance-page)

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

(define-runtime-path study-modules-path
  (build-path 'up "studies"))

(define known-study-modules
  (let ([paths (find-files (lambda (p)
                             (equal? (path-get-extension p) #".rkt"))
                           (simplify-path study-modules-path))])
    (for/list ([p (in-list paths)])
      (define-values (_folder filename _dir?)
        (split-path p))
      (string->symbol (~a "congame/studies/" (path-replace-extension filename #""))))))

(define (slugify s)
  (~> s
      (string-downcase)
      (regexp-replace " +" _ "-")
      (regexp-replace* "[^a-z0-9-]" _ "")))

(define create-study-form
  (form* ([name (ensure binding/text (required))]
          [slug (ensure binding/text)]
          [racket-module (ensure binding/text
                                 (required)
                                 (one-of (for/list ([m (in-list known-study-modules)])
                                           (cons (~a m) m))))]
          [racket-id (ensure binding/symbol (required))])
    (list name
          (or slug (slugify name))
          racket-module
          racket-id)))

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
    (rw "racket-module" (field-group "Module" (widget-select (cons
                                                              (cons "" "Please select a module")
                                                              (for/list ([m (in-list known-study-modules)])
                                                                (cons (~a m) (~a m)))))))
    (rw "racket-id" (field-group "ID"))
    (:button
     ([:type "submit"])
     "Create"))))

(define/contract ((create-study-page db) req)
  (-> database? (-> request? response?))
  (let loop ([req req])
    (send/suspend/dispatch/protect
     (lambda (embed/url)
       (match (form-run create-study-form req)
         [(list 'passed (list name slug mod id) _)
          (define the-study
            (with-database-connection [conn db]
              (insert-one! conn (make-study-meta
                                 #:name name
                                 #:slug slug
                                 #:racket-module mod
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
             (study-instance-name s))))))))))

(define create-study-instance-form
  (form* ([name (ensure binding/text (required))]
          [slug (ensure binding/text)]
          [status (ensure binding/text (required) (one-of '(("active" . active)
                                                            ("inactive" . inactive)
                                                            ("archived" . archived))))])
    (list name (or slug (slugify name)) status)))

(define (render-study-instance-form target rw)
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
     "Create"))))

(define/contract ((create-study-instance-page db) req study-id)
  (-> database? (-> request? id/c response?))
  (let loop ([req req])
    (send/suspend/dispatch/protect
     (lambda (embed/url)
       (match (form-run create-study-instance-form req)
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
