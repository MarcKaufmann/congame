#lang racket/base

(require db
         (submod congame/components/bot actions)
         congame-web/components/auth
         congame-web/components/bot-set
         congame-web/components/replication
         congame-web/components/tag
         (prefix-in tpl: congame-web/components/template)
         congame-web/components/upload
         congame-web/components/user
         congame-web/pages/render
         congame-web/studies/all ;; required for its effects
         congame/components/dsl
         congame/components/export
         congame/components/instance-link
         congame/components/registry
         congame/components/study
         congame/components/transition-graph
         deta
         file/zip
         gregor
         koyo
         (except-in forms form)
         net/url
         racket/contract
         racket/file
         racket/format
         racket/match
         racket/port
         racket/pretty
         racket/promise
         racket/string
         racket/vector
         sentry
         threading
         web-server/dispatchers/dispatch
         web-server/http
         (prefix-in config: "../config.rkt"))

(provide
 studies-page
 create-study-page
 view-study-page
 edit-study-dsl-page
 create-study-instance-page
 edit-study-instance-page
 view-study-instance-page
 create-study-instance-link-page
 view-study-participant-page
 create-study-instance-bot-sets-page
 view-study-instance-bot-set-page
 bulk-archive-instances-page
 stop-impersonation-page
 create-replication-page
 upsert-cli-study-page)

(define datetime-format
  "YYYY-MM-dd HH:mm:ss")

(define/contract ((studies-page db) req)
  (-> database? (-> request? response?))
  (define tab (or (bindings-ref-symbol (request-bindings/raw req) 'tab) 'all))
  (define studies
    (list-studies
     #:owner (and
              (eq? tab 'my-studies)
              (user-id (current-user)))
     db))
  (define replications (list-replications db))
  (define tags (list-tags db))
  (send/suspend/dispatch/protect
   (lambda (embed/url)
     (tpl:page
      (tpl:container
       (haml
        (:div
         (:section.studies
          (:h1
           "Studies ("
           (:a ([:href (reverse-uri 'admin:studies-page #:query '((tab . "all")))]) "all")
           " | "
           (:a ([:href (reverse-uri 'admin:studies-page #:query '((tab . "my-studies")))]) "my studies")
           ")")
          (:h4
           (:a
            ([:href (reverse-uri 'admin:create-study-page)])
            "New Study")
           " "
           (:a
            ([:href (reverse-uri 'admin:bulk-archive-instances-page)])
            "Bulk Archive"))
          (:ul.study-list
           ,@(for/list ([s (in-list studies)])
               (haml
                (:li
                 (:a
                  ([:href (reverse-uri 'admin:view-study-page (study-meta-id s))])
                  (study-meta-name s)) (format "(~a)" (study-meta-racket-id s)))))))
         (:section.replications
          (:h1 "Replications")
          (:h4
           (:a
            ([:href (reverse-uri 'admin:create-replication-page)])
            "New Replication"))
          (:ul.replication-list
           ,@(for/list ([r (in-list replications)])
               (haml
                (:li
                 (replication-slug r) " (" (replication-git-sha r) ") "
                 (:a
                  ([:onclick "return confirm('Are you sure?')"]
                   [:href (embed/url
                           (lambda (_req)
                             (delete-replication! db r)
                             (redirect-to (reverse-uri 'admin:studies-page))))])
                  "Delete"))))))
         (:section.tags
          (:h1 "Tags")
          (:h4
           (:a
            ([:href (reverse-uri 'admin:create-tag-page)])
            "New Tag")
           (:ul.tag-list
            ,@(for/list ([t (in-list tags)])
                (haml
                 (:li
                  (:a
                   ([:href (reverse-uri 'admin:view-tag-page (tag-id t))])
                   (tag-name t)))))))))))))))

(define (slugify s)
  (~> s
      (string-downcase)
      (regexp-replace " +" _ "-")
      (regexp-replace* "[^a-z0-9-]" _ "")))

(define (make-create-study-form db)
  (form* ([name (ensure binding/text (required))]
          [slug (ensure binding/text)]
          [type (ensure binding/text (required) (one-of '(("racket" . racket)
                                                          ("dsl" . dsl))))]
          [study-id (ensure binding/text (one-of (for/list ([id (in-hash-keys (get-registered-studies))])
                                                   (cons (~a id) id))))]
          [dsl-id (ensure binding/symbol)]
          [dsl-source (ensure binding/file)])
    (let ([slug (or slug (slugify name))])
      (if (lookup-study-meta/by-slug db slug)
          (err '((slug . "This slug is taken by another study.")))
          (case type
            [(racket)
             (if (not study-id)
                 (err '((study-id . "required for Racket-based studies")))
                 (ok (list name slug type study-id #f)))]
            [(dsl)
             (if (and dsl-id dsl-source)
                 (with-handlers ([exn:fail? (λ (e)
                                              (sentry-capture-exception! e)
                                              (err `((dsl-source . ,(exn-message e)))))])
                   (define source
                     (binding:file->dsl-source dsl-source dsl-id))
                   (ok (list name slug type dsl-id source)))
                 (err (filter
                       cdr
                       `((dsl-id . ,(and (not dsl-id) "required for Conscript studies"))
                         (dsl-source . ,(and (not dsl-source) "required for Conscript studies"))))))])))))

(define (binding:file->dsl-source b dsl-id)
  (define content-type
    (and~>
     (headers-assq* #"content-type" (binding:file-headers b))
     (header-value)))
  (case content-type
    [(#"application/zip" #"application/x-zip-compressed")
     (define archive-path (save-file! b))
     (dsl-require
      #;src `(archive ,archive-path)
      #;study-id dsl-id
      #;owner-is-admin? (user-admin-like? (current-user)))
     `(archive ,archive-path)]
    [else
     (define source
       (bytes->string/utf-8
        (binding:file-content b)))
     (dsl-require
      #;src source
      #;study-id dsl-id
      #;owner-is-admin? (user-admin-like? (current-user))) ;; for effect
     `(source ,source)]))

(define ((field-group label [w (widget-text)] [ew (widget-errors)]) name value errors)
  (haml
   (.field-group
    (:label label " " (w name value errors))
    ,@(ew name value errors))))

(define (tooltip label info)
  (haml
   (:span
    label " "
    (:span
     ([:title info])
     "ℹ️"))))

(define (render-study-form target rw)
  (haml
   (:form
    ([:action target]
     [:method "POST"]
     [:enctype "multipart/form-data"])
    (rw "name" (field-group (tooltip "Name" "A descriptive study name that is unique across all studies on the server.  Must only contain alphanumeric characters and spaces.")))
    (rw "slug" (field-group "Slug (optional)"))
    (rw "type" (field-group (tooltip "Type" "Leave as-is for now.")
                            (widget-select
                             #:attributes `([data-mask "type"])
                             `(("dsl" . "Conscript")
                               ("racket" . "Racket")))))
    (:div
     ([:data-mask-group "racket"]
      [:style "display: none"])
     (rw "study-id"
         (field-group
          "Study ID"
          (widget-select (cons
                          (cons "" "Please select a study")
                          (for/list ([id (in-hash-keys (get-registered-studies))])
                            (cons (~a id) (~a id))))))))
    (:div
     ([:data-mask-group "dsl"])
     (rw "dsl-id"
         (field-group
          (tooltip "Study ID" "The id of the provided study in your source code.")
          (widget-text)))
     (rw "dsl-source"
         (field-group
          (tooltip "Study Source" "The file that contains your study. While all extensions work, .rkt is encouraged.")
          (widget-file)
          (widget-preformatted-error))))
    (:button
     ([:type "submit"])
     "Create"))))

(define/contract ((create-study-page db) req)
  (-> database? (-> request? response?))
  (let loop ([req req])
    (send/suspend/dispatch/protect
     (lambda (embed/url)
       (match (form-run (make-create-study-form db) req)
         [`(passed (,name ,slug ,type ,id ,dsl-source) ,_)
          (define the-study
            (with-database-connection [conn db]
              (insert-one! conn (make-study-meta
                                 #:owner-id (user-id (current-user))
                                 #:name name
                                 #:slug slug
                                 #:type type
                                 #:racket-id id
                                 #:dsl-source (match dsl-source
                                                [`(source ,src) src]
                                                [_ ""])
                                 #:dsl-archive-path (match dsl-source
                                                      [`(archive ,path) path]
                                                      [_ sql-null])))))

          (redirect-to (reverse-uri 'admin:view-study-page (study-meta-id the-study)))]

         [`(,_ ,_ ,rw)
          (tpl:page
           (tpl:container
            (haml
             (:section.create-study
              (:h1 "Create Study")
              (render-study-form (embed/url loop) rw)))))])))))

(define/contract ((view-study-page db) _req study-id)
  (-> database? (-> request? id/c response?))
  (define meta (lookup-study-meta db study-id))
  (unless meta (next-dispatcher))
  (define u (current-user))
  (send/suspend/dispatch/protect
   (lambda (embed/url)
     (define (make-actions instance-id)
       (if config:debug
           (haml
            (:td
             (:a
              ([:href (embed/url
                       (lambda (_req)
                         (clear-study-instance-data! db instance-id)
                         (redirect/get/forget/protect)
                         (redirect-to (reverse-uri 'admin:view-study-page study-id))))]
               [:onclick "return confirm('Are you sure?')"])
              "Clear All Data")
             " "
             (:a
              ([:href (embed/url
                       (lambda (_req)
                         (clear-study-instance-data-for-user! db instance-id (user-id (current-user)))
                         (redirect/get/forget/protect)
                         (redirect-to (reverse-uri 'admin:view-study-page study-id))))]
               [:onclick "return confirm('Are you sure?')"])
              "Clear My Data")))
           (haml (:td))))
     (define instances-xexpr
       (cond
         [(user-researcher? u)
          (haml
           (:table.study-list
            (:tr
             (:th "Instance Name")
             (:th "Created")
             (:th "Actions"))
            ,@(for/list ([s (in-list (list-study-instances-for-owner db study-id (user-id u)))])
                (haml
                 (:tr
                  (:td
                   (:a
                    ([:href (reverse-uri 'admin:view-study-instance-page study-id (study-instance-id s))])
                    (study-instance-name s)))
                  (:td
                   (~t (study-instance-created-at s) "yyyy-MM-dd, HH:mm"))
                  (make-actions (study-instance-id s)))))))]
         [else
          (define instances-by-researcher
            (for/fold ([insts (hash)])
                      ([r (in-list (list-study-instances-by-researcher db study-id))])
              (hash-update insts
                           (researcher&instance-researcher-username r)
                           (λ (lst) (cons r lst))
                           null)))
          (haml
           (:div.researcher-list
            ,@(for/list ([(username instances) (in-hash instances-by-researcher)])
                (haml
                 (:div
                  (:p "Researcher: " (:strong (~a username)))
                  (:table.study-list
                   (:tr
                    (:th "Instance Name")
                    (:th "Created")
                    (:th "Actions"))
                   ,@(for/list ([in (in-list instances)])
                       (haml
                        (:tr
                         (:td
                          (:a
                           ([:href (reverse-uri 'admin:view-study-instance-page study-id (researcher&instance-instance-id in))])
                           (researcher&instance-instance-name in)))
                         (:td
                          (~t (researcher&instance-created-at in) "yyyy-MM-dd, HH:mm"))
                         (make-actions (researcher&instance-instance-id in)))))))))))]))
     (tpl:page
      (tpl:container
       (haml
        (:section.studies
         (:h1 (format "Instances of ~a" (study-meta-racket-id meta)))
         (:h4
          (:a
           ([:href (reverse-uri 'admin:create-study-instance-page study-id)])
           "New Instance"))
         (when (eq? 'dsl (study-meta-type meta))
           (haml
            (:div
             (:h4
              (:a
               ([:href (reverse-uri 'admin:edit-study-dsl-page study-id)])
               "Edit Conscript Source"))
             (:h4
              (:a
               ([:href (embed/url
                        (lambda (_req)
                          (define uploader (current-uploader))
                          (define archive-path
                            (study-meta-dsl-archive-path meta))
                          (define filename
                            (if (sql-null? archive-path)
                                "study.rkt"
                                "study.zip"))
                          (response/output
                           #:mime-type #"text/plain"
                           #:headers (list
                                      (make-header
                                       #"content-disposition"
                                       (string->bytes/utf-8
                                        (format "attachment; filename=\"~a\"" filename))))
                           (lambda (out)
                             (if (sql-null? archive-path)
                                 (write-string (study-meta-dsl-source meta) out)
                                 (parameterize ([current-uploader uploader])
                                   (call-with-uploaded-file
                                    archive-path
                                    (lambda (in)
                                      (copy-port in out)))))))))])
               "Download Conscript Source")))))
         instances-xexpr
         (:br)
         (:a
          ([:href (embed/url
                   (λ (_req)
                     (define owner-is-admin?
                       (with-database-connection [conn db]
                         (~> (from user #:as u)
                             (where (= u.id ,(study-meta-owner-id meta)))
                             (lookup conn _)
                             (user-admin-like?))))
                     (define the-study
                       (lookup-study* meta owner-is-admin?))
                     (define temp-dir
                       (make-temporary-file (~a (study-meta-name meta) "~a") 'directory))
                     (define pdfs
                       (list*
                        (cons (study-meta-name meta)
                              (comptime-transitions->pdf (study-transitions the-study)))
                        (for/list ([sub (in-list (study-steps the-study))]
                                   #:when (step/study? sub)
                                   #:unless (procedure? (step/study-study sub)))
                          (cons (step-id sub)
                                (comptime-transitions->pdf (study-transitions (step/study-study sub)))))))
                     (define filenames
                       (for/list ([p (in-list pdfs)])
                         (define id (car p))
                         (define src-path (cdr p))
                         (define dst-path (build-path temp-dir (~a id ".pdf")))
                         (define-values (_dst-dir dst-filename _dst-ext)
                           (split-path dst-path))
                         (begin0 dst-filename
                           (copy-file src-path dst-path))))
                     (response/output
                      #:headers (list
                                 (header #"Content-type"
                                         #"application/zip")
                                 (header #"Content-disposition"
                                         #"attachment; filename=\"transition-graphs.zip\""))
                      (lambda (out)
                        (parameterize ([current-directory temp-dir])
                          (zip->output
                           #:path-prefix "transition-graphs"
                           filenames out))))))])
          "Generate transition graph"))))))))

(define edit-study-dsl-form
  (form* ([dsl-id (ensure binding/symbol (required))]
          [dsl-source (ensure binding/file (required))])
    (with-handlers ([exn:fail? (λ (e)
                                 (sentry-capture-exception! e)
                                 (err `((dsl-source . ,(exn-message e)))))])
      (list dsl-id (binding:file->dsl-source dsl-source dsl-id)))))

(define (render-edit-study-dsl-form target rw)
  (haml
   (:form
    ([:action target]
     [:method "POST"]
     [:enctype "multipart/form-data"])
    (rw "dsl-id" (field-group "Study ID"))
    (rw "dsl-source" (field-group "Study Source"
                                  (widget-file)
                                  (widget-preformatted-error)))
    (:button
     ([:type "submit"])
     "Update"))))

(define ((widget-preformatted-error) name _value errors)
  (define source-error
    (assq (string->symbol name) errors))
  (if source-error
      `((pre ([class "errors"]) ,(cdr source-error)))
      null))

(define/contract ((edit-study-dsl-page db) req study-id)
  (-> database? (-> request? id/c response?))
  (define meta
    (lookup-study-meta db study-id))
  (unless meta
    (next-dispatcher))

  (let loop ([req req])
    (send/suspend/dispatch/protect
     (lambda (embed/url)
       (define defaults
         (hash "dsl-id" (~a (study-meta-racket-id meta))))

       (match (form-run edit-study-dsl-form req #:defaults defaults)
         [(list 'passed (list dsl-id dsl-source) _)
          (with-database-connection [conn db]
            (~> meta
                (set-study-meta-racket-id dsl-id)
                (set-study-meta-dsl-source
                 (match dsl-source
                   [`(source ,source) source]
                   [_ ""]))
                (set-study-meta-dsl-archive-path
                 (match dsl-source
                   [`(archive ,path) path]
                   [_ sql-null]))
                (update-one! conn _)))
          (redirect-to (reverse-uri 'admin:view-study-page study-id))]

         [(list _ _ rw)
          (tpl:page
           (tpl:container
            (haml
             (:section.create-study
              (:h1 "Edit Conscript Source")
              (render-edit-study-dsl-form (embed/url loop) rw)))))])))))

(define (make-study-instance-form db [maybe-instance #f])
  (form* ([name (ensure binding/text (required))]
          [slug (ensure binding/text)]
          [enrollment-code (ensure binding/text)]
          [no-enrollment-code? (ensure binding/boolean)]
          [status (ensure binding/text (required) (one-of '(("active" . active)
                                                            ("inactive" . inactive)
                                                            ("archived" . archived))))])
    (let ([slug (or slug (slugify name))]
          [enrollment-code (if no-enrollment-code? "" (or enrollment-code (generate-random-string 16)))])
      (define maybe-other-instance
        (lookup-study-instance/by-slug db slug))
      (define maybe-instance-id
        (and maybe-instance (study-instance-id maybe-instance)))
      (define maybe-other-instance-id
        (and maybe-other-instance (study-instance-id maybe-other-instance)))
      (if (eqv? maybe-instance-id (or maybe-other-instance-id maybe-instance-id))
          (list name slug enrollment-code status)
          (err '((slug . "This slug is being used by another instance.")))))))

(define (render-study-instance-form target rw [submit-label "Create"])
  (haml
   (:form
    ([:action target]
     [:method "POST"])
    (rw "name" (field-group "Name"))
    (rw "slug" (field-group "Slug"))
    (rw "enrollment-code" (field-group "Enrollment Code"))
    (rw "no-enrollment-code?" (field-group "No enrollment code?" (widget-checkbox)))
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
       (define defaults
         (hash "no-enrollment-code?" "1"))
       (match (form-run (make-study-instance-form db) req #:defaults defaults)
         [(list 'passed (list name slug enrollment-code status) _)
          (with-database-connection [conn db]
            (insert-one! conn (make-study-instance
                               #:owner-id (user-id (current-user))
                               #:study-id study-id
                               #:name name
                               #:slug slug
                               #:enrollment-code enrollment-code
                               #:status status)))
          (redirect-to (reverse-uri 'admin:view-study-page study-id))]

         [(list _ _ rw)
          (tpl:page
           (tpl:container
            (haml
             (:section.create-study
              (:h1 "Create Study Instance")
              (render-study-instance-form (embed/url loop) rw)))))])))))

(define/contract ((edit-study-instance-page db) req study-id study-instance-id)
  (-> database? (-> request? id/c id/c response?))
  (let loop ([req req])
    (define the-instance (lookup-study-instance/checked db study-instance-id))
    (unless the-instance
      (next-dispatcher))
    (send/suspend/dispatch/protect
     (lambda (embed/url)
       (define defaults
         (hash "name" (study-instance-name the-instance)
               "slug" (study-instance-slug the-instance)
               "enrollment-code" (study-instance-enrollment-code the-instance)
               "status" (~a (study-instance-status the-instance))))
       ;; Only add the no-enrollment-code? key if we want the checkbox
       ;; to be checked.  It will be checked regardless of the value
       ;; at that key in the hash when present.
       (let ([defaults (if (equal? "" (study-instance-enrollment-code the-instance))
                           (hash-set defaults "no-enrollment-code?" "1")
                           defaults)])
         (match (form-run (make-study-instance-form db the-instance) req #:defaults defaults)
           [`(passed (,name ,slug ,enrollment-code ,status) ,_)
            (with-database-connection [conn db]
              (update-one! conn (~> the-instance
                                    (set-study-instance-name name)
                                    (set-study-instance-slug slug)
                                    (set-study-instance-enrollment-code enrollment-code)
                                    (set-study-instance-status status))))
            (redirect-to (reverse-uri 'admin:view-study-instance-page study-id study-instance-id))]

           [`(,(or 'pending 'failed) ,_ ,rw)
            (tpl:page
             (tpl:container
              (haml
               (:section.edit-study-instance
                (:h1 "Edit Instance")
                (render-study-instance-form (embed/url loop) rw "Update")))))]))))))

(define (lookup-study-instance/checked db instance-id)
  (user-roles-case (current-user)
    [(researcher) (lookup-study-instance-for-researcher db instance-id (user-id (current-user)))]
    [else (lookup-study-instance db instance-id)]))

(define/contract ((view-study-instance-page db) _req study-id study-instance-id)
  (-> database? (-> request? id/c id/c response?))
  (define the-instance (lookup-study-instance/checked db study-instance-id))
  (unless the-instance
    (next-dispatcher))
  (define participants
    (list-study-instance-participants/admin db study-instance-id))
  (define vars
    (list-study-instance-vars db study-instance-id))
  (define bot-sets
    (list-bot-sets db study-instance-id))
  (define links
    (list-instance-links/admin db study-instance-id))
  (send/suspend/dispatch/protect
   (lambda (embed/url)
     (tpl:page
      (tpl:container
       (haml
        (:section.study-instance
         (:h1
          (:a
           ([:href (reverse-uri 'admin:view-study-page study-id)])
           (study-instance-name the-instance)))
         (:h4
          (:a
           ([:href (reverse-uri 'admin:edit-study-instance-page study-id study-instance-id)])
           "Edit"))
         (:table.table
          (:tr
           (:th "Slug")
           (:td (study-instance-slug the-instance)))
          (:tr
           (:th "Enrollment Code")
           (:td (study-instance-enrollment-code the-instance)))
          (:tr
           (:th "Status")
           (:td (~a (study-instance-status the-instance)))))
         (:h4
          (:a
           ([:href
             (embed/url
              (lambda (_req)
                (define vars
                  (list-study-instance-vars db study-instance-id))
                (response/jsexpr
                 (study-instance->jsexpr db study-id study-instance-id vars participants))))])
           "Export JSON"))
         (:h2 "Bot Sets")
         (:h3
          (:a
           ([:href (reverse-uri 'admin:create-study-instance-bot-sets-page study-id study-instance-id)])
           "Create Bot Set"))
         (:table.table
          (:thead
           (:tr
            (:th "Bot Set ID")
            (:th "Bot Count")))
          (:tbody
           ,@(for/list ([bs (in-list bot-sets)])
               (haml
                (:tr
                 (:td
                  (:a
                   ([:href (reverse-uri 'admin:view-study-instance-bot-set-page study-id study-instance-id (bot-set-id bs))])
                   (~a (bot-set-id bs))))
                 (:td
                  (~a (bot-set-bot-count bs))))))))
         (:h2 "Instance Links")
         (:h3
          (:a
           ([:href (reverse-uri 'admin:create-study-instance-link-page study-id study-instance-id)])
           "Create Link"))
         (haml
          (:table.table
           (:thead
            (:tr
             (:th "Other Instance")
             (:th "Pseudonym")
             (:th "Relationship (to other)")
             (:th "Status")
             (:th "")))
           (:tbody
            ,@(for/list ([l (in-list links)])
                (haml
                 (:tr
                  (:td
                   (:a
                    ([:href (reverse-uri
                             'admin:view-study-instance-page
                             (instance-link/admin-other-study-id l)
                             (instance-link/admin-other-id l))])
                    (instance-link/admin-other-name l)))
                  (:td (instance-link/admin-pseudonym l))
                  (:td (string-titlecase (~a (instance-link/admin-relationship l))))
                  (:td (if (instance-link/admin-linked? l) "Linked" "Not Linked"))
                  (:td (:a
                        ([:href (embed/url
                                 (lambda (_req)
                                   (delete-instance-link/admin db l)
                                   (redirect-to
                                    (reverse-uri
                                     'admin:view-study-instance-page
                                     study-id
                                     study-instance-id))))]
                         [:onclick "return confirm('Are you sure?')"])
                        "Delete"))))))))
         (:h2 "Instance Data")
         (:h3
          (:a
           ([:href
             (embed/url
              (lambda (_req)
                (clear-study-instance-vars! db study-instance-id)
                (redirect/get/forget/protect)
                (redirect-to (reverse-uri 'admin:view-study-instance-page study-id study-instance-id))))]
            [:onclick "return confirm('Are you sure?')"])
           "Clear Instance Data"))
         (render-study-instance-vars vars)
         (:h2 "Participants")
         (render-participant-list study-id study-instance-id participants))))))))

(define (render-study-instance-vars vars)
  (haml
   (:table.table.table--admin
    (:thead
     (:tr
      (:th "Stack")
      (:th "ID")
      (:th "First Put At")
      (:th "Last Put At")
      (:th "Value")))
    (:tbody
     ,@(for/list ([v (in-list vars)])
         (haml
          (:tr
           (:td (:pre (~a (study-instance-var-stack v))))
           (:td (~a (study-instance-var-id v)))
           (:td (~t (study-instance-var-first-put-at v) datetime-format))
           (:td (~t (study-instance-var-last-put-at v) datetime-format))
           (:td (study-var-details v)))))))))

(define (render-participant-list study-id study-instance-id participants)
  (haml
   (:table.table.table--admin
    (:thead
     (:tr
      (:th "Participant ID")
      (:th "Email")
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
           (:td (~t (study-participant/admin-enrolled-at p) datetime-format))
           (:td (~a (study-participant/admin-progress p))))))))))

(define/contract ((create-study-instance-link-page db) req study-id instance-id)
  (-> database? (-> request? id/c id/c response?))
  (send/suspend/dispatch/protect
   (lambda (embed/url)
     (define all-studies-by-id
       (for/hasheqv ([s (in-list (list-studies db))])
         (values (study-meta-id s) s)))
     (define all-study-instances
       (list-all-study-instances db))
     (define option-groups
       (for/fold ([instances (hasheqv)]
                  #:result (sort
                            (for/list ([(id is) (in-hash instances)])
                              (define s (hash-ref all-studies-by-id id))
                              (define options
                                (for/list ([i (in-list (sort is #:key study-instance-name string<?))])
                                  (cons (number->string (study-instance-id i))
                                        (study-instance-name i))))
                              (list (study-meta-name s) options))
                            #:key car
                            string<?))
                 ([i (in-list all-study-instances)])
         (define id (study-instance-study-id i))
         (hash-update instances id (λ (is) (cons i is)) null)))
     (define the-form
       (form* ([instance-id (ensure binding/number
                                    (required)
                                    (one-of
                                     (for/list ([i (in-list all-study-instances)])
                                       (cons (study-instance-id i)
                                             (study-instance-id i)))))]
               [pseudonym (ensure binding/symbol (required))]
               [relationship (ensure binding/text
                                     (required)
                                     (one-of `(("source" . source)
                                               ("reporter" . reporter))))])
         (list instance-id pseudonym relationship)))
     (let loop ([req req])
       (match (form-run the-form req)
         [`(passed ,(list instance-id-b pseudonym relationship) ,_)
          (redirect/get/forget/protect)
          (with-database-connection [conn db]
            (insert-one!
             conn
             (make-study-instance-link
              #:study-instance-id-a instance-id
              #:study-instance-id-b instance-id-b
              #:pseudonym-b pseudonym
              #:relationship relationship)))
          (redirect-to (reverse-uri 'admin:view-study-instance-page study-id instance-id))]
         [`(,_ ,_ ,rw)
          (tpl:page
           (tpl:container
            (haml
             (:div
              (:h1 "Create Study Instance Link")
              (:form
               ([:action (embed/url loop)]
                [:method "POST"])
               (rw "instance-id" (field-group "Instance ID:" (widget-select option-groups)))
               (rw "pseudonym" (field-group "Pseudonym:" (widget-text)))
               (rw "relationship" (field-group "Relationship:" (widget-select
                                                                `(("source" . "Source")
                                                                  ("reporter" . "Reporter")))))
               (:button.button
                ([:type "submit"])
                "Create Link"))))))])))))

(define impersonator-return-key
  'impersonator:return-url)

;; TODO: Stop showing e-mail and show participant ID instead.
(define/contract ((view-study-participant-page auth db) req study-id study-instance-id participant-id)
  (-> auth-manager? database? (-> request? id/c id/c id/c response?))
  (define the-study
    (lookup-study-meta db study-id))
  (define the-instance
    (lookup-study-instance/checked db study-instance-id))
  (define the-participant
    (lookup-study-participant/admin db participant-id))
  (unless (and the-study the-instance the-participant)
    (next-dispatcher))
  (define vars
    (lookup-study-vars db participant-id))
  (send/suspend/dispatch/protect
   (lambda (embed/url)
     (tpl:page
      (tpl:container
       (haml
        (:section.study-participant
         (:h1 (study-participant/admin-email the-participant))
         (:h4
          "Instance '"
          (:a
           ([:href (reverse-uri 'admin:view-study-instance-page study-id study-instance-id)])
           (study-instance-name the-instance))
          "' of study '"
          (:a
           ([:href (reverse-uri 'admin:view-study-page study-id)])
           (study-meta-name the-study))
          "'")
         (:h4
          (:a
           ([:onclick "return confirm('Are you sure?')"]
            [:href (embed/url
                    (lambda (_req)
                      (clear-participant-progress! db participant-id)
                      (redirect/get/forget/protect)
                      (redirect-to (reverse-uri 'admin:view-study-participant-page study-id study-instance-id participant-id))))])
           "Clear Participant Progress"))
         (:h4
          (:a
           ([:href (embed/url
                    (lambda (_req)
                      (response/json
                       (hash
                        'participant-id participant-id
                        'instance-id study-instance-id
                        'study-id study-id
                        'vars (map ->jsexpr vars)))))])
           "Export JSON"))
         (unless (vector-member 'admin (study-participant/admin-roles the-participant))
           (haml
            (:h4
             (:a
              ([:href
                (embed/url
                 (lambda (_req)
                   (session-set! impersonator-return-key (url->string (request-uri req)))
                   (auth-manager-impersonate! auth (study-participant/admin-user-id the-participant))
                   (redirect/get/forget/protect)
                   (redirect-to (reverse-uri 'study-page (study-instance-slug the-instance)))))])
              "Impersonate User"))))
         (:table.table.table--admin
          (:thead
           (:tr
            (:th "Stack")
            (:th "Round")
            (:th "Group")
            (:th "ID")
            (:th "First Put At")
            (:th "Last Put At")
            (:th "Value")))
          (:tbody
           ,@(for/list ([v (in-list vars)])
               (haml
                (:tr
                 (:td (:pre (~s (study-var-stack v))))
                 (:td (:pre (~s (study-var-round-stack v))))
                 (:td (:pre (~s (study-var-group-stack v))))
                 (:td (~a (study-var-id v)))
                 (:td (~t (study-var-first-put-at v) datetime-format))
                 (:td (~t (study-var-last-put-at v) datetime-format))
                 (:td (study-var-details v))))))))))))))

(define (study-var-details var)
  (haml
   (:details
    (:summary (:code (~study-var-summary var)))
    (:pre (~study-var var)))))

(define/contract ((create-study-instance-bot-sets-page db) req study-id study-instance-id)
  (-> database? (-> request? id/c id/c response?))
  (define the-study
    (lookup-study-meta db study-id))
  (define the-instance
    (lookup-study-instance/checked db study-instance-id))
  (unless (and the-study the-instance)
    (next-dispatcher))
  (define study-racket-id
    (study-meta-racket-id the-study))
  (define bot-form
    (make-study-bot-set-form study-racket-id))
  (let bot-loop ([req req])
    (send/suspend/dispatch/protect
     (lambda (embed/url)
       (match (form-run bot-form req)
         [(list 'passed (list info bot-count) _)
          ;; Fudge the method so the second `form-run' doesn't
          ;; immediately think this is a submission.
          (define req* (struct-copy request req [method #"GET"]))
          (let model-loop ([req req*])
            (match (form-run (make-study-bot-set-model-form info) req)
              [(list 'passed model _)
               (define the-bot-set
                 (create-bot-set! db
                                  #:study-id study-id
                                  #:study-instance-id study-instance-id
                                  #:bot-id (bot-info-id info)
                                  #:model-id (object-name model)
                                  #:bot-count bot-count))
               (redirect-to
                (reverse-uri 'admin:view-study-instance-bot-set-page
                             study-id
                             study-instance-id
                             (bot-set-id the-bot-set)))]

              [(list _ _ rw)
               (tpl:page
                (tpl:container
                 (haml
                  (:section.create-bot-set
                   (:h1 "Select a model")
                   (:form
                    ([:action (embed/url model-loop)]
                     [:method "POST"])
                    (:label
                     "Model"
                     (rw "model" (widget-select
                                  (for/list ([(id _) (in-hash (bot-info-models info))])
                                    (define id* (symbol->string id))
                                    (cons id* id*)))))
                    (:button
                     ([:type "submit"])
                     "Create Bot Set"))))))]))]

         [(list _ _ rw)
          (tpl:page
           (tpl:container
            (haml
             (:section.create-bot-set
              (:h1 "Create Bot Set")
              (:form
               ([:action (embed/url bot-loop)]
                [:method "POST"])
               (:label
                "Bot "
                (rw "bot" (widget-select
                           (for/list ([(id _) (in-hash (get-bot-infos-for-study study-racket-id))])
                             (define id* (symbol->string id))
                             (cons id* id*)))))
               (:br)
               (:label
                "Count "
                (rw "bot-count" (widget-number))
                ,@(rw "bot-count" (widget-errors)))
               (:br)
               (:button
                ([:type "submit"])
                "Next"))))))])))))

(define (make-study-bot-set-form study-id)
  (form* ([bot (ensure binding/symbol
                       (required)
                       (one-of
                        (for/list ([(id info) (in-hash (get-bot-infos-for-study study-id))])
                          (cons id info))))]
          [bot-count (ensure binding/number (required) (lambda (n)
                                                         (if (exact-positive-integer? n)
                                                             (ok n)
                                                             (err "count must be a positive integer"))))])
    (list bot bot-count)))

(define (make-study-bot-set-model-form info)
  (form* ([model (ensure binding/symbol (required) (one-of
                                                    (for/list ([(id model) (in-hash (bot-info-models info))])
                                                      (cons id model))))])
    model))

(define bot-set-run-form
  (form* ([concurrency (ensure binding/number)]
          [headless? (ensure binding/boolean)])
    (list (or concurrency 3) headless?)))

(define (render-bot-set-run-form target rw)
  (haml
   (:form
    ([:action target]
     [:method "POST"])
    (rw "concurrency" (field-group "Concurrency"))
    (rw "headless?" (field-group "Headless?" (widget-checkbox)))
    (:button ([:type "sumbit"]) "Run"))))

(define/contract ((view-study-instance-bot-set-page db um) req study-id study-instance-id bot-set-id)
  (-> database? user-manager? (-> request? id/c id/c id/c response?))
  (define the-study
    (lookup-study-meta db study-id))
  (define the-instance
    (lookup-study-instance/checked db study-instance-id))
  (define the-bot-set
    (lookup-bot-set db bot-set-id))
  (unless (and the-study the-instance the-bot-set)
    (next-dispatcher))
  (define participants
    (list-study-instance-participants/admin db study-instance-id bot-set-id))
  (send/suspend/dispatch/protect
   (lambda (embed/url)
     (tpl:page
      (tpl:container
       (haml
        (:section.bot-set
         (:h1
          "Bot set for "
          (:a
           ([:href (reverse-uri 'admin:view-study-instance-page study-id study-instance-id)])
           (study-instance-name the-instance)))
         (:h3 "Model " (~a (bot-set-model-id the-bot-set)))
         (let loop ([req req])
           (define defaults
             (hash "concurrency" "3"
                   "headless?" "1"))
           (match (form-run bot-set-run-form req #:defaults defaults)
             [`(passed (,concurrency ,headless?) ,_)
              (define-values (wait-for-bots kill-bots)
                (run-bot-set
                 db um the-study the-instance the-bot-set
                 #:concurrency concurrency
                 #:headless? headless?))
              (let wait-loop ([req req]) ;; noqa
                (if (sync/timeout 30 (thread wait-for-bots))
                    (redirect-to
                     (reverse-uri
                      'admin:view-study-instance-page
                      study-id
                      study-instance-id))
                    (response/xexpr
                     (haml
                      (:div
                       (:p "The bots aren't done yet.")
                       (:ul
                        (:li
                         (:a
                          ([:href (embed/url wait-loop)])
                          "Wait 30 More Seconds"))
                        (:li
                         (:a
                          ([:href (embed/url
                                   (lambda (_req)
                                     (kill-bots)
                                     (redirect-to
                                      (reverse-uri
                                       'admin:view-study-instance-page
                                       study-id
                                       study-instance-id))))])
                          "Kill Bots"))))))))]

             [`(,(or 'pending 'failed) ,_ ,rw)
              (render-bot-set-run-form (embed/url loop) rw)]))
         (:br)
         (:a
          ([:onclick "return confirm('Are you sure?')"]
           [:href (embed/url
                   (λ (_req)
                     (delete-bot-set! db the-bot-set)
                     (redirect/get/forget/protect)
                     (redirect-to (reverse-uri 'admin:view-study-instance-page study-id study-instance-id))))])
          "Delete")
         (:h2 "Participants")
         (render-participant-list study-id study-instance-id participants))))))))

(define (run-bot-set db um the-study the-instance the-set
                     #:concurrency [concurrency 3]
                     #:headless? [headless? #t])
  (define-values (password users)
    (prepare-bot-set! db um the-set))
  (define study-racket-id
    (study-meta-racket-id the-study))
  (define bot-infos
    (get-bot-infos-for-study study-racket-id))
  (define bot-info
    (hash-ref bot-infos (bot-set-bot-id the-set)))
  (define bot
    (bot-info-bot bot-info))
  (define model
    (hash-ref
     (bot-info-models bot-info)
     (bot-set-model-id the-set)))
  (define custodian
    (make-custodian))
  (define promises
    (parameterize ([current-custodian custodian]
                   [current-subprocess-custodian-mode 'interrupt])
      (define sema
        (make-semaphore concurrency))
      (for/list ([u (in-list users)]
                 [p (in-naturals 60100)])
        (delay/thread
         (call-with-semaphore sema
           (lambda ()
             ;; FIXME: Rename study-page to study-instance-page.
             (define participant
               (with-database-connection [conn db]
                 (~> (from study-participant #:as p)
                     (where (and (= p.instance-id ,(study-instance-id the-instance))
                                 (= p.user-id ,(user-id u))))
                     (lookup conn _))))
             (call-with-study-manager
              (make-study-manager
               #:database db
               #:participant participant)
              (lambda ()
                (run-bot
                 #:study-url (apply
                              make-application-url
                              (string-split
                               (reverse-uri 'study-page (study-instance-slug the-instance))
                               "/"))
                 #:username (user-username u)
                 #:password password
                 #:headless? headless?
                 #:port p
                 (bot model))))))))))
  (values
   (λ () (for-each force promises))
   (λ () (custodian-shutdown-all custodian))))

(define/contract ((bulk-archive-instances-page db) req)
  (-> database? (-> request? response?))
  (define studies (list-studies db #:owner (user-id (current-user))))
  (define instances (list-active-study-instances/by-owner db (user-id (current-user))))
  (define instances-by-study
    (for/fold ([instances-by-study (hasheqv)])
              ([i (in-list instances)])
      (hash-update
       instances-by-study (study-instance-study-id i)
       (λ (is) (cons i is)) null)))
  (send/suspend/dispatch/protect
   (lambda (embed/url)
     (let loop ([req req])
       (cond
         [(equal? (request-method req) #"POST")
          (define bindings (request-bindings/raw req))
          (define instance-ids
            (for/list ([bind (in-list (bindings-assq-all #"instances" bindings))])
              (string->number
               (bytes->string/utf-8
                (binding:form-value bind)))))
          (bulk-archive-study-instances! db (user-id (current-user)) instance-ids)
          (redirect/get/forget/protect)
          (redirect-to (reverse-uri 'admin:studies-page))]
         [else
          (tpl:page
           (tpl:container
            (haml
             (:div
              (:h1 "Bulk Archive Study Instances")
              (:form
               ([:action (embed/url loop)]
                [:method "POST"])
               (:label
                "Study Instances"
                (:br)
                (:select
                 ([:name "instances"]
                  [:multiple ""]
                  [:style "height: 200px"])
                 ,@(for/list ([s (in-list studies)]
                              #:when (hash-has-key? instances-by-study (study-meta-id s)))
                     (haml
                      (:optgroup
                       ([:label (study-meta-name s)])
                       ,@(for/list ([i (in-list (hash-ref instances-by-study (study-meta-id s)))])
                           (haml
                            (:option
                             ([:value (~a (study-instance-id i))])
                             (study-instance-name i)))))))))
               (:br)
               (:button
                ([:type "submit"])
                "Archive"))))))])))))

(define/contract ((stop-impersonation-page am) _req)
  (-> auth-manager? (-> request? response?))
  (when (impostor?)
    (auth-manager-stop-impersonation! am))
  (define return-url
    (or (session-ref impersonator-return-key #f)
        (reverse-uri 'study-instances-page)))
  (redirect-to return-url))

(define (~study-var-summary var)
  (format "~.a" (print-study-var
                 var
                 (lambda (v)
                   (parameterize ([pretty-print-columns 40]
                                  [pretty-print-depth 0])
                     (pretty-write v))))))

(define (~study-var var)
  (print-study-var var))

(define (print-study-var var [do-print pretty-write])
  (with-handlers ([exn:fail? (λ (e) (format "<error: ~.a>" (exn-message e)))])
    (with-output-to-string
      (lambda ()
        (parameterize ([pretty-print-columns 40])
          (do-print (->jsexpr (if (study-instance-var? var)
                                  (study-instance-var-value/deserialized var)
                                  (study-var-value/deserialized var)))))))))


;; tags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 create-tag-page
 view-tag-page)

(define create-tag-form
  (form* ([name (ensure binding/text (required))])
    name))

(define/contract ((create-tag-page db) req)
  (-> database? (-> request? response?))
  (let loop ([req req])
    (send/suspend/dispatch/protect
     (lambda (embed/url)
       (match (form-run create-tag-form req)
         [`(passed ,name ,_)
          (define the-tag (create-tag! db name))
          (redirect-to (reverse-uri 'admin:view-tag-page (tag-id the-tag)))]

         [`(,_ ,_ ,rw)
          (tpl:page
           (tpl:container
            (haml
             (:section.create-tag
              (:h1 "Create Tag")
              (:form
               ([:action (embed/url loop)]
                [:method "POST"])
               (rw "name" (field-group "Name"))
               (:button ([:type "submit"]) "Create"))))))])))))

(define associate-tag-form
  (form* ([instance-ids (ensure binding/text (required))])
    instance-ids))

(define/contract ((view-tag-page db) req tag-id)
  (-> database? (-> request? id/c response?))
  (define the-tag (lookup-tag db tag-id))
  (unless the-tag (next-dispatcher))
  (let loop ([req req])
    (send/suspend/dispatch/protect
     (lambda (embed/url)
       (match (form-run associate-tag-form req)
         [`(passed ,_ ,_)
          (define instance-ids
            (map (compose1 string->number bytes->string/utf-8 binding:form-value)
                 (bindings-assq-all #"instance-ids" (request-bindings/raw req))))
          (associate-tags! db tag-id instance-ids)
          (redirect-to (reverse-uri 'admin:view-tag-page tag-id))]

         [`(,_ ,_ ,rw)
          (define instance-ids
            (get-tag-instance-ids db tag-id))
          (tpl:page
           (tpl:container
            (haml
             (:section
              (:h1 (tag-name the-tag))
              (:h4 "Instances")
              (:form
               ([:action (embed/url loop)]
                [:method "POST"])
               (:select
                ([:name "instance-ids"]
                 [:multiple ""]
                 [:size "24"])
                ,@(for/list ([i (in-list (list-all-study-instances db))])
                    (define id (study-instance-id i))
                    `(option
                      ([value ,(~a id)]
                       ,@(if (member id instance-ids)
                             `((selected ""))
                             `()))
                      ,(study-instance-name i))))
               (:br)
               (:button
                ([:type "submit"])
                "Save"))))))])))))

(define replication-form
  (form* ([slug (ensure binding/text (required))]
          [git-sha (ensure binding/text (required))]
          [admin-username (ensure binding/text (required))]
          [admin-password (ensure binding/text (required))]
          [instance-ids (ensure binding/text (required))])
    (list slug git-sha admin-username admin-password instance-ids)))

(define/contract ((create-replication-page db reps) req)
  (-> database? replication-manager? (-> request? response?))
  (let loop ([req req])
    (send/suspend/dispatch/protect
     (lambda (embed/url)
       (match (form-run replication-form req)
         [`(passed (,slug ,git-sha ,admin-username ,admin-password ,_instance-ids) ,_)
          (create-replication!
           reps
           #:slug slug
           #:git-sha git-sha
           #:admin-username admin-username
           #:admin-password admin-password
           #:instance-ids (~> (request-bindings/raw req)
                              (bindings-assq-all #"instance-ids" _)
                              (map (compose1 string->number bytes->string/utf-8 binding:form-value) _)))
          (redirect-to (reverse-uri 'admin:studies-page))]

         [`(,_ ,_ ,rw)
          (define studies (list-studies db))
          (define study-instances-by-study
            (for/fold ([instances (hasheqv)])
                      ([i (in-list (list-all-study-instances db))])
              (hash-update
               instances
               (study-instance-study-id i)
               (λ (is) (cons i is))
               null)))
          (tpl:page
           (tpl:container
            (haml
             (:section
              (:h1 "Create Replication")
              (:form
               ([:action (embed/url loop)]
                [:method "POST"])
               (:label
                (:p "Slug")
                (rw "slug" (widget-text))
                ,@(rw "slug" (widget-errors)))
               (:label
                (:p "Git SHA")
                (rw "git-sha" (widget-text))
                ,@(rw "git-sha" (widget-errors)))
               (:label
                (:p "Admin Username")
                (rw "admin-username" (widget-text))
                ,@(rw "admin-username" (widget-errors)))
               (:label
                (:p "Admin Password")
                (rw "admin-password" (widget-password))
                ,@(rw "admin-username" (widget-errors)))
               (:label
                (:p "Instances")
                (:select
                 ([:name "instance-ids"]
                  [:multiple ""]
                  [:size "24"])
                 ,@(for/list ([s (in-list studies)])
                     (haml
                      (:optgroup
                       ([:label (study-meta-name s)])
                       ,@(for/list ([i (in-list (hash-ref study-instances-by-study (study-meta-id s) null))])
                           (haml
                            (:option
                             ([:value (~a (study-instance-id i))])
                             (study-instance-name i)))))))))
               (:br)
               (:button
                ([:type "submit"])
                "Create Replication"))))))])))))

;; cli ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((upsert-cli-study-page db) req)
  (match (form-run cli-study-form req)
    [`(passed (,study-id ,study-source) ,_)
     (match-define `(archive ,path) study-source)
     (define slug (format "cli-~a" study-id))
     (define meta
       (with-database-transaction [conn db]
         (~> (cond
               [(lookup-study-meta/by-slug db slug)
                => (lambda (s)
                     (begin0 s
                       (unless (eqv? (study-meta-owner-id s)
                                     (user-id (current-user)))
                         (error 'upsert-cli-study-page "the requested study does not belong to the current user"))
                       (unless (eq? (study-meta-type s) 'dsl)
                         (error 'upsert-cli-study-page "the requested study is not a DSL study"))))]
               [else
                (~> (make-study-meta
                     #:owner-id (user-id (current-user))
                     #:name (symbol->string study-id)
                     #:slug slug
                     #:type 'dsl
                     #:racket-id study-id
                     #:dsl-source ""
                     #:dsl-archive-path path)
                    (insert-one! conn _))])
             (set-study-meta-dsl-archive-path path)
             (update-one! conn _))))
     (response/jsexpr
      (hasheq
       'link (make-application-url "admin" "studies" (number->string (study-meta-id meta)))))]
    [`(,_ ,_ ,_)
     (response/empty #:code 400)]))

(define cli-study-form
  (form* ([study-id (ensure binding/symbol)]
          [study-source (ensure binding/file)])
    (list study-id (binding:file->dsl-source study-source study-id))))
