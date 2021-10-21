#lang racket/base

(require (for-syntax racket/base)
         (submod congame/components/bot actions)
         congame/components/export
         congame/components/registry
         congame/components/study
         congame/components/transition-graph
         deta
         file/zip
         gregor
         koyo/continuation
         koyo/database
         koyo/haml
         koyo/json
         koyo/random
         koyo/url
         (except-in forms form)
         racket/contract
         racket/file
         racket/format
         (except-in racket/list group-by)
         racket/match
         racket/port
         racket/pretty
         racket/string
         threading
         web-server/dispatchers/dispatch
         web-server/http
         "../components/auth.rkt"
         "../components/bot-set.rkt"
         "../components/prolific.rkt"
         (prefix-in tpl: "../components/template.rkt")
         "../components/user.rkt"
         "../studies/all.rkt"
         "render.rkt")

(provide
 studies-page
 create-study-page
 view-study-page
 create-study-instance-page
 edit-study-instance-page
 view-study-instance-page
 view-study-participant-page
 create-study-instance-bot-sets-page
 view-study-instance-bot-set-page
 stop-impersonation-page)

(define datetime-format
  "YYYY-MM-dd HH:mm:ss")

(define/contract ((studies-page db) _req)
  (-> database? (-> request? response?))
  (define studies (list-studies db))
  (tpl:page
   (tpl:container
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
         [`(passed (,name ,slug ,id) ,_)
          (define the-study
            (with-database-connection [conn db]
              (insert-one! conn (make-study-meta
                                 #:name name
                                 #:slug slug
                                 #:racket-id id))))

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
  (define meta
    (lookup-study-meta db study-id))
  (unless meta
    (next-dispatcher))
  (define instances
    (list-study-instances db study-id))
  (send/suspend/dispatch/protect
   (lambda (embed/url)
     (tpl:page
      (tpl:container
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
                 (study-instance-name s))))))
         (:br)
         (:a
          ([:href (embed/url
                   (λ (_req)
                     (define the-study
                       (lookup-registered-study
                        (study-meta-racket-id meta)))
                     (define temp-dir
                       (make-temporary-file (~a (study-meta-name meta) "~a") 'directory))
                     (define pdfs
                       (list*
                        (cons (study-meta-name meta)
                              (comptime-transitions->pdf (study-transitions the-study)))
                        (for/list ([sub (in-list (study-steps the-study))]
                                   #:when (step/study? sub))
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

(define study-instance-form
  (form* ([name (ensure binding/text (required))]
          [slug (ensure binding/text)]
          [enrollment-code (ensure binding/text)]
          [no-enrollment-code? (ensure binding/boolean)]
          [status (ensure binding/text (required) (one-of '(("active" . active)
                                                            ("inactive" . inactive)
                                                            ("archived" . archived))))])
    (list name
          (or slug (slugify name))
          (if no-enrollment-code? "" (or enrollment-code (generate-random-string 16)))
          status)))

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
       (match (form-run study-instance-form req #:defaults defaults)
         [(list 'passed (list name slug enrollment-code status) _)
          (define the-study-instance
            (with-database-connection [conn db]
              (insert-one! conn (make-study-instance
                                 #:study-id study-id
                                 #:name name
                                 #:slug slug
                                 #:enrollment-code enrollment-code
                                 #:status status))))

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
    (define the-instance (lookup-study-instance db study-instance-id))
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
         (match (form-run study-instance-form req #:defaults defaults)
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

(define/contract ((view-study-instance-page db) _req study-id study-instance-id)
  (-> database? (-> request? id/c id/c response?))
  (define the-instance (lookup-study-instance db study-instance-id))
  (unless the-instance
    (next-dispatcher))
  (define participants
    (list-study-instance-participants/admin db study-instance-id))
  (define vars
    (list-study-instance-vars db study-instance-id))
  (define bot-sets
    (list-bot-sets db study-instance-id))
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
                (response/jsexpr
                 (study-participants->jsexpr db study-id study-instance-id participants))))])
           "Export JSON"))
         (:h4
          (:a
           ([:href
             (embed/url
              (lambda (_req)
                (define total-payments (list-study-instance-total-payments/admin db study-instance-id))
                (response/output
                 #:headers (list (make-header #"content-disposition" #"attachment; filename=\"total-payments.csv\""))
                 (lambda (out)
                   (for ([p (in-list total-payments)])
                     (fprintf out
                              "~a,~a,~a~n"
                              (first p)
                              (cond [(prolific-username? (second p))
                                     (first (string-split (second p) "@"))]
                                    [else (second p)])
                              (~r
                               #:precision '(= 2)
                               (third p))))))))])
           "Export Total Payments CSV"))
         (:h4
          (:a
           ([:href
             (embed/url
              (lambda (_req)
                (define individual-payments (list-study-instance-payments/admin db study-instance-id))
                (response/output
                 #:headers (list (make-header #"content-disposition" #"attachment; filename=\"individual-payments.csv\""))
                 (lambda (out)
                   (for ([p (in-list individual-payments)])
                     (fprintf out
                              "~a,~a,~a~n"
                              (first p)
                              (second p)
                              (~r
                               #:precision '(= 2)
                               (third p))))))))])
           "Export Individual Payments CSV"))
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
   (:table.table
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
           (:td (:pre
                 (with-output-to-string
                   (lambda ()
                     (pretty-print (study-instance-var-value/deserialized v)))))))))))))

(define (render-participant-list study-id study-instance-id participants)
  (haml
   (:table.table
    (:thead
     (:tr
      (:th "Participant ID")
      (:th "Email")
      (:th "Enrolled At")
      (:th "Progress")
      (:th "Current Round")
      (:th "Current Group")))
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
           (:td (~a (study-participant/admin-progress p)))
           (:td (study-participant/admin-current-round-name p))
           (:td (study-participant/admin-current-group-name p)))))))))

;; TODO: Stop showing e-mail and show participant ID instead.
(define/contract ((view-study-participant-page auth db) _req study-id study-instance-id participant-id)
  (-> auth-manager? database? (-> request? id/c id/c id/c response?))
  (define the-study
    (lookup-study-meta db study-id))
  (define the-instance
    (lookup-study-instance db study-instance-id))
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
         (unless (eq? (study-participant/admin-role the-participant) 'admin)
           (haml
            (:h4
             (:a
              ([:href
                (embed/url
                 (lambda (_req)
                   (auth-manager-impersonate! auth (study-participant/admin-user-id the-participant))
                   (redirect/get/forget/protect)
                   (redirect-to (reverse-uri 'study-instances-page))))])
              "Impersonate User"))))
         (:table.table
          (:thead
           (:tr
            (:th "Payment Name")
            (:th "Payment Amount")))
          (:tbody
           ,@(for/list ([(name amount) (in-hash (lookup-payments db participant-id))])
               (haml
                (:tr
                 (:td name)
                 (:td (~r #:precision '(= 2) amount)))))))
         (:table.table
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
                 (:td (:pre (~a (study-var-stack v))))
                 (:td (study-var-round-name v))
                 (:td (study-var-group-name v))
                 (:td (~a (study-var-id v)))
                 (:td (~t (study-var-first-put-at v) datetime-format))
                 (:td (~t (study-var-last-put-at v) datetime-format))
                 (:td (:pre
                       (with-output-to-string
                         (lambda ()
                           (pretty-print (study-var-value/deserialized v))))))))))))))))))

(define/contract ((create-study-instance-bot-sets-page db) req study-id study-instance-id)
  (-> database? (-> request? id/c id/c response?))
  (define the-study
    (lookup-study-meta db study-id))
  (define the-instance
    (lookup-study-instance db study-instance-id))
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
    (lookup-study-instance db study-instance-id))
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
              (define runner
                (make-bot-runner db um the-study the-instance the-bot-set
                                 #:concurrency concurrency
                                 #:headless? headless?))
              (runner req)]

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

(define ((make-bot-runner db um the-study the-instance the-set
                          #:concurrency [concurrency 3]
                          #:headless? [headless? #t]) _req)
  (define-values (password users)
    (prepare-bot-set! db um the-set))
  (define study-racket-id
    (study-meta-racket-id the-study))
  (define bot-infos
    (get-bot-infos-for-study study-racket-id))
  (define bot-info
    (hash-ref bot-infos (bot-set-bot-id the-set)))
  (define bot (bot-info-bot bot-info))
  (define model
    (hash-ref (bot-info-models bot-info)
              (bot-set-model-id the-set)))
  (define sema
    (make-semaphore concurrency))
  (define chs
    (for/list ([u (in-list users)]
               [p (in-naturals 60100)])
      (define res-ch (make-channel))
      (begin0 res-ch
        (thread
         (lambda ()
           (define res
             (with-handlers ([exn:fail? values])
               (begin0 'ok
                 (call-with-semaphore sema
                   (lambda ()
                     ;; FIXME: Rename study-page to study-instance-page.
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
                      (bot model)))))))
           (channel-put res-ch res))))))
  (for ([ch (in-list chs)])
    (define res (channel-get ch))
    (when (exn:fail? res)
      (raise res)))
  (redirect-to
   (reverse-uri 'admin:view-study-instance-page
                (study-meta-id the-study)
                (study-instance-id the-instance))))


(define/contract ((stop-impersonation-page am) _req)
  (-> auth-manager? (-> request? response?))
  (when (impostor?)
    (auth-manager-stop-impersonation! am))
  (redirect-to (reverse-uri 'study-instances-page)))
