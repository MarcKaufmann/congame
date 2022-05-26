#lang racket/base

(require component
         (only-in congame/components/study
                  make-study-meta
                  study-meta-id
                  make-study-instance
                  study-instance-id)
         congame-web/components/user
         congame-web/dynamic
         db
         deta
         koyo/database
         koyo/logging
         marionette
         (prefix-in http: net/http-easy)
         racket/format
         rackunit
         "common.rkt")

(define stop-logger void)
(define test-system
  (system-replace prod-system 'db make-test-database))

(define api-key #f)
(define empty-study-instance #f)

(define identity-tests
  (test-suite
   "identity"
   #:before
   (lambda ()
     (set! stop-logger
           (start-logger
            #:levels `((app                  . debug)
                       (mail-adapter         . debug)
                       (memory-session-store . debug)
                       (north-adapter        . debug)
                       (server               . debug)
                       (session              . debug)
                       (study                . debug)
                       (system               . debug)
                       (worker               . info))))
     (system-start test-system)

     (define db (system-ref test-system 'db))
     (with-database-connection [conn db]
       (query-exec conn "TRUNCATE users, study_participants, study_instances, studies CASCADE;"))

     (define users (system-ref test-system 'users))

     (define owner-user
       (make-test-user! users "user@example.com" "password"))
     (user-manager-verify! users (user-id owner-user) (user-verification-code owner-user))

     (define api-user
       (make-test-user! users "api@example.com" "password" #:roles #(api)))
     (user-manager-verify! users (user-id api-user) (user-verification-code api-user))
     (set! api-key (user-api-key api-user))

     (define the-instance
       (with-database-transaction [conn db]
         (define racket-id 'test-empty-study)
         (define name (symbol->string racket-id))
         (define slug name)
         (define the-study
           (insert-one! conn (make-study-meta
                              #:name name
                              #:slug slug
                              #:racket-id racket-id)))

         (insert-one! conn (make-study-instance
                            #:owner-id (user-id owner-user)
                            #:study-id (study-meta-id the-study)
                            #:name name
                            #:slug slug
                            #:status 'active))))
     (set! empty-study-instance the-instance))
   #:after
   (lambda ()
     (system-stop test-system))

   (test-case "enroll from identity"
     (define root "http://127.0.0.1:8000")
     (define res
       (http:post (~a root "/api/v1/study-participants-with-identity")
                  #:headers (hasheq 'authorization api-key)
                  #:json (hasheq
                          'instance-id (study-instance-id empty-study-instance)
                          'identity-email "enroll-test@identity"
                          'identity-domain "http://example.com"
                          'identity-key "enroll-test")))
     (check-equal? (http:response-status-code res) 200)

     (define data (http:response-json res))
     (define target-path (hash-ref data 'target-path))
     (call-with-marionette/browser/page!
      #:headless? #f
      (lambda (p)
        (page-goto! p (~a root target-path))
        (check-not-false (page-query-selector! p "#study-done")))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests identity-tests))
