#lang racket/base

(require component
         (prefix-in bot: (submod congame/components/bot actions))
         congame-pjb-studies/pjb-pilot-bot
         congame-web/components/user
         congame-web/dynamic
         (submod congame/components/bot actions)
         (except-in congame/components/study fail)
         db
         deta
         koyo/database
         koyo/logging
         marionette
         racket/match
         rackunit
         "common.rkt"
         "studies/test-substudy-failing.rkt")

(define stop-logger void)
(define test-system
  (system-replace prod-system 'db make-test-database))
(define stop-shared-marionette #f)
(define shared-browser #f)
(define bot-tests
  (test-suite
   "bots"
   #:before
   (lambda ()
     (set! stop-shared-marionette (start-marionette!))
     (set! shared-browser (browser-connect!))
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
     (define bot-user
       (make-test-user! users "bot@example.com" "password" #:bot? #t))
     (user-manager-verify! users (user-id bot-user) (user-verification-code bot-user))

     (define (add-study&instance&enroll! racket-id)
       (define the-instance
         (with-database-transaction [conn db]
           (define name (symbol->string racket-id))
           (define slug name)
           (define the-study
             (insert-one! conn (make-study-meta
                                #:name name
                                #:slug slug
                                #:racket-id racket-id)))

           (insert-one! conn (make-study-instance
                              #:owner-id (user-id bot-user)
                              #:study-id (study-meta-id the-study)
                              #:name name
                              #:slug slug
                              #:status 'active))))
       (enroll-participant! db
                            (user-id bot-user)
                            (study-instance-id the-instance)))

     (add-study&instance&enroll! 'pjb-pilot-study)
     (add-study&instance&enroll! 'test-substudy-failing))
   #:after
   (lambda ()
     (dynamic-wind
       void
       (λ () (system-stop test-system))
       (λ ()
         (browser-disconnect! shared-browser)
         (stop-shared-marionette))))

   (test-suite
    "pjb-pilot-bot"

    (run-bot
     #:study-url "http://127.0.0.1:8000/study/pjb-pilot-study"
     #:username "bot@example.com"
     #:password "password"
     #:browser shared-browser
     (make-pjb-pilot-bot pjb-pilot-bot-model)))

   (test-suite
    "test-substudy-failing"

    ;; For this test we want to run the bots up to the step that fails
    ;; and then stop and resume, then continue by failing.  This tests
    ;; a compositional issue with parameterization "inheritance" and
    ;; composable continuations.

    ;; run up to the failure step
    (run-bot
     #:study-url "http://127.0.0.1:8000/study/test-substudy-failing"
     #:username "bot@example.com"
     #:password "password"
     #:browser shared-browser
     (make-test-substudy-failing-bot
      (λ (id bot)
        (match id
          [`(*root* child1 fail1) (bot:completer)]
          [_ (bot)]))))

    ;; resume and run to the end
    (run-bot
     #:study-url "http://127.0.0.1:8000/study/test-substudy-failing"
     #:username "bot@example.com"
     #:password "password"
     #:browser shared-browser
     (make-test-substudy-failing-bot
      (λ (id bot)
        (match id
          [`(*root* done) (bot:completer)]
          [_ (bot)]))))

    (check-equal? (cdr (unbox test-substudy-failing-failed-step)) 'expected-failure))))

(module+ test
  (require rackunit/text-ui)
  (run-tests bot-tests))
