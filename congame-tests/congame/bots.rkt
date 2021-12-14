#lang racket/base

(require component
         congame-pjb-studies/pjb-pilot-bot
         congame-web/components/user
         congame-web/dynamic
         (submod congame/components/bot actions)
         (except-in congame/components/study fail)
         db
         deta
         koyo/database
         koyo/logging
         rackunit
         "common.rkt")

(define stop-logger void)
(define test-system
  (system-replace prod-system 'db make-test-database))
(define bot-tests
  (test-suite
   "bots"
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
     (define bot-user
       (make-test-user! users "bot@example.com" "password" #:bot? #t))
     (user-manager-verify! users (user-id bot-user) (user-verification-code bot-user))
     (define pjb-pilot-instance
       (with-database-transaction [conn db]
         (define pjb-pilot-study
           (insert-one! conn (make-study-meta
                              #:name "pjb-pilot"
                              #:slug "pjb-pilot"
                              #:racket-id 'pjb-pilot-study)))

         (insert-one! conn (make-study-instance
                            #:study-id (study-meta-id pjb-pilot-study)
                            #:name "pjb-pilot"
                            #:slug "pjb-pilot"
                            #:status 'active))))
     (enroll-participant! db
                          (user-id bot-user)
                          (study-instance-id pjb-pilot-instance)))
   #:after
   (lambda ()
     (system-stop test-system))

   (test-suite
    "pjb-pilot-bot"

    (run-bot
     #:study-url "http://127.0.0.1:8000/study/pjb-pilot"
     #:username "bot@example.com"
     #:password "password"
     #:headless? #t
     (make-pjb-pilot-bot pjb-pilot-bot-model)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests bot-tests))
