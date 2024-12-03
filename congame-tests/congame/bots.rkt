#lang racket/base

(require component
         (only-in congame/components/study make-study-manager)
         (only-in (submod congame/components/study private) current-study-manager)
         (prefix-in bot: (submod congame/components/bot actions))
         congame-example-study/prisoners-dilemma
         congame-pjb-studies/pjb-pilot-bot
         congame-web/components/user
         congame-web/dynamic
         (submod congame/components/bot actions)
         (except-in congame/components/study fail)
         data/monocle
         db
         deta
         koyo/database
         koyo/logging
         marionette
         racket/match
         racket/promise
         rackunit
         threading
         "common.rkt"
         "studies/test-looping-failures.rkt"
         "studies/test-substudy-failing.rkt")

;; Adding new tests:
;;   * write the study under `studies/`
;;   * add the study to the instance list in `../info.rkt`
;;   * recompile everything/bust the study cache

(define stop-logger void)
(define test-system
  (system-replace prod-system 'db make-test-database))
(define stop-shared-marionette #f)
(define shared-browser #f)
(define participants (make-hasheq)) ; racket-id -> (bot-id -> participant)
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
     (define admin-user (make-test-user! users "admin@example.com" "password" #:roles #(admin)))
     (user-manager-verify! users (user-id admin-user) (user-verification-code admin-user))
     (define bot-user-1 (make-test-user! users "bot1@example.com" "password" #:roles #(bot)))
     (user-manager-verify! users (user-id bot-user-1) (user-verification-code bot-user-1))
     (define bot-user-2 (make-test-user! users "bot2@example.com" "password" #:roles #(bot)))
     (user-manager-verify! users (user-id bot-user-2) (user-verification-code bot-user-2))

     (define (add-study&instance&enroll! racket-id)
       (define the-instance
         (with-database-transaction [conn db]
           (define name (symbol->string racket-id))
           (define slug name)
           (define the-study
             (~>
              (make-study-meta
               #:owner-id (user-id admin-user)
               #:name name
               #:slug slug
               #:racket-id racket-id)
              (insert-one! conn _)))
           (~>
            (make-study-instance
             #:owner-id (user-id admin-user)
             #:study-id (study-meta-id the-study)
             #:name name
             #:slug slug
             #:status 'active)
            (insert-one! conn _))))
       (define participant-1
         (enroll-participant! db (user-id bot-user-1) (study-instance-id the-instance)))
       (define participant-2
         (enroll-participant! db (user-id bot-user-2) (study-instance-id the-instance)))
       (for ([(p idx) (in-indexed (in-list (list participant-1 participant-2)))])
         (define bot-id (string->symbol (format "bot-~a" (add1 idx))))
         (hash-update!
          participants racket-id
          (λ (ht)
            (begin0 ht
              (hash-set! ht bot-id p)))
          make-hasheq)))

     (add-study&instance&enroll! 'pjb-pilot-study)
     (add-study&instance&enroll! 'prisoners-dilemma)
     (add-study&instance&enroll! 'test-looping-failures)
     (add-study&instance&enroll! 'test-substudy-failing))
   #:after
   (lambda ()
     (dynamic-wind
       void
       (lambda ()
         (system-stop test-system))
       (lambda ()
         (browser-disconnect! shared-browser)
         (stop-shared-marionette))))

   #;
   (test-suite
    "pjb-pilot-bot"

    (run-bot
     #:study-url "http://127.0.0.1:8000/study/pjb-pilot-study"
     #:username "bot1@example.com"
     #:password "password"
     #:browser shared-browser
     (make-pjb-pilot-bot pjb-pilot-bot-model/full)))

   #;
   (test-suite
    "test-substudy-failing"

    ;; For this test we want to run the bots up to the step that fails
    ;; and then stop and resume, then continue by failing.  This tests
    ;; a compositional issue with parameterization "inheritance" and
    ;; composable continuations.

    ;; run up to the failure step
    (run-bot
     #:study-url "http://127.0.0.1:8000/study/test-substudy-failing"
     #:username "bot1@example.com"
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
     #:username "bot1@example.com"
     #:password "password"
     #:browser shared-browser
     (make-test-substudy-failing-bot
      (λ (id bot)
        (match id
          [`(*root* done) (bot:completer)]
          [_ (bot)]))))

    (check-equal? (cdr (unbox test-substudy-failing-failed-step)) 'expected-failure))

   #;
   (test-suite
    "test-looping-failures"

    (run-bot
     #:study-url "http://127.0.0.1:8000/study/test-looping-failures"
     #:username "bot1@example.com"
     #:password "password"
     #:browser shared-browser
     (make-test-looping-failures-bot
      (λ (id bot)
        (match id
          [`(*root* done) (bot:completer)]
          [_ (bot)])))))

   (test-suite
    "test-prioners-dilemma"

    (test-case "happy path"
      (define bot-1-manager
        (make-study-manager
         #:database (system-ref test-system 'db)
         #:participant ((&hash-ref* 'prisoners-dilemma 'bot-1) participants)))

      (define bot-1-promise
        (delay/thread
         (parameterize ([current-study-manager bot-1-manager])
           (run-bot
            #:study-url "http://127.0.0.1:8000/study/prisoners-dilemma"
            #:username "bot1@example.com"
            #:password "password"
            #:port 2829
            (make-prisoners-dilemma-bot prisoners-dilemma-model)))))

      (define bot-2-manager
        (make-study-manager
         #:database (system-ref test-system 'db)
         #:participant ((&hash-ref* 'prisoners-dilemma 'bot-2) participants)))

      (define bot-2-promise
        (delay/thread
         (parameterize ([current-study-manager bot-2-manager])
           (run-bot
            #:study-url "http://127.0.0.1:8000/study/prisoners-dilemma"
            #:username "bot2@example.com"
            #:password "password"
            #:port 2830
            (make-prisoners-dilemma-bot prisoners-dilemma-model)))))

      (for-each sync (list bot-1-promise bot-2-promise))

      ;; Test that defvar* puts the value under a custom root. Somewhat
      ;; of a flimsy test since it could fail if we change the "unique
      ;; id" for the "behavior" binding in prisoners-dilemma.
      (parameterize ([current-study-manager bot-1-manager])
        (check-not-false
         (parameterize ([current-study-stack '(*root*)])
           (get #:root '*dynamic:xyz.trichotomy.congame.prisoners-dilemma:bot-behavior* 'bot-behavior #f))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests bot-tests))
