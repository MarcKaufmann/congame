#lang racket/base

(require actor
         component
         congame-web/components/user
         congame/components/bot
         (submod congame/components/bot actions)
         congame/components/study
         (submod congame/components/study private)
         data/monocle
         koyo/database
         koyo/url
         racket/contract/base
         racket/match
         racket/string
         web-server/http)

(provide
 (contract-out
  [bot-manager? (-> any/c boolean?)]
  [make-bot-manager (-> database? user-manager? bot-manager?)]
  [wrap-bot-manager (-> bot-manager? (-> procedure? (-> request? response?)))]
  [spawn-bot (-> bot? void?)]))

(struct bot-manager (db um impl)
  #:methods gen:component
  [(define (component-start bm)
     (match-define (bot-manager db um _) bm)
     (struct-copy bot-manager bm [impl (bot-manager-actor db um)]))
   (define (component-stop bm)
     (stop (bot-manager-impl bm))
     (struct-copy bot-manager bm [impl #f]))])

(define current-bot-manager
  (make-parameter #f))

(define (make-bot-manager db um)
  (bot-manager db um #f))

(define (((wrap-bot-manager bm) hdl) req)
  (parameterize ([current-bot-manager bm])
    (hdl req)))

(define (spawn-bot b)
  (match-define (study-manager p db)
    (current-study-manager))
  (define current-instance-id
    (study-participant-instance-id p))
  (define instance
    (lookup-study-instance db current-instance-id))
  (define instance-url
    (apply
     make-application-url
     (string-split
      (reverse-uri 'study-page (study-instance-slug instance))
      "/")))
  (match-define (bot-manager _ _ impl)
    (current-bot-manager))
  (run impl b current-instance-id instance-url))

(struct state (stopped? thds idx))
(define-struct-lenses state)

(define-actor (bot-manager-actor db um)
  #:state (state #f null 0)
  #:event (lambda (st)
            (apply
             choice-evt
             (for/list ([thd (in-list (state-thds st))])
               (handle-evt
                (thread-dead-evt thd)
                (lambda (_)
                  (lens-update &state-thds st (λ (thds) (remq thd thds))))))))
  #:stopped? state-stopped?
  (define (run st b instance-id instance-url)
    (define-values (u password)
      (user-manager-create-bot! um))
    (define bot-participant
      (enroll-participant! db (user-id u) instance-id))
    (define-values (next-st next-port)
      (state-next-port st))
    (define thd
      (thread
       (lambda ()
         (call-with-study-manager
          (make-study-manager
           #:database db
           #:participant bot-participant)
          (lambda ()
            (run-bot
             #:study-url instance-url
             #:username (user-username u)
             #:password password
             #:headless? #f
             #:port next-port
             b))))))
    (values
     (lens-update
      &state-thds
      next-st
      (λ (thds) (cons thd thds)))
     (void)))

  (define (stop st)
    (for-each break-thread (state-thds st))
    (values (&state-stopped? st #t) (void))))

(define (state-next-port st)
  (match-define (state _ _ idx) st)
  (define next-port (+ idx 61000))
  (define next-idx (modulo (add1 idx) 1000))
  (values (&state-idx st next-idx) next-port))
