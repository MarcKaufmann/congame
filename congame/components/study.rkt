#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/lib/function-header)
         db
         db/util/postgresql
         deta
         gregor
         koyo/continuation
         koyo/database
         koyo/haml
         (except-in forms form)
         racket/contract
         racket/fasl
         racket/match
         racket/port
         racket/sequence
         racket/serialize
         racket/string
         racket/stxparam
         syntax/parse/define
         threading
         web-server/servlet
         (only-in xml xexpr?)
         "git.rkt")

(provide
 next
 done
 put
 get
 button
 form
 skip
 make-step
 make-step/study
 wrap-sub-study
 make-study
 run-study)

;; TODO:
;; * admin: add studies, instances, visualize, reset users' data

;; canaries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values (next next?)
  (let ()
    (struct next () #:transparent)
    (values (next) next?)))

(define-values (done done?)
  (let ()
    (struct done () #:transparent)
    (values (done) done?)))


;; storage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (deserialize* s)
  (deserialize (fasl->s-exp (open-input-bytes s))))

(define (serialize* v)
  (call-with-output-bytes
   (lambda (out)
     (s-exp->fasl (serialize v) out))))

(define (put k v)
  (log-study-debug "PUT~n  stack: ~s~n  key: ~s~n  value: ~s" (current-study-ids) k v)
  (with-database-connection [conn (current-database)]
    (query-exec conn #<<QUERY
INSERT INTO study_data (
  participant_id, study_stack, key, value, git_sha
) VALUES (
  $1, $2, $3, $4, $5
) ON CONFLICT (participant_id, study_stack, key) DO UPDATE SET
  value = EXCLUDED.value,
  git_sha = EXCLUDED.git_sha,
  last_put_at = CURRENT_TIMESTAMP
QUERY
                (current-participant-id)
                (current-study-array)
                (symbol->string k)
                (serialize* v)
                current-git-sha)))

(define (get k [default (lambda ()
                          (error 'get "value not found for key ~.s" k))])
  (log-study-debug "GET~n  stack: ~s~n  key: ~s" (current-study-ids) k)
  (with-database-connection [conn (current-database)]
    (define maybe-value
      (query-maybe-value conn (~> (from "study_data" #:as d)
                                  (select d.value)
                                  (where (and
                                          (= d.participant-id ,(current-participant-id))
                                          (= d.study-stack ,(current-study-array))
                                          (= d.key ,(symbol->string k)))))))

    (cond
      [maybe-value => deserialize*]
      [(procedure? default) (default)]
      [else default])))

(define (current-study-ids)
  (reverse (current-study-stack)))

(define (current-study-array)
  (list->pg-array (map symbol->string (current-study-ids))))

;; widgets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-embed/url
  (make-parameter 'no-embed/url))

(define current-request
  (make-parameter 'no-request))

(define current-return
  (make-parameter 'no-return))

(define-syntax-rule (define-widget-stxparams id ...)
  (begin
    (define-syntax-parameter id
      (lambda (stx)
        (raise-syntax-error #f "can only be used inside with-widget-parameterization" stx)))
    ...))

(define-widget-stxparams
  embed
  this-request
  this-step
  continue)

(define-syntax-rule (with-widget-parameterization e ...)
  ;; Capture return here so that any embedded (via embed/url) lambda can close over it.
  (let ([embed/url (current-embed/url)]
        [the-request (current-request)]
        [the-step (current-step)]
        [return (current-return)])
    (syntax-parameterize ([embed
                           (syntax-parser
                             [(_ f:expr)
                              #'(embed/url
                                 (lambda (req)
                                   (parameterize ([current-embed/url embed/url]
                                                  [current-request req]
                                                  [current-return return]
                                                  [current-step the-step])
                                     (f req))))])]

                          [continue
                           (syntax-parser
                             [(_)
                              #'(return 'continue)]

                             [(_ to:id)
                              #'(return (cons 'to-step to))])]

                          [this-request
                           (lambda (stx)
                             (syntax/loc stx the-request))]

                          [this-step
                           (lambda (stx)
                             (syntax/loc stx the-step))])
      e ...)))

(define-syntax-parser define/widget
  [(_ head:function-header body ...+)
   #'(define head
       (with-widget-parameterization
         body ...))])

(define/widget (button action label)
  (haml
   (:a
    ([:href
      (embed
       (lambda (_req)
         (action)
         (continue)))])
    label)))

(define/widget (form f action render)
  (match (form-run f this-request)
    [(list 'passed res _)
     (action res)
     (continue)]

    [(list _ _ rw)
     (haml
      (:form
       ([:action (embed
                  (lambda (_req)
                    (response/xexpr
                     ((step-handler this-step)))))]
        [:method "POST"])
       (render rw)))]))

(define/widget (skip [to-step-id #f])
  (if to-step-id
      (continue to-step-id)
      (continue)))


;; step ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct step (id handler transition)
  #:transparent)

(struct step/study step (study)
  #:transparent)

(struct study (requires provides steps)
  #:transparent)

(define step-id/c symbol?)
(define handler/c (-> xexpr?))
(define transition/c (-> (or/c done? next? step-id/c)))

(define/contract (make-step id handler [transition (lambda () next)])
  (->* (step-id/c handler/c) (transition/c) step?)
  (step id handler transition))

(define/contract (make-step/study id s [transition (lambda () next)])
  (->* (step-id/c study?) (transition/c) step?)
  (step/study
   id
   (lambda ()
     (with-widget-parameterization
       (run-study s)
       (continue)))
   transition
   s))

(define/contract (wrap-sub-study s wrapper)
  (-> study? (-> handler/c handler/c) study?)
  (struct-copy study s [steps (for/list ([a-step (in-list (study-steps s))])
                                (cond
                                  [(step/study? a-step)
                                   (make-step/study
                                    (step-id a-step)
                                    (wrap-sub-study (step/study-study a-step) wrapper)
                                    (step-transition a-step))]

                                  [else
                                   (make-step
                                    (step-id a-step)
                                    (wrapper (step-handler a-step))
                                    (step-transition a-step))]))]))


;; study ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-logger study)

;; This is actually a stack of step ids where each step represents a [sub]study.
(define current-study-stack
  (make-parameter null))

(define current-step
  (make-parameter 'no-step))

(define current-resume-stack
  (make-parameter null))

(define/contract (make-study steps
                             #:requires [requires null]
                             #:provides [provides null])
  (->* ((non-empty-listof step?))
       (#:requires (listof symbol?)
        #:provides (listof symbol?))
       study?)
  (study requires provides steps))

(define/contract (run-study s [req (current-request)])
  (->* (study?) (request?) any)
  (define resume-stack (current-resume-stack))
  (define new-study-stack
    (if (eq? (current-step) 'no-step)
        (list '*root*)
        (cons (step-id (current-step)) (current-study-stack))))
  (parameterize ([current-study-stack new-study-stack])
    (cond
      [(null? resume-stack)
       (begin0 (run-step req s (study-next-step s))
         (redirect/get/forget/protect))]

      [else
       (define the-step (study-find-step s (car resume-stack)))
       (unless the-step (raise-resume-error))
       (parameterize ([current-resume-stack (cdr resume-stack)])
         (run-step req s the-step))])))

(define (raise-resume-error)
  (define-values (resume-stack resume-step)
    (current-participant-progress (current-study-manager)))
  (error 'run-study "failed to resume step in study~n  resume step: ~.s~n  resume stack: ~.s" resume-step resume-stack))

(define (run-step req s the-step)
  (log-study-debug "running step: ~.s" the-step)
  (update-participant-progress! (step-id the-step))
  (define res
    (call-with-current-continuation
     (lambda (return)
       (send/suspend/dispatch/protect
        (lambda (embed/url)
          (parameterize ([current-embed/url embed/url]
                         [current-request req]
                         [current-return return]
                         [current-step the-step])
            (response/xexpr ((step-handler the-step)))))))
     servlet-prompt))

  (log-study-debug "step ~.s returned ~.s" the-step res)
  (match res
    [(? response?)
     (send/back res)]

    [(cons 'to-step to-step-id)
     (define new-req (redirect/get/forget/protect))
     (define next-step (study-find-step s to-step-id))
     (unless next-step
       (error 'run-step "skipped to a nonexistent step: ~s~n  current step: ~.s~n  current study: ~.s" to-step-id the-step s))
     (run-step new-req s next-step)]

    [_
     (define new-req (redirect/get/forget/protect))
     (define next-step
       (match ((step-transition the-step))
         [(? done?) #f]
         [(? next?) (study-find-next-step s (step-id the-step))]
         [next-step-id (study-find-step s next-step-id)]))

     (cond
       [next-step => (lambda (the-next-step)
                       (run-step new-req s the-next-step))]
       [else
        (for/hasheq ([id (in-list (study-provides s))])
          (values id (get id (lambda ()
                               (error 'run-study "study did not 'put' provided variable: ~s" id)))))])]))

(define (study-next-step s)
  (car (study-steps s)))

(define (study-find-next-step s id)
  (for/fold ([previous #f]
             [next-step #f]
             #:result next-step)
            ([a-step (in-list (study-steps s))]
             #:unless next-step)
    (if (and previous (eq? (step-id previous) id))
        (values previous a-step)
        (values a-step #f))))

(define (study-find-step s id)
  (for/first ([a-step (in-list (study-steps s))]
              #:when (eq? (step-id a-step) id))
    a-step))


;; db ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (schema-out study-meta)
 (schema-out study-instance)
 (schema-out study-participant)
 make-study-manager
 call-with-study-manager
 list-studies
 list-study-instances
 list-all-study-instances
 enroll-participant!
 mark-participant-completed!
 lookup-study)

(define-schema study-meta
  #:table "studies"
  ([id integer/f #:primary-key #:auto-increment]
   [name string/f #:contract non-empty-string?]
   [slug string/f #:contract non-empty-string?]
   [racket-module symbol/f]
   [racket-id symbol/f]
   [(created-at (now/moment)) datetime-tz/f]))

(define-schema study-instance
  #:table "study_instances"
  ([id integer/f #:primary-key #:auto-increment]
   [study-id integer/f]
   [name string/f #:contract non-empty-string?]
   [slug string/f #:contract non-empty-string?]
   [(created-at (now/moment)) datetime-tz/f]))

(define-schema study-participant
  #:table "study_participants"
  ([id integer/f #:primary-key #:auto-increment]
   [user-id integer/f]
   [instance-id integer/f]
   [(progress #()) (array/f string/f)]
   [(completed? #f) boolean/f]
   [(enrolled-at (now/moment)) datetime-tz/f]))

(struct study-manager (participant db)
  #:transparent)

(define/contract (make-study-manager #:database db
                                     #:participant participant)
  (->* (#:database database?
        #:participant study-participant?)
       ()
       study-manager?)
  (study-manager participant db))

(define/contract current-study-manager
  (parameter/c (or/c #f study-manager?))
  (make-parameter #f))

(define current-database
  (compose1 study-manager-db current-study-manager))

(define current-participant-id
  (compose1 study-participant-id study-manager-participant current-study-manager))

(define/contract (list-studies db)
  (-> database? (listof study-meta?))
  (with-database-connection [conn db]
    (sequence->list
     (in-entities conn (~> (from study-meta #:as s)
                           (order-by ([s.created-at #:desc])))))))

(define/contract (list-study-instances db study-id)
  (-> database? id/c (listof study-instance?))
  (with-database-connection [conn db]
    (sequence->list
     (in-entities conn (~> (from study-instance #:as i)
                           (where (= i.study-id ,study-id))
                           (order-by ([i.created-at #:desc])))))))

(define/contract (list-all-study-instances db)
  (-> database? (listof study-instance?))
  (with-database-connection [conn db]
    (sequence->list
     (in-entities conn (~> (from study-instance #:as i)
                           (order-by ([i.created-at #:desc])))))))

(define/contract (enroll-participant! db user-id instance-id)
  (-> database? id/c id/c study-participant?)
  (with-database-transaction [conn db]
    #:isolation 'serializable
    (define maybe-participant
      (lookup conn
              (~> (from study-participant #:as p)
                  (where (and (= p.user-id ,user-id)
                              (= p.instance-id ,instance-id))))))

    (or maybe-participant
        (insert-one! conn
                     (make-study-participant
                      #:user-id user-id
                      #:instance-id instance-id)))))

(define/contract (lookup-study db slug user-id)
  (-> database? string? id/c (or/c false/c (list/c study? study-participant?)))
  (with-database-transaction [conn db]
    (cond
      [(lookup conn
               (~> (from study-instance #:as i)
                   (where (= i.slug ,slug))))
       => (lambda (i)
            (define meta
              (lookup conn
                      (~> (from study-meta #:as m)
                          (where (= m.id ,(study-instance-study-id i))))))

            (define participant
              (lookup conn
                      (~> (from study-participant #:as p)
                          (where (and (= p.user-id ,user-id)
                                      (= p.instance-id ,(study-instance-id i)))))))

            (and participant
                 (list
                  (dynamic-require
                   (study-meta-racket-module meta)
                   (study-meta-racket-id meta))
                  participant)))]

      [else #f])))

(define (update-participant-progress! step-id)
  (define m (current-study-manager))
  (define p (study-manager-participant m))
  (with-database-connection [conn (study-manager-db m)]
    (define progress
      (list->vector
       (append
        (map symbol->string (current-study-ids))
        (list (symbol->string step-id)))))
    (update! conn (set-study-participant-progress p progress))))

(define (mark-participant-completed! m)
  (define p (study-manager-participant m))
  (with-database-connection [conn (study-manager-db m)]
    (update! conn (set-study-participant-completed? p #t))))

(define (current-participant-progress m)
  (define p (study-manager-participant m))
  (for*/list ([id:str (in-vector (study-participant-progress p))]
              [id (in-value (string->symbol id:str))]
              #:unless (eq? id '*root*))
    id))

(define/contract (call-with-study-manager mgr f)
  (-> study-manager? (-> any) any)
  (parameterize ([current-study-manager mgr]
                 [current-resume-stack (current-participant-progress mgr)])
    (f)))
