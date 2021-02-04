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
         racket/format
         racket/generic
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
         "bot.rkt"
         (prefix-in bot: (submod "bot.rkt" actions))
         "export.rkt"
         "registry.rkt")

(provide
 current-git-sha
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
 run-study
 participant-email
 current-participant-id
 ; current-step
 amount/c
 get-payment
 put-payment!
 get-all-payments
 )

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

(define/contract current-git-sha
  (parameter/c (or/c #f string?))
  (make-parameter #f))

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
                (current-git-sha))))

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

; FIXME: Using exact naturals would be even better, but then
; we need to ensure that the payments are included in cents, rather
; than whole dollars. This will almost surely lead to errors of rounding
; or misunderstanding.
(define amount/c
  (and/c number? (or/c positive? zero?)))

; FIXME: The other schemas are later, but for the contract I need `study-payment?`

(define-schema study-payment
  #:table "payments"
  ([participant-id integer/f]
   [(timestamp (now/moment)) datetime-tz/f]
   [payment-name string/f]
   [payment (numeric/f 6 2)]))

(define/contract (put-payment! k payment)
  (-> symbol? amount/c study-payment?)
  (with-database-connection [conn (current-database)]
    (insert-one! conn (make-study-payment #:participant-id (current-participant-id)
                                          #:payment-name (symbol->string k)
                                          #:payment payment))))

(define (get-payment k)
  (define result
    (with-database-connection [conn (current-database)]
      (lookup conn (~> (from study-payment #:as p)
                       (where (and (= ,(current-participant-id) p.participant-id)
                                   (= ,(symbol->string k) p.payment-name)))))))
  (cond [result => study-payment-payment]
        [else
         (error "no payment ~a found for participant ~a" k (current-participant-id))]))

(define (get-all-payments)
  (with-database-connection [conn (current-database)]
    (for/hash ([p (in-entities conn
                               (~> (from study-payment #:as p)
                                   (where (= ,(current-participant-id) p.participant-id))))])
      (values (string->symbol (study-payment-payment-name p))
              (study-payment-payment p)))))

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

(define/widget (button action label #:id [id ""])
  (haml
   (:a.button.next-button
    ([:data-widget-id (when-bot id)]
     [:href
      (embed
       (lambda (_req)
         (action)
         (continue)))])
    label)))

(define/widget (form f action render #:id [id ""])
  (match (form-run f this-request)
    [(list 'passed res _)
     (redirect/get/forget/protect)
     (action res)
     (continue)]

    [(list _ _ rw)
     (haml
      (:form
       ([:action (embed
                  (lambda (_req)
                    (response/step this-step)))]
        [:data-widget-id (when-bot id)]
        [:method "POST"])
       (render rw)))]))

(define/widget (skip [to-step-id #f])
  (if to-step-id
      (continue to-step-id)
      (continue)))


;; step ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ private
  (provide
   (struct-out step)
   (struct-out step/study)
   (struct-out study)))

(struct step (id handler handler/bot transition)
  #:transparent)

(struct step/study step (study)
  #:transparent)

(struct study (requires provides steps)
  #:transparent)

(define step-id/c symbol?)
(define handler/c (-> xexpr?))
(define transition/c (-> (or/c done? next? step-id/c)))
(define binding/c (list/c symbol? symbol?))

(define (default-transition)
  next)

(define/contract (make-step id
                            handler
                            [transition default-transition]
                            #:for-bot [handler/bot
                                       (lambda ()
                                         (if (eq? transition default-transition)
                                             (bot:continuer)
                                             (raise-user-error "no bot transition for step" id)))])
  (->* (step-id/c handler/c)
       (transition/c
        #:for-bot procedure?)
       step?)
  (step id handler handler/bot transition))

(define/contract (make-step/study id s
                                  [transition (lambda () next)]
                                  #:require-bindings [require-bindings null]
                                  #:provide-bindings [provide-bindings null])
  (->* (step-id/c study?)
       (transition/c
        #:require-bindings (listof binding/c)
        #:provide-bindings (listof binding/c))
       step?)
  (step/study
   id
   (lambda ()
     (define all-required-bindings
       (for/fold ([seen (for/hasheq ([binding (in-list require-bindings)])
                          (values (car binding) #t))]
                  [bindings require-bindings]
                  #:result bindings)
                 ([required-id (in-list (study-requires s))]
                  #:unless (hash-has-key? seen required-id))
         (values (hash-set seen required-id #t)
                 (cons (list required-id required-id) bindings))))
     (define bindings
       (for/hasheq ([binding (in-list all-required-bindings)])
         (match-define (list dst-id src-id) binding)
         (values dst-id (get src-id (lambda ()
                                      (error 'get "value not found for key ~.s in sub-study~n  bound to: ~s~n  required by step: ~.s" src-id dst-id id))))))
     (with-widget-parameterization
       (define result-bindings
         (run-study s #:bindings bindings))
       (for ([binding (in-list provide-bindings)])
         (match-define (list dst-id src-id) binding)
         (put dst-id (hash-ref result-bindings src-id (lambda ()
                                                        (error 'run-study/step "expected binding ~s to be provided by sub-study~n  bound to: ~s~n  at step: ~.s" src-id dst-id id)))))
       (continue)))
   void
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

(define (response/step s)
  (response/xexpr
   (haml
    (:div.step
     ([:data-study-stack (when-bot (call-with-output-string
                                    (lambda (out)
                                      (write (current-study-stack) out))))]
      [:data-step-id (when-bot (step-id s))])
     ((step-handler s))))))

(define-syntax-rule (when-bot e)
  (if (current-user-bot?) (~a e) ""))


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

(define/contract (run-study s
                            [req (current-request)]
                            #:bindings [bindings (hasheq)])
  (->* (study?)
       (request?
        #:bindings (hash/c symbol? any/c))
       any)
  (define resume-stack (current-resume-stack))
  (define new-study-stack
    (if (eq? (current-step) 'no-step)
        (list '*root*)
        (cons (step-id (current-step)) (current-study-stack))))
  (parameterize ([current-study-stack new-study-stack])
    (cond
      ;; An empty resume stack means that this is the first time we've
      ;; entered this [sub]study.
      [(null? resume-stack)
       ;; Only put bindings on the first step of the [sub]study, to
       ;; avoid overwriting bindings that may have been set on
       ;; subsequent steps on resume.
       (for ([(id value) (in-hash bindings)])
         (put id value))
       (begin0 (run-step req s (study-next-step s))
         (redirect/get/forget/protect))]

      [else
       (define the-step
         (study-find-step s (car resume-stack) raise-resume-error))
       (parameterize ([current-resume-stack (cdr resume-stack)])
         (run-step req s the-step))])))

(define (raise-resume-error)
  (define resume-stack
    (current-participant-progress (current-study-manager)))
  (error 'run-study "failed to resume step in study~n  resume stack: ~.s" resume-stack))

(define (run-step req s the-step)
  (log-study-debug "running step: ~.s" the-step)
  (update-participant-progress! (step-id the-step))
  (define res
    (call-with-current-continuation
     (lambda (return)
       (send/suspend/dispatch/protect
        (lambda (embed/url)
          ;; These parameterizations are closed-over and re-set by
          ;; `embed` within handlers when necessary because `call/cc`
          ;; above captures everything outside up to the
          ;; `servlet-prompt' (in our case up to
          ;; `wrap-protect-continuations').
          (parameterize ([current-embed/url embed/url]
                         [current-request req]
                         [current-return return]
                         [current-step the-step])
            (response/step the-step)))))
     servlet-prompt))

  (log-study-debug "step ~.s returned ~.s" the-step res)
  (match res
    [(? response?)
     (send/back res)]

    [(cons 'to-step to-step-id)
     (define new-req (redirect/get/forget/protect))
     (define next-step
       (study-find-step s to-step-id (lambda ()
                                       (error 'run-step "skipped to a nonexistent step: ~s~n  current step: ~.s~n  current study: ~.s" to-step-id the-step s))))
     (run-step new-req s next-step)]

    [_
     (define new-req (redirect/get/forget/protect))
     (define next-step
       (match ((step-transition the-step))
         [(? done?) #f]
         [(? next?) (study-find-next-step s (step-id the-step))]
         [next-step-id (study-find-step s next-step-id (lambda ()
                                                         (error 'run-step "transitioned to a nonexistent step: ~.s~n  current step: ~.s~n  current study: ~.s" next-step-id (step-id the-step) s)))]))

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

(define (study-find-step s id [failure-thunk (lambda () #f)])
  (define st
    (for/first ([a-step (in-list (study-steps s))]
                #:when (eq? (step-id a-step) id))
      a-step))
  (or st (failure-thunk)))


;; db ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (schema-out study-meta)
 (schema-out study-instance)
 (schema-out study-participant)
 (schema-out study-participant/admin)
 (schema-out study-var)
 (schema-out study-payment)
 study-var-value/deserialized
 make-study-manager
 call-with-study-manager
 list-studies
 list-study-instances
 list-all-study-instances
 list-active-study-instances
 list-study-instance-participants/admin
 clear-participant-progress!
 enroll-participant!
 mark-participant-completed!
 lookup-study
 lookup-study-meta
 lookup-study-instance
 lookup-study-participant/admin
 lookup-study-vars)

(define-schema study-meta
  #:table "studies"
  ([id integer/f #:primary-key #:auto-increment]
   [name string/f #:contract non-empty-string?]
   [slug string/f #:contract non-empty-string?]
   [racket-id symbol/f]
   [(created-at (now/moment)) datetime-tz/f]))

(define-schema study-instance
  #:table "study_instances"
  ([id integer/f #:primary-key #:auto-increment]
   [study-id integer/f]
   [name string/f #:contract non-empty-string?]
   [slug string/f #:contract non-empty-string?]
   [(status 'active) symbol/f #:contract (or/c 'active 'inactive 'archived)]
   [(created-at (now/moment)) datetime-tz/f]))

(define-schema study-participant
  #:table "study_participants"
  ([id integer/f #:primary-key #:auto-increment]
   [user-id integer/f]
   [instance-id integer/f]
   [(progress #()) (array/f string/f)]
   [(completed? #f) boolean/f]
   [(enrolled-at (now/moment)) datetime-tz/f]))

(define-schema study-participant/admin
  #:virtual
  ([id integer/f]
   [email string/f]
   [progress (array/f string/f)]
   [completed? boolean/f]
   [enrolled-at datetime-tz/f]))

(define-schema study-var
  #:virtual
  ([stack (array/f string/f)]
   [id symbol/f]
   [value binary/f]
   [first-put-at datetime-tz/f]
   [last-put-at datetime-tz/f])
  #:methods gen:jsexprable
  [(define/generic ->jsexpr/super ->jsexpr)
   (define (->jsexpr v)
     (match-define (study-var _ stack id _ first-put-at last-put-at) v)
     (hash 'stack (vector->list stack)
           'id (symbol->string id)
           'value (->jsexpr/super (study-var-value/deserialized v))
           'first-put-at (moment->iso8601 first-put-at)
           'last-put-at (moment->iso8601 last-put-at)))])

(define study-var-value/deserialized
  (compose1 deserialize* study-var-value))

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

(define/contract (list-active-study-instances db)
  (-> database? (listof study-instance?))
  (with-database-connection [conn db]
    (sequence->list
     (in-entities conn (~> (from study-instance #:as i)
                           (where (= i.status "active"))
                           (order-by ([i.created-at #:desc])))))))

(define/contract (list-study-instance-participants/admin db instance-id)
  (-> database? id/c (listof study-participant/admin?))
  (with-database-connection [conn db]
    (sequence->list
     (in-entities conn (~> (from study-participant #:as p)
                           (join user #:as u #:on (= u.id p.user-id))
                           (where (= p.instance-id ,instance-id))
                           (order-by ([p.enrolled-at #:desc]))
                           (select p.id u.username p.progress p.completed? p.enrolled-at)
                           (project-onto study-participant/admin-schema))))))

(define/contract (enroll-participant! db user-id instance-id)
  (-> database? id/c id/c study-participant?)
  (with-database-transaction [conn db]
    (cond
      [(lookup conn
               (~> (from study-instance #:as i)
                   (where (and
                           (= i.id ,instance-id)
                           (= i.status "active")))))
       => (lambda (_instance)
            (define maybe-participant
              (lookup conn
                      (~> (from study-participant #:as p)
                          (where (and (= p.user-id ,user-id)
                                      (= p.instance-id ,instance-id))))))

            (or maybe-participant
                (insert-one! conn
                             (make-study-participant
                              #:user-id user-id
                              #:instance-id instance-id))))]

      [else #f])))

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
                  (lookup-registered-study
                   (study-meta-racket-id meta)
                   (lambda (id)
                     (error 'lookup-registered-study "No such registered study: ~s~n. Did you install the necessary congame-studies?" id)))
                  participant)))]

      [else #f])))

(define/contract (lookup-study-meta db study-id)
  (-> database? id/c (or/c #f study-meta?))
  (with-database-connection [conn db]
    (lookup conn (~> (from study-meta #:as s)
                     (where (= s.id ,study-id))))))

(define/contract (lookup-study-instance db instance-id)
  (-> database? id/c (or/c #f study-instance?))
  (with-database-connection [conn db]
    (lookup conn (~> (from study-instance #:as i)
                     (where (= i.id ,instance-id))))))

(define/contract (lookup-study-participant/admin db participant-id)
  (-> database? id/c (or/c #f study-participant/admin?))
  (with-database-connection [conn db]
    (lookup conn (~> (from study-participant #:as p)
                     (join user #:as u #:on (= u.id p.user-id))
                     (where (= p.id ,participant-id))
                     (select p.id u.username p.progress p.completed? p.enrolled-at)
                     (project-onto study-participant/admin-schema)))))

(define/contract (participant-email pid)
  (-> id/c string?)
  (study-participant/admin-email
    (lookup-study-participant/admin
     (study-manager-db (current-study-manager))
     pid)))

(define/contract (lookup-study-vars db participant-id)
  (-> database? id/c (listof study-var?))
  (with-database-connection [conn db]
    (sequence->list
     (in-entities conn (~> (from "study_data" #:as d)
                           (select d.study-stack d.key d.value d.first-put-at d.last-put-at)
                           (project-onto study-var-schema)
                           (where (= d.participant-id ,participant-id))
                           (order-by ([d.first-put-at #:asc])))))))

(define/contract (clear-participant-progress! db participant-id)
  (-> database? id/c void?)
  (with-database-transaction [conn db]
    (query-exec conn (~> (from "study_participants" #:as p)
                         (where (= p.id ,participant-id))
                         (update
                          [is_completed #f]
                          [progress ,(list->pg-array null)])))
    (query-exec conn (~> (from "study_data" #:as d)
                         (where (= d.participant-id ,participant-id))
                         (delete)))
    (query-exec conn (~> (from "payments" #:as p)
                         (where (= p.participant_id ,participant-id))
                         (delete)))))

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
