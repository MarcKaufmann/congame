#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/lib/function-header)
         db
         db/util/postgresql
         deta
         gregor
         koyo/continuation
         koyo/database
         koyo/haml
         koyo/random
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
         sentry
         syntax/parse/define
         threading
         web-server/servlet
         (only-in xml xexpr?)
         "bot.rkt"
         (prefix-in bot: (submod "bot.rkt" actions))
         "export.rkt"
         "registry.rkt")


;; canaries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 next
 done)

(define-syntax-parser define-canary
  [(_ id:id)
   #:with id? (format-id #'id "~a?" #'id)
   #'(define-values (id id?)
       (let ()
         (struct id () #:transparent)
         (values (id) id?)))])

(define-canary next)
(define-canary done)


;; storage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 with-study-transaction
 current-git-sha
 current-round-name
 current-group-name
 set-current-round-name!
 set-current-group-name!
 set-round-name!
 set-group-name!
 put
 put/instance
 put/instance-file
 put/group
 get
 get/instance
 get/instance-file
 get/group)

(define/contract current-git-sha
  (parameter/c (or/c #f string?))
  (make-parameter #f))

(define (deserialize* s)
  (deserialize (fasl->s-exp (open-input-bytes s))))

(define (serialize* v)
  (call-with-output-bytes
   (lambda (out)
     (s-exp->fasl (serialize v) out))))

(define (current-round-name)
  (study-participant-current-round-name (current-participant)))

(define (current-group-name [reload? #f])
  (if reload?
      (with-database-connection [conn (current-database)]
        (query-value conn (~> (from study-participant #:as p)
                              (where (= p.id ,(current-participant-id)))
                              (select p.current-group-name))))
      (study-participant-current-group-name (current-participant))))

(define (set-current-round-name! round-name)
  (define mgr (current-study-manager))
  (define updated-participant
    (with-database-connection [conn (current-database)]
      (~> (current-participant)
          (set-study-participant-current-round-name _ round-name)
          (update-one! conn _))))
  (set-study-manager-participant! mgr updated-participant))

(define (set-current-group-name! group-name)
  (define mgr (current-study-manager))
  (define updated-participant
    (with-database-connection [conn (current-database)]
      (~> (current-participant)
          (set-study-participant-current-group-name _ group-name)
          (update-one! conn _))))
  (set-study-manager-participant! mgr updated-participant))

(define (set-round-name! participant-id round-name)
  (with-database-connection [conn (current-database)]
    (and~> (lookup conn (~> (from study-participant #:as p)
                            (where (= p.id ,participant-id))))
           (set-study-participant-current-round-name _ round-name)
           (update-one! conn _))))

(define (set-group-name! participant-id group-name)
  (with-database-connection [conn (current-database)]
    (and~> (lookup conn (~> (from study-participant #:as p)
                            (where (= p.id ,participant-id))))
           (set-study-participant-current-group-name _ group-name)
           (update-one! conn _))))

(define (put k v
             #:round [round-name (current-round-name)]
             #:group [group-name (current-group-name)])
  (log-study-debug
   "put~n  stack: ~s~n  round: ~s~n  group: ~s~n  key: ~s~n  value: ~s~n  participant-id: ~s"
   (current-study-ids) round-name group-name k v (current-participant-id))
  (with-database-connection [conn (current-database)]
    (query-exec conn #<<QUERY
INSERT INTO study_data (
  participant_id, study_stack, round_name, group_name, key, value, git_sha
) VALUES (
  $1, $2, $3, $4, $5, $6, $7
) ON CONFLICT ON CONSTRAINT study_data_pkey DO UPDATE SET
  value = EXCLUDED.value,
  git_sha = EXCLUDED.git_sha,
  last_put_at = CURRENT_TIMESTAMP
QUERY
                (current-participant-id)
                (current-study-array)
                round-name
                group-name
                (symbol->string k)
                (serialize* v)
                (current-git-sha))))

(define (get k [default (lambda ()
                          (error 'get "value not found for key ~.s" k))]
             #:round [round-name (current-round-name)]
             #:group [group-name (current-group-name)])
  (log-study-debug
   "get~n  stack: ~s~n  round: ~s~n  group: ~s~n  key: ~s~n  participant-id: ~s"
   (current-study-ids) round-name group-name k (current-participant-id))
  (with-database-connection [conn (current-database)]
    (define maybe-value
      (query-maybe-value conn (~> (from "study_data" #:as d)
                                  (select d.value)
                                  (where (and
                                          (= d.participant-id ,(current-participant-id))
                                          (= d.study-stack ,(current-study-array))
                                          (= d.round-name ,round-name)
                                          (= d.group-name ,group-name)
                                          (= d.key ,(symbol->string k)))))))

    (cond
      [maybe-value => deserialize*]
      [(procedure? default) (default)]
      [else default])))

(define (call-with-study-transaction f)
  (let loop ()
    (with-handlers ([exn:fail:sql?
                     (lambda (e)
                       (case (exn:fail:sql-sqlstate e)
                         [("40001")
                          (log-study-warning "failed to apply instance transaction, retrying...")
                          (loop)]

                         [else
                          (raise e)]))])
      (with-database-transaction [conn (current-database)]
        #:isolation 'serializable
        (f)))))

(define-syntax-rule (with-study-transaction e0 e ...)
  (call-with-study-transaction (λ () e0 e ...)))

;; For later extension if we decide to store files outside the database.
;; This way studies that operate on files can be forward-compatible with
;; changes in the way we actually store files.
(define (put/instance-file v) v)
(define (get/instance-file v) v)

(define (put/instance k v)
  (log-study-debug "put/instance~n  stack: ~s~n  key: ~s~n  value: ~s~n  participant-id: ~s"
                   (current-study-ids) k v (current-participant-id))
  (with-database-transaction [conn (current-database)]
    (query-exec conn #<<QUERY
INSERT INTO study_instance_data(
  instance_id, study_stack, key, value, git_sha
) VALUES (
  $1, $2, $3, $4, $5
) ON CONFLICT ON CONSTRAINT study_instance_data_pkey DO UPDATE SET
  value = EXCLUDED.value,
  git_sha = EXCLUDED.git_sha,
  last_put_at = CURRENT_TIMESTAMP
QUERY
                (current-study-instance-id)
                (current-study-array)
                (symbol->string k)
                (serialize* v)
                (current-git-sha))))

(define (get/instance k [default (lambda ()
                                   (error 'get/instance "value not found for key ~.s" k))])
  (log-study-debug "get/instance~n  stack: ~s~n  key: ~s~n  participant-id: ~s"
                   (current-study-ids) k (current-participant-id))
  (with-database-transaction [conn (current-database)]
    (define maybe-value
      (query-maybe-value conn (~> (from "study_instance_data" #:as d)
                                  (select d.value)
                                  (where (and
                                          (= d.instance-id ,(current-study-instance-id))
                                          (= d.study-stack ,(current-study-array))
                                          (= d.key ,(symbol->string k)))))))

    (cond
      [maybe-value => deserialize*]
      [(procedure? default) (default)]
      [else default])))

(define (put/group k v)
  (log-study-debug "put/group~n  stack: ~s~n  key: ~s~n  value: ~s~n  participant-id: ~s" (current-study-ids) k v (current-participant-id))
  (with-database-transaction [conn (current-database)]
    (query-exec conn #<<QUERY
INSERT INTO study_group_data (
  round_name, group_name, study_stack, key, value, git_sha, last_put_by
) VALUES (
  $1, $2, $3, $4, $5, $6, $7
) ON CONFLICT ON CONSTRAINT study_group_data_pkey DO UPDATE SET
  value = EXCLUDED.value,
  git_sha = EXCLUDED.git_sha,
  last_put_by = EXCLUDED.last_put_by,
  last_put_at = CURRENT_TIMESTAMP
QUERY
                (current-round-name)
                (current-group-name)
                (current-study-array)
                (symbol->string k)
                (serialize* v)
                (current-git-sha)
                (current-participant-id))))

(define (get/group k [default (lambda ()
                                (error 'get/group "value not found for key ~.s" k))])
  (log-study-debug "get/group~n  stack: ~s~n  key: ~s~n  participant-id: ~s" (current-study-ids) k (current-participant-id))
  (with-database-transaction [conn (current-database)]
    (define maybe-value
      (query-maybe-value conn (~> (from "study_group_data" #:as d)
                                  (select d.value)
                                  (where (and
                                          (= d.round-name ,(current-round-name))
                                          (= d.group-name ,(current-group-name))
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


;; payments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 amount/c
 get-payment
 lookup-payments
 put-payment!
 get-all-payments)

; FIXME: Using exact naturals would be even better, but then we need
; to ensure that the payments are included in cents, rather than whole
; dollars. This will almost surely lead to errors of rounding or
; misunderstanding.
(define amount/c
  (and/c number? (or/c positive? zero?)))

; FIXME: The other schemas are later, but for the contract I need
; `study-payment?`

(define-schema study-payment
  #:table "payments"
  ([participant-id integer/f]
   [(timestamp (now/moment)) datetime-tz/f]
   [payment-name string/f]
   [payment (numeric/f 6 2)]))

;; Since this doesn't have a primary key, the returned study-payment
;; entity can't be update!d or delete!d.  Inserting the same payment
;; twice raises a constraint error.
(define/contract (put-payment! k payment)
  (-> symbol? amount/c void?)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (define pid (current-participant-id))
                     (define user
                       (make-sentry-user
                        #:id (~a pid)
                        #:email (participant-email pid)))
                     (sentry-capture-exception! e #:user user)
                     (log-study-error "put payment failed~n  exn: ~a" (exn-message e)))])
    (void
     (with-database-connection [conn (current-database)]
       (insert-one! conn (make-study-payment #:participant-id (current-participant-id)
                                             #:payment-name (symbol->string k)
                                             #:payment payment))))))

(define (get-payment k)
  (define result
    (with-database-connection [conn (current-database)]
      (lookup conn (~> (from study-payment #:as p)
                       (where (and (= ,(current-participant-id) p.participant-id)
                                   (= ,(symbol->string k) p.payment-name)))))))
  (cond [result => study-payment-payment]
        [else
         (error "no payment ~a found for participant ~a" k (current-participant-id))]))

(define/contract (lookup-payments db pid)
  (-> database? id/c (hash/c string? number?))
  (with-database-connection (conn db)
    (for/hash ([(name amount) (in-entities conn
                               (~> (from study-payment #:as p)
                                   (where (= p.participant-id ,pid))
                                   (select p.payment-name p.payment)))])
      (values name amount))))

(define (get-all-payments)
  (lookup-payments (current-database) (current-participant-id)))

;; widgets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 current-embed/url
 when-bot
 page
 button
 button/confirm
 form
 skip)

(define current-embed/url
  (make-parameter 'no-embed/url))

(define current-request
  (make-parameter 'no-request))

(define current-return
  (make-parameter 'no-return))

(define current-renderer
  (make-parameter 'no-renderer))

(define-syntax-rule (page e)
  (letrec ([render
            (lambda ()
              (parameterize ([current-renderer render])
                e))])
    (step-page render)))

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
        [the-renderer (current-renderer)]
        [the-request (current-request)]
        [return (current-return)]
        [the-step (current-step)]
        [bot? (current-user-bot?)])
    (syntax-parameterize ([embed
                           (syntax-parser
                             [(_ f:expr)
                              #'(embed/url
                                 (lambda (req)
                                   (parameterize ([current-embed/url embed/url]
                                                  [current-renderer the-renderer]
                                                  [current-request req]
                                                  [current-return return]
                                                  [current-step the-step]
                                                  [current-user-bot? bot?])
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

(define/widget (button action label #:id [id ""] #:to-step-id [to-step-id #f])
  (haml
   (:a.button.next-button
    ([:data-widget-id (when-bot id)]
     [:href
      (embed
       (lambda (_req)
         (action)
         (cond [to-step-id
                (continue to-step-id)]
               [else
                (continue)])))])
    label)))

(define/widget (button/confirm label
                               #:id [id ""]
                               #:cancel-id [cancel-id ""]
                               #:confirm-id [confirm-id ""]
                               #:confirm-xexpr [confirm-xexpr `(p "Are you sure?")]
                               . action-widgets)
  (haml
   (:a.button.next-button
    ([:data-widget-id (when-bot id)]
     [:href
      (embed
       (lambda (_req)
         (response/render
          this-step
          (lambda ()
            (haml
             (:div
              confirm-xexpr
              ,@(if (null? action-widgets)
                    (list
                     (haml
                      (.buttons
                       (button void "Cancel" #:id cancel-id #:to-step-id (step-id this-step))
                       (button void "Continue" #:id confirm-id))))
                    action-widgets)))))))])
    label)))

(define/widget (form f action render #:id [id ""] #:enctype [enctype "multipart/form-data"])
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
                    (response/render this-step (current-renderer))))]
        [:data-widget-id (when-bot id)]
        [:enctype enctype]
        [:method "POST"])
       (render rw)))]))

(define/widget (skip [to-step-id #f])
  (if to-step-id
      (continue to-step-id)
      (continue)))


;; step ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 current-xexpr-wrapper
 make-step
 make-step/study
 step-id
 step-page?
 study-transitions)

(module+ private
  (provide
   (struct-out step)
   (struct-out step/study)
   (struct-out study)))

(struct step (id handler handler/bot transition)
  #:transparent)

(struct step/study step (study)
  #:transparent)

(struct step-page (renderer)
  #:transparent)

(struct study (name requires provides transitions steps failure-handler)
  #:transparent)

(define step-id/c symbol?)
(define handler/c (-> step-page?))
(define transition-result/c (or/c done? next? step-id/c))
(define transition/c (-> transition-result/c))
(define binding/c (list/c symbol? (or/c symbol? (list/c 'const any/c))))

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
         (match binding
           [`(,dst-id (const ,v))
            (values dst-id v)]

           [`(,dst-id ,src-id)
            (values dst-id (get src-id (lambda ()
                                         (error 'get "value not found for key ~.s in sub-study~n  bound to: ~s~n  required by step: ~.s" src-id dst-id id))))])))
     (with-widget-parameterization
       (define result-bindings
         (run-study s #:bindings bindings))
       (for ([binding (in-list provide-bindings)])
         (match-define (list dst-id src-id) binding)
         (put dst-id (hash-ref result-bindings src-id (lambda ()
                                                        (error 'run-study/step "expected binding ~s to be provided by sub-study~n  bound to: ~s~n  at step: ~.s" src-id dst-id id)))))
       ;; We're done with the sub-study so clear out the resume stack
       ;; (if any) to avoid issues with resuming at a boundary between
       ;; two sub-studies (#24).
       (current-resume-stack null)
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

(define/contract current-xexpr-wrapper
  (parameter/c (-> xexpr? xexpr?))
  (make-parameter values))

(define (response/render s r)
  (response/xexpr
   ((current-xexpr-wrapper)
    (haml
     (:div.step
      ([:data-participant-id (~a (current-participant-id))]
       [:data-study-stack (when-bot (call-with-output-string
                                     (lambda (out)
                                       (write (current-study-stack) out))))]
       [:data-step-id (when-bot (step-id s))])
      (r))))))

(define (response/step s)
  (response/render s (step-page-renderer ((step-handler s)))))

(define-syntax-rule (when-bot e)
  (if (current-user-bot?) (~a e) ""))


;; study ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 current-study-stack
 wrap-sub-study
 make-study
 run-study
 fail
 study-steps
 step/study?
 step/study-study)

(define-logger study)

;; This is actually a stack of step ids where each step represents a [sub]study.
(define current-study-stack
  (make-parameter null))

(define current-step
  (make-parameter #f))

(define current-resume-stack
  (make-parameter null))

(define current-resume-done?
  (make-parameter (box #t)))

(define/contract (make-study name steps
                             #:requires [requires null]
                             #:provides [provides null]
                             #:transitions [transitions #f]
                             #:failure-handler [failure-hdl #f])
  (->* (string? (non-empty-listof step?))
       (#:requires (listof symbol?)
        #:provides (listof symbol?)
        #:transitions (or/c #f (hash/c symbol? any/c))
        #:failure-handler (or/c #f (-> step? any/c step-id/c)))
       study?)
  (define comptime-transitions (and transitions (hash-ref transitions 'comptime)))
  (define runtime-transitions (and transitions (hash-ref transitions 'runtime)))
  (define steps-with-transitions
    (cond
      [runtime-transitions
       (let ([transitions runtime-transitions])
         (define steps*
           (for/list ([s (in-list steps)])
             (define sid (step-id s))
             (cond
               [(assoc sid transitions)
                => (lambda (p)
                     (define fn-or-id (cdr p))
                     (define transition
                       (cond
                         [(symbol? fn-or-id) (λ () fn-or-id)]
                         [else fn-or-id]))
                     (cond
                       [(step/study? s)
                        (struct-copy step/study s [transition #:parent step transition])]
                       [(step? s)
                        (struct-copy step s [transition transition])]
                       [else
                        (raise-argument-error 'make-study "step?" s)]))]

               [else
                (raise-user-error 'make-study "no transition specified for step ~a in study ~a" sid name)])))

         ;; Ensure the first step in the study lines up with the first
         ;; specified transition.
         (define first-step-id (car (car transitions)))
         (define first-step
           (findf (λ (s)
                    (eq? (step-id s) first-step-id))
                  steps*))
         (cons first-step (remq first-step steps*)))]

      [else
       steps]))

  (study name requires provides comptime-transitions steps-with-transitions failure-hdl))

(define/contract (run-study s
                            [req (current-request)]
                            #:bindings [bindings (hasheq)])
  (->* (study?)
       (request?
        #:bindings (hash/c symbol? any/c))
       any)
  (define resume-stack
    (if (unbox (current-resume-done?))
        null
        (current-resume-stack)))
  (define root? (not (current-step)))
  (define new-study-stack
    (if root?
        '(*root*)
        (cons (step-id (current-step))
              (current-study-stack))))
  (log-study-debug "run study~n  name: ~e~n  steps: ~e~n  resume stack: ~e~n  new study stack: ~e~n  participant-id: ~s"
                   (study-name s)
                   (map step-id (study-steps s))
                   resume-stack
                   new-study-stack
                   (current-participant-id))
  (with-handlers ([exn:fail:study?
                   (lambda (e)
                     (define hdl (study-failure-handler s))
                     (define the-step (exn:fail:study-step e))
                     (define the-reason (exn:fail:study-reason e))
                     (cond
                       [hdl
                        (parameterize ([current-study-stack new-study-stack])
                          (define next-step-id (hdl the-step the-reason))
                          (define next-step (study-find-step s next-step-id))
                          (run-step req s next-step))]
                       [root?
                        (error 'run-study "fail~n  step: ~e~n  reason: ~a" the-step the-reason)]
                       [else
                        (fail the-reason the-step)]))])
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

        ;; When we "continue"...
        [else
         (define the-step
           (study-find-step s
                            (car resume-stack)
                            (lambda ()
                              (error 'run-study "failed to resume step in study~n  study: ~e~n  resume stack: ~e" (study-name s) resume-stack))))
         (begin0
             (parameterize ([current-resume-stack (cdr resume-stack)])
               (begin0 (run-step req s the-step)
                 (redirect/get/forget/protect)))
           (set-box! (current-resume-done?) #t))]))))

(define (run-step req s the-step)
  (log-study-debug "run step ~e for participant ~s" (step-id the-step) (current-participant-id))
  (update-participant-progress! (step-id the-step))
  (define res
    (call-with-current-continuation
     (lambda (return)
       (send/suspend/dispatch/protect
        (lambda (embed/url)
          (define (embed/url/fail proc)
            ;; Actions run inside `embed/url' request handlers, which
            ;; means we have to trampoline any `(fail)' exns back over
            ;; to our captured continuation s.t. they may be handled
            ;; by exception handlers installed up-stack.
            (embed/url
             (lambda (embed-req)
               (with-handlers ([exn:fail:study? return])
                 (proc embed-req)))))

          ;; These parameterizations are closed-over and re-set by
          ;; `embed` within handlers when necessary because `call/cc`
          ;; above captures everything outside up to the
          ;; `servlet-prompt' (in our case up to
          ;; `wrap-protect-continuations').
          (parameterize ([current-embed/url embed/url/fail]
                         [current-request req]
                         [current-return return]
                         [current-step the-step])
            (response/step the-step)))))
     servlet-prompt))

  (log-study-debug "step ~e returned ~e" (step-id the-step) res)
  (match res
    [(? exn:fail:study?)
     (raise res)]

    [(? response?)
     (send/back res)]

    [(cons 'to-step to-step-id)
     (define new-req (redirect/get/forget/protect))
     (define next-step
       (study-find-step s to-step-id (lambda ()
                                       (error 'run-step "skipped to a nonexistent step: ~s~n  current step: ~.s~n  current study: ~.s" to-step-id the-step s))))
     (run-step new-req s next-step)]

    [_
     ;; Forget here to prevent users from going back and re-running the transition.
     (redirect/get/forget/protect)
     (log-study-debug "running transition for step~n  id: ~e~n  study: ~e~n  resume stack: ~e~n  participant-id: ~s"
                      (step-id the-step)
                      (study-name s)
                      (current-resume-stack)
                      (current-participant-id))
     (define next-step
       (match ((step-transition the-step))
         [(? done?) #f]
         [(? next?) (study-find-next-step s (step-id the-step))]
         [next-step-id (study-find-step s next-step-id (lambda ()
                                                         (error 'run-step "transitioned to a nonexistent step: ~.s~n  current step: ~.s~n  current study: ~.s" next-step-id (step-id the-step) s)))]))

     ;; Forget here to prevent refreshing from re-running the transition.
     (define new-req (redirect/get/forget/protect))
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

(struct exn:fail:study exn:fail (step reason)
  #:transparent)

(define/contract (fail reason [s (current-step)])
  (->* (any/c) (step?) void?)
  (raise (exn:fail:study "study failed" (current-continuation-marks) s reason)))


;; db ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (schema-out researcher&instance)
 (schema-out study-meta)
 (schema-out study-instance)
 (schema-out study-instance-var)
 (schema-out study-participant)
 (schema-out study-participant/admin)
 (schema-out study-var)
 (schema-out study-payment)
 study-instance-var-value/deserialized
 study-var-value/deserialized
 make-study-manager
 call-with-study-manager
 list-studies
 list-study-instances
 list-study-instances-by-researcher
 list-study-instances-for-owner
 list-all-study-instances
 list-active-study-instances
 list-study-instance-participants/admin
 list-study-instance-payments/admin
 list-study-instance-total-payments/admin
 list-study-instance-vars
 clear-study-instance-vars!
 current-participant-id
 participant-email
 current-participant-owner?
 current-study-instance-id
 current-study-instance-name
 clear-participant-progress!
 participant-enrolled?
 enroll-participant!
 lookup-study
 lookup-study-meta
 lookup-study-instance
 lookup-study-instance-for-researcher
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
   [owner-id integer/f]
   [name string/f #:contract non-empty-string?]
   [slug string/f #:contract non-empty-string?]
   [(enrollment-code (generate-random-string 16)) string/f]
   [(status 'active) symbol/f #:contract (or/c 'active 'inactive 'archived)]
   [(created-at (now/moment)) datetime-tz/f]))

(define-schema researcher&instance
  #:virtual
  ([researcher-id integer/f]
   [researcher-username string/f]
   [instance-id integer/f]
   [instance-name string/f]
   [created-at datetime-tz/f]))

(define-schema study-instance-var
  #:table "study_instance_data"
  ([instance-id integer/f]
   [stack (array/f string/f) #:name "study_stack"]
   [id symbol/f #:name "key"]
   [value binary/f]
   [first-put-at datetime-tz/f]
   [last-put-at datetime-tz/f])
  #:methods gen:jsexprable
  [(define/generic ->jsexpr/super ->jsexpr)
   (define (->jsexpr v)
     (match-define (study-instance-var _meta _ stack id value first-put-at last-put-at) v)
     (hash 'stack (vector->list stack)
           'id (symbol->string id)
           'value (->jsexpr/super (study-instance-var-value/deserialized v))
           'first-put-at (moment->iso8601 first-put-at)
           'last-put-at (moment->iso8601 last-put-at)))])

(define-schema study-participant
  #:table "study_participants"
  ([id integer/f #:primary-key #:auto-increment]
   [(current-round-name "") string/f]
   [(current-group-name "") string/f]
   [user-id integer/f]
   [instance-id integer/f]
   [(progress #()) (array/f string/f)]
   [(enrolled-at (now/moment)) datetime-tz/f]))

(define-schema study-participant/admin
  #:virtual
  ([id integer/f]
   [user-id integer/f]
   [email string/f]
   [roles (array/f symbol/f)]
   [progress (array/f string/f)]
   [(current-round-name "") string/f]
   [(current-group-name "") string/f]
   [enrolled-at datetime-tz/f]))

(define-schema study-var
  #:virtual
  ([stack (array/f string/f)]
   [round-name string/f]
   [group-name string/f]
   [id symbol/f]
   [value binary/f]
   [first-put-at datetime-tz/f]
   [last-put-at datetime-tz/f])
  #:methods gen:jsexprable
  [(define/generic ->jsexpr/super ->jsexpr)
   (define (->jsexpr v)
     (match-define (study-var _ stack round-name group-name id _ first-put-at last-put-at) v)
     (hash 'stack (vector->list stack)
           'round round-name
           'group group-name
           'id (symbol->string id)
           'value (->jsexpr/super (study-var-value/deserialized v))
           'first-put-at (moment->iso8601 first-put-at)
           'last-put-at (moment->iso8601 last-put-at)))])

(define study-instance-var-value/deserialized
  (compose1 deserialize* study-instance-var-value))

(define study-var-value/deserialized
  (compose1 deserialize* study-var-value))

(struct study-manager ([participant #:mutable] db)
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

(define current-participant
  (compose1 study-manager-participant current-study-manager))

(define current-participant-id
  (compose1 study-participant-id current-participant))

(define current-study-instance-id
  (compose1 study-participant-instance-id current-participant))

(define (current-study-instance-name)
  (study-instance-name
   (lookup-study-instance (current-database) (current-study-instance-id))))

(define (current-study-instance-owner-id)
  (study-instance-owner-id
   (lookup-study-instance (current-database) (current-study-instance-id))))

(define current-participant-user-id
  (compose1 study-participant-user-id current-participant))

(define (current-participant-owner?)
  (equal? (current-participant-user-id) (current-study-instance-owner-id)))

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

(define/contract (list-study-instances-by-researcher db study-id)
  (-> database? id/c (listof researcher&instance?))
  (with-database-connection [conn db]
    (sequence->list
     (in-entities conn (~> (from user #:as u)
                           (join study-instance #:as i #:on (= i.owner-id u.id))
                           (where (= i.study-id ,study-id))
                           (select u.id u.username i.id i.name i.created-at)
                           (project-onto researcher&instance-schema))))))

(define/contract (list-study-instances-for-owner db study-id owner-id)
  (-> database? id/c id/c (listof study-instance?))
  (with-database-connection [conn db]
    (sequence->list
     (in-entities conn (~> (from study-instance #:as i)
                           (where (and (= i.study-id ,study-id)
                                       (= i.owner-id ,owner-id)))
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

(define/contract (list-study-instance-vars db instance-id)
  (-> database? id/c (listof study-instance-var?))
  (with-database-connection [conn db]
    (sequence->list
     (in-entities conn (~> (from study-instance-var #:as v)
                           (where (= v.instance-id ,instance-id))
                           (order-by ([v.first-put-at #:asc])))))))

(define/contract (list-study-instance-participants/admin db instance-id [for-bot-set #f])
  (->* (database? id/c) ((or/c #f id/c)) (listof study-participant/admin?))
  (with-database-connection [conn db]
    (define q
      (~> (from study-participant #:as p)
          (join user #:as u #:on (= u.id p.user-id))
          (where (= p.instance-id ,instance-id))
          (order-by ([p.enrolled-at #:desc]))
          (select p.id u.id u.username u.roles p.progress p.current-round-name p.current-group-name p.enrolled-at)
          (project-onto study-participant/admin-schema)))

    (sequence->list
     (in-entities conn (if for-bot-set
                           (where q (= u.bot-set-id ,for-bot-set))
                           (where q (or (not (array-contains? u.roles (array "bot")))
                                        (and (array-contains? u.roles (array "bot"))
                                             (is u.bot-set-id null)))))))))

(define/contract (list-study-instance-total-payments/admin db instance-id)
  (-> database? id/c (listof (list/c id/c string? number?)))
  (with-database-connection [conn db]
    (for/list ([(pid username total)
                (in-entities conn (~> (from study-participant #:as p)
                                      (join study-payment #:as sp #:on (= sp.participant-id p.id))
                                      (join "users" #:as u #:on (= u.id p.user-id))
                                      (select p.id u.username (sum sp.payment))
                                      (group-by u.username p.id)
                                      (where (= p.instance-id ,instance-id))))])
      (list pid username total))))

(define/contract (list-study-instance-payments/admin db instance-id)
  (-> database? id/c (listof (list/c number? string? number?)))
  (with-database-connection [conn db]
    (for/list ([(pid payment-name amount)
                (in-entities conn (~> (from study-participant #:as p)
                                      (join study-payment #:as sp #:on (= p.id sp.participant-id))
                                      (select p.id sp.payment-name sp.payment)
                                      (where (= p.instance-id ,instance-id))))])
      `(,pid ,payment-name ,amount))))

(define/contract (clear-study-instance-vars! db instance-id)
  (-> database? id/c void?)
  (with-database-connection [conn db]
    (query-exec conn (~> (from study-instance-var #:as v)
                         (where (= v.instance-id ,instance-id))
                         (delete)))))

(define/contract (participant-enrolled? db user-id instance-id)
  (-> database? id/c id/c boolean?)
  (with-database-connection [conn db]
    (query-maybe-value conn (~> (from study-participant #:as p)
                                (select #t)
                                (where (and (= p.user-id ,user-id)
                                            (= p.instance-id ,instance-id)))))))

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

(define/contract (lookup-study-instance-for-researcher db instance-id researcher-id)
  (-> database? id/c id/c (or/c #f study-instance?))
  (with-database-connection [conn db]
    (lookup conn (~> (from study-instance #:as i)
                     (where (and (= i.id ,instance-id)
                                 (= i.owner-id ,researcher-id)))))))

(define/contract (lookup-study-participant/admin db participant-id)
  (-> database? id/c (or/c #f study-participant/admin?))
  (with-database-connection [conn db]
    (lookup conn (~> (from study-participant #:as p)
                     (join user #:as u #:on (= u.id p.user-id))
                     (where (= p.id ,participant-id))
                     (select p.id u.id u.username u.roles p.progress p.current-round-name p.current-group-name p.enrolled-at)
                     (project-onto study-participant/admin-schema)))))

;; FIXME: Needs to deal with emails for users signed up via identity
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
                           (select d.study-stack d.round-name d.group-name d.key d.value d.first-put-at d.last-put-at)
                           (project-onto study-var-schema)
                           (where (= d.participant-id ,participant-id))
                           (order-by ([d.first-put-at #:asc])))))))

(define/contract (clear-participant-progress! db participant-id)
  (-> database? id/c void?)
  (with-database-transaction [conn db]
    (query-exec conn (~> (from "study_participants" #:as p)
                         (where (= p.id ,participant-id))
                         (update
                          [progress ,(list->pg-array null)]
                          [current-round-name ""]
                          [current-group-name ""])))
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

(define (current-participant-progress m)
  (define p (study-manager-participant m))
  (for*/list ([id:str (in-vector (study-participant-progress p))]
              [id (in-value (string->symbol id:str))]
              #:unless (eq? id '*root*))
    id))

(define/contract (call-with-study-manager mgr f)
  (-> study-manager? (-> any) any)
  (parameterize ([current-study-manager mgr]
                 [current-resume-stack (current-participant-progress mgr)]
                 [current-resume-done? (box #f)])
    (f)))
