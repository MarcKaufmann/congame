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
         koyo/http
         koyo/random
         (except-in forms form)
         racket/contract/base
         racket/contract/region
         racket/fasl
         racket/format
         racket/generic
         racket/lazy-require
         racket/match
         racket/port
         racket/sequence
         racket/serialize
         racket/splicing
         racket/string
         racket/stxparam
         syntax/parse/define
         threading
         web-server/dispatchers/dispatch
         web-server/servlet
         "bot.rkt"
         (prefix-in bot: (submod "bot.rkt" actions))
         "export.rkt"
         "registry.rkt"
         "struct.rkt"
         "xexpr.rkt")

(lazy-require
 ["dsl.rkt" (dsl-require)])


;; canaries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-parser define-canary
  [(_ id:id)
   #:with id? (format-id #'id "~a?" #'id)
   #'(begin
       (provide id id?)
       (define-values (id id?)
         (let ()
           (struct id () #:transparent)
           (values (id) id?))))])

(define-canary next)
(define-canary done)


;; storage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 call-with-study-transaction
 with-study-transaction
 current-git-sha
 get-current-round-stack
 get-current-round-name
 get-current-group-stack
 get-current-group-name
 put-current-round-name
 put-current-group-name
 put
 put/instance
 put/instance-file
 get
 get/instance
 get/instance-file
 get/linked/instance
 get-progress)

(define/contract current-git-sha
  (parameter/c (or/c #f string?))
  (make-parameter #f))

(define (deserialize* s)
  (deserialize (fasl->s-exp (open-input-bytes s))))

(define (serialize* v)
  (call-with-output-bytes
   (lambda (out)
     (s-exp->fasl (serialize v) out))))

(define (get-current-round-stack)
  (let loop ([stack (current-study-stack)])
    (parameterize ([current-study-stack stack])
      (cond
        [(null? stack) null]
        [else
         (define reversed-stack
           (get
            #:root '*round*
            'round-stack
            (list "")))
         (cons
          (car (reverse reversed-stack))
          (loop (cdr stack)))]))))

(define (get-current-round-name)
  (car (get-current-round-stack)))

(define (get-current-group-stack #:participant-id [participant-id (current-participant-id)])
  (let loop ([stack (current-study-stack)])
    (parameterize ([current-study-stack stack])
      (cond
        [(null? stack) null]
        [else
         (define reversed-stack
           (get
            #:root '*group*
            #:participant-id participant-id
            'group-stack
            (list "")))
         (cons
          (car (reverse reversed-stack))
          (loop (cdr stack)))]))))

(define (get-current-group-name)
  (car (get-current-group-stack)))

(define (put-current-round-name round-name)
  (define round-stack
    (get-current-round-stack))
  (put #:root '*round*
       'round-stack (reverse (cons round-name (cdr round-stack)))))

(define (put-current-group-name group-name #:participant-id [participant-id (current-participant-id)])
  (define group-stack
    (get-current-group-stack #:participant-id participant-id))
  (put #:root '*group*
       #:participant-id participant-id
       'group-stack (reverse (cons group-name (cdr group-stack)))))

;; SECURITY: See note on `get'.
(define (put k v
             #:root [root-id '*root*]
             #:round [round-stack (list "")]
             #:group [group-stack (list "")]
             #:participant-id [participant-id (current-participant-id)])
  (define reversed-round-stack (reverse round-stack))
  (define reversed-group-stack (reverse group-stack))
  (log-study-debug
   "put~n  stack: ~s~n  root: ~s~n  round: ~s~n  group: ~s~n  key: ~s~n  value: ~.s~n  participant-id: ~s"
   (current-study-ids) root-id reversed-round-stack reversed-group-stack k v participant-id)
  (with-database-transaction [conn (current-database)]
    (define current-instance-id
      (study-participant-instance-id (current-participant)))
    (define target-instance-id
      (query-maybe-value conn (~> (from "study_participants" #:as p)
                                  (select p.instance-id)
                                  (where (= p.id ,participant-id)))))
    (unless (eqv? current-instance-id target-instance-id)
      (error 'put "target participant is in a different study"))

    (query-exec conn #<<QUERY
INSERT INTO study_data (
  participant_id, study_stack, round_stack, group_stack, key, value, git_sha
) VALUES (
  $1, $2, $3, $4, $5, $6, $7
) ON CONFLICT ON CONSTRAINT study_data_pkey DO UPDATE SET
  value = EXCLUDED.value,
  git_sha = EXCLUDED.git_sha,
  last_put_at = CURRENT_TIMESTAMP
QUERY
                participant-id
                (current-study-array root-id)
                (list->pg-array reversed-round-stack)
                (list->pg-array reversed-group-stack)
                (symbol->string k)
                (serialize* v)
                (current-git-sha))))

;; SECURITY: When performing gets, we try to make sure the current
;; participant's study instance id is the same as the study instance id
;; of the target participant id. This prevents inadvertent access to
;; other study instances' data. However, since we base our notion of
;; "current participant" on a parameter, and that parameter is provided
;; by this module, anyone with access to the code can fake it. For
;; #lang conscript, we can simply not provide users with the structures
;; necessary to fake that information, but for regular studies this
;; poses a potential security threat.
(define (get k [default (lambda ()
                          (error 'get "value not found for key ~.s" k))]
             #:root [root-id '*root*]
             #:round [round-stack (list "")]
             #:group [group-stack (list "")]
             #:participant-id [participant-id (current-participant-id)])
  (define reversed-round-stack (reverse round-stack))
  (define reversed-group-stack (reverse group-stack))
  (log-study-debug
   "get~n  stack: ~s~n  root: ~s~n  round: ~s~n  group: ~s~n  key: ~s~n  participant-id: ~s"
   (current-study-ids) root-id reversed-round-stack reversed-group-stack k participant-id)
  (with-database-connection [conn (current-database)]
    (define maybe-value
      (query-maybe-value conn (~> (from "study_data" #:as d)
                                  (join "study_participants" #:as p #:on (= d.participant-id p.id))
                                  (select d.value)
                                  (where (and
                                          (= d.participant-id ,participant-id)
                                          (= d.study-stack ,(current-study-array root-id))
                                          (= d.round-stack ,(list->pg-array reversed-round-stack))
                                          (= d.group-stack ,(list->pg-array reversed-group-stack))
                                          (= d.key ,(symbol->string k))
                                          (= p.instance-id ,(study-participant-instance-id (current-participant))))))))

    (cond
      [maybe-value => deserialize*]
      [(procedure? default) (default)]
      [else default])))

(define (get-progress)
  (with-database-connection [conn (current-database)]
    (for/hasheqv ([(pid progress)
                   (in-entities conn (~> (from study-participant #:as p)
                                         (select p.id p.progress)
                                         (where (= p.instance-id ,(study-participant-instance-id
                                                                   (current-participant))))))])
      (values pid (pg-array->list progress)))))

(define (call-with-study-transaction f)
  (let loop ()
    (with-handlers* ([exn:fail:sql?
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

;; TODO: We may eventually want a way to share data across studies or
;; study instances.  Eg. having a correlated treatment across studies
;; for the same individual.
;;
;; Additionally, we may want a variant of {put,get}/instance that
;; makes it easy to get & store group-level data, implemented on top
;; of {put,get}/instance.
(define (put/instance k v
                      #:root [root-id '*root*])
  (log-study-debug
   "put/instance~n  stack: ~s~n  key: ~s~n  value: ~s~n  root: ~s~n  participant-id: ~s~n  current-git-sha: ~s~n"
   (current-study-ids) k v root-id (current-participant-id) (current-git-sha))
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
                (current-study-array root-id)
                (symbol->string k)
                (serialize* v)
                (current-git-sha))))

(define (get/instance k
                      [default (lambda ()
                                 (error 'get/instance "value not found for key ~.s" k))]
                      #:root [root-id '*root*])
  (log-study-debug
   "get/instance~n  stack: ~s~n  key: ~s~n  root: ~s~n  participant-id: ~s"
   (current-study-ids) k root-id (current-participant-id))
  (do-get/instance
   (current-study-instance-id)
   (current-study-array root-id)
   (symbol->string k)
   default))

(define (do-get/instance instance-id stack key default)
  (with-database-transaction [conn (current-database)]
    (define maybe-value
      (query-maybe-value conn (~> (from "study_instance_data" #:as d)
                                  (select d.value)
                                  (where (and
                                          (= d.instance-id ,instance-id)
                                          (= d.study-stack ,stack)
                                          (= d.key ,key))))))

    (cond
      [maybe-value => deserialize*]
      [(procedure? default) (default)]
      [else default])))

(define (get/linked/instance pseudonym k
                             [default (λ () (error 'get/instance/linked "value not found for ~s" k))]
                             #:root [root-id '*root*])
  (with-database-transaction [conn (current-database)]
    (define instance-id
      (query-maybe-value
       conn
       (~> (from study-instance-link #:as this-link)
           (join study-instance-link
                 #:as other-link
                 #:on (and (= other-link.study-instance-id-b this-link.study-instance-id-a)
                           (= other-link.study-instance-id-a this-link.study-instance-id-b)))
           (where (and (= this-link.study-instance-id-a ,(current-study-instance-id))
                       (= this-link.pseudonym-b ,pseudonym)
                       (= this-link.relationship "reporter")
                       (= other-link.relationship "source")))
           (select this-link.study-instance-id-b))))
    (unless instance-id
      (error 'get/linked/instance "no link for pseudonym ~a" pseudonym))
    (do-get/instance
     instance-id
     (current-study-array root-id)
     (symbol->string k)
     default)))

(define (current-study-ids)
  (reverse (current-study-stack)))

(define (current-study-array [root-id '*root*])
  (list->pg-array
   (for/list ([id (in-list (current-study-ids))])
     (symbol->string (if (eq? id '*root*) root-id id)))))


;; vars ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 with-namespace
 defvar
 defvar*
 defvar/instance
 defvar*/instance
 get-var
 get-var*
 get-var*/instance
 get-var/instance
 put-var
 put-var*
 put-var*/instance
 put-var/instance
 undefined
 undefined?
 if-undefined)

(define-syntax-parameter current-var-namespace #f)

(define-syntax (with-namespace stx)
  (syntax-parse stx
    [(_ namespace:id body ...+)
     #'(splicing-syntax-parameterize ([current-var-namespace 'namespace])
         body ...)]))

(define undefined
  (string->uninterned-symbol "undefined"))
(define (undefined? v)
  (eq? v undefined))
(define-syntax-rule (if-undefined val-expr then-expr)
  (let ([tmp val-expr])
    (if (undefined? tmp) then-expr tmp)))

;; TODO(doc): The reason defvar has no default is because, while we
;; could give that reasonable semantics at the Racket level, that
;; default is not going to be in the database so researchers could end
;; up relying on data that doesn't make it into the final dataset.
(define-syntax (defvar stx)
  (syntax-parse stx
    [(_ id:id)
     #'(begin
         (define-syntax id
           (make-set!-transformer
            (lambda (stx)
              (syntax-case stx (set!)
                [(set! id v) #'(put-var 'id v)]
                [id (identifier? #'id) #'(get-var 'id)])))))]))

(define (get-var k)
  (get k undefined))

(define (put-var k v)
  (put k v))

(define-syntax (defvar/instance stx)
  (syntax-parse stx
    [(_ id:id)
     #`(begin
         (define-syntax id
           (make-set!-transformer
            (lambda (stx)
              (syntax-case stx (set!)
                [(set! id v) #'(put-var/instance 'id v)]
                [id (identifier? #'id) #'(get-var/instance 'id)])))))]))

(define (put-var/instance k v)
  (put/instance k v))

(define (get-var/instance k)
  (get/instance k undefined))

(define-syntax (defvar* stx)
  (syntax-parse stx
    [(_ id:id unique-id:id)
     #`(begin
         (define-syntax id
           (make-set!-transformer
            (lambda (stx)
              (syntax-case stx (set!)
                [(set! id v) #'(put-var* 'unique-id 'id v)]
                [id (identifier? #'id) #'(get-var* 'unique-id 'id)])))))]
    [(_ id:id)
     #:with namespace (syntax-parameter-value #'current-var-namespace)
     #:fail-unless (syntax->datum #'namespace)
     "arity 1 defvar* must be used inside a with-namespace block"
     #:with unique-id (format-id #'id "~a:~a" #'namespace #'id)
     #'(defvar* id unique-id)]))

(define (put-var* uid k v)
  (parameterize ([current-study-stack '(*root*)])
    (put #:root (string->symbol (format "*dynamic:~a*" uid)) k v)))

(define (get-var* uid k)
  (parameterize ([current-study-stack '(*root*)])
    (get #:root (string->symbol (format "*dynamic:~a*" uid)) k undefined)))

(define-syntax (defvar*/instance stx)
  (syntax-parse stx
    [(_ id:id unique-id:id)
     #`(begin
         (define-syntax id
           (make-set!-transformer
            (lambda (stx)
              (syntax-case stx (set!)
                [(set! id v) #'(put-var*/instance 'unique-id 'id v)]
                [id (identifier? #'id) #'(get-var*/instance 'unique-id 'id)])))))]
    [(_ id:id)
     #:with namespace (syntax-parameter-value #'current-var-namespace)
     #:fail-unless (syntax->datum #'namespace)
     "arity 1 defvar*/instance must be used inside a with-namespace block"
     #:with unique-id (format-id #'id "~a:~a" #'namespace #'id)
     #'(defvar*/instance id unique-id)]))

(define (put-var*/instance uid k v)
  (parameterize ([current-study-stack '(*root*)])
    (put/instance #:root (string->symbol (format "*dynamic:~a*" uid)) k v)))

(define (get-var*/instance uid k)
  (parameterize ([current-study-stack '(*root*)])
    (get/instance #:root (string->symbol (format "*dynamic:~a*" uid)) k undefined)))


;; timings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 current-step-timings)

;; current-step-timings : (cons total-time focus-time)
;; Undefined behavior when accessed within a step. Only read this data
;; within a widget's action or during a step transition.
(define current-step-timings
  (make-parameter (cons #f #f)))

(define (call-with-timings req proc)
  (define binds (request-bindings/raw req))
  (define tt (bindings-ref-number binds '__tt))
  (define ft (bindings-ref-number binds '__ft))
  (parameterize ([current-step-timings (cons tt ft)])
    (proc)))


;; widgets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 when-bot
 page
 make-stub
 button
 button/confirm
 form
 skip
 attachment)

(define current-embed/url
  (make-parameter 'no-embed/url))

(define current-request
  (make-parameter 'no-request))

(define current-return
  (make-parameter 'no-return))

(define current-renderer
  (make-parameter 'no-renderer))

(module+ private
  (provide
   current-embed/url
   current-request
   current-return
   current-renderer))

;; The point of this is to wrap rendering in a thunk so that we may
;; re-run the rendering part upon form validation error, but not the
;; rest of the step (i.e. the stuff before the page).
(define-syntax-parser page
  [(_ {~optional {~seq #:validator validator-expr:expr}} e)
   #'(letrec ([render
               (lambda ()
                 (parameterize ([current-renderer render])
                   e))])
       (step-page render {~? validator-expr validate-xexpr}))])

(define ((make-stub title [final? #f]))
  (page
   (haml
    (.container
     (:h1 title)
     (unless final?
       (button void "Next"))))))

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
                              #'(return `(continue ,(current-parameterization)))]

                             [(_ to:id)
                              #'(return `(to-step ,to ,(current-parameterization)))])]

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

(define/widget (button action label
                       #:id [id ""]
                       #:to-step-id [to-step-id #f]
                       #:up-target [up-target ".step"]
                       #:up-transition [up-transition "none"])
  (haml
   (:a.button.next-button
    ([:up-follow up-target]
     [:up-transition up-transition]
     [:data-widget-id (when-bot id)]
     [:href
      (embed
       (lambda (req)
         (call-with-timings
          req
          (lambda ()
            (action)
            (if to-step-id
                (continue to-step-id)
                (continue))))))])
    label)))

;; NOTE: This approach seems like a bad idea. Instead, use a separate
;; step to implement the confirm/cancel part of this in order to better
;; integrate with the rest of the system. Currently, this does not play
;; well with timings.
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

(define nested-form-guard
  (make-parameter #f))

(define/widget (form f action render
                     #:id [id ""]
                     #:enctype [enctype "multipart/form-data"]
                     #:combine [combine-proc (λ (_k _v1 v2) v2)]
                     #:defaults [defaults (hash)])
  (when (nested-form-guard)
    (error 'form "cannot nest forms"))
  (match (form-run
          #:combine combine-proc
          #:defaults defaults
          f this-request)
    [(list 'passed res _)
     (call-with-timings
      this-request
      (lambda ()
        (redirect/get/forget/protect)
        (action res)
        (continue)))]

    [(list _ _ rw)
     (haml
      (:form
       ([:action (embed
                  (lambda (_req)
                    (response/render this-step (current-renderer))))]
        [:data-widget-id (when-bot id)]
        [:enctype enctype]
        [:method "POST"])
       (parameterize ([nested-form-guard #t])
         (render rw))))]))

;; FIXME: Skipping from a nested step fails because skip ends up
;; returning from an already-expired step somehow. See
;; skip-after-refresh.
(define/widget (skip [to-step-id #f])
  (if to-step-id
      (continue to-step-id)
      (continue)))

(define/widget (attachment label proc
                           #:filename [filename "data.txt"]
                           #:content-type [content-type "text/plain"])
  (haml
   (:a.button.attachment-button
    ([:href (embed
             (lambda (req)
               (call-with-timings
                req
                (lambda ()
                  (response/output
                   #:mime-type (string->bytes/utf-8 content-type)
                   #:headers (list
                              (make-header
                               #"content-disposition"
                               (string->bytes/utf-8 (format "attachment; filename=\"~a\"" filename))))
                   proc)))))])
    label)))


;; step ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 current-xexpr-wrapper
 step-id
 step-page?
 study?
 step?
 study-transitions

 (contract-out
  [make-step (->* (step-id/c handler/c)
                  (transition/c
                   #:view-handler (or/c #f (-> request? response?))
                   #:for-bot procedure?)
                  step?)]
  [make-step/study (->* (step-id/c (or/c study? (-> study?)))
                        (transition/c
                         #:require-bindings (listof binding/c)
                         #:provide-bindings (listof binding/c))
                        step?)]))

(module+ private
  (provide
   (struct-out step)
   (struct-out step/study)
   (struct-out study)))

(define step-id/c symbol?)
(define handler/c (-> step-page?))
(define transition-result/c (or/c done? next? step-id/c))
(define transition/c (-> transition-result/c))
(define binding/c (list/c symbol? (or/c symbol? (list/c 'const any/c) (-> any/c))))

(define (default-transition)
  next)

(define (make-step id
                   handler
                   [transition default-transition]
                   #:view-handler [view-hdl #f]
                   #:for-bot [handler/bot
                              (lambda ()
                                (if (eq? transition default-transition)
                                    (bot:continuer)
                                    (raise-user-error "no bot transition for step" id)))])
  (step id handler handler/bot view-hdl transition))

(define (make-step/study id study-or-proc
                         [transition (lambda () next)]
                         #:require-bindings [require-bindings null]
                         #:provide-bindings [provide-bindings null])
  (define (handler)
    (define s
      (if (procedure? study-or-proc)
          (study-or-proc)
          study-or-proc))
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

          [`(,dst-id ,(? procedure? src-proc))
           (values dst-id (src-proc))]

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
  (step/study id handler void #f transition study-or-proc))

(define/contract (map-step s proc)
  (-> step? (-> handler/c handler/c) step?)
  (if (step/study? s)
      (make-step/study
       (step-id s)
       (let ([study-or-proc (step/study-study s)])
         (if (procedure? study-or-proc)
             (λ () (map-study (study-or-proc) proc))
             (map-study study-or-proc proc)))
       (step-transition s))
      (make-step
       (step-id s)
       (proc (step-handler s))
       (step-transition s))))

(define/contract (map-study s proc)
  (-> study? (-> handler/c handler/c) study?)
  (struct-copy study s [steps (for/list ([a-step (in-list (study-steps s))])
                                (map-step a-step proc))]))

(define current-xexpr-wrapper
  (make-parameter values))

(define (response/render s r [validator validate-xexpr])
  (response/xexpr*
   #:validator validator
   ((current-xexpr-wrapper)
    (haml
     (:div.step
      ([:data-participant-id (~a (current-participant-id))]
       [:data-study-stack (when-bot (call-with-output-string
                                     (lambda (out)
                                       (write (current-study-stack) out))))]
       [:data-step-id (when-bot (step-id s))]
       [:data-track-timings ""])
      (r))))))

(define (response/step s)
  (match-define (step-page renderer validator)
    ((step-handler s)))
  (response/render s renderer validator))

(module+ private
  (provide response/step))

(define-syntax-rule (when-bot e)
  (if (current-user-bot?) (~a e) ""))


;; study ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 current-step
 current-study-stack
 map-step
 map-study
 make-study
 run-study
 view-study
 fail
 study-steps
 step/study?
 step/study-study)

(define-logger study)

;; This is actually a stack of step ids where each step represents a [sub]study.
(define current-study-stack
  (make-parameter null))

(module+ private
  (provide current-study-stack))

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
                             #:view-handler [view-hdl #f]
                             #:failure-handler [failure-hdl #f])
  (->* (string? (non-empty-listof step?))
       (#:requires (listof symbol?)
        #:provides (listof symbol?)
        #:transitions (or/c #f (hash/c symbol? any/c))
        #:view-handler (or/c #f (-> request? response?))
        #:failure-handler (or/c #f (-> step? any/c step-id/c)))
       study?)
  (define known-steps
    (for/hash ([s (in-list steps)])
      (values (step-id s) #t)))
  (define comptime-transitions
    (and transitions (hash-ref transitions 'comptime)))
  (define runtime-transitions
    (and transitions (hash-ref transitions 'runtime)))
  (define steps-with-transitions
    (cond
      [runtime-transitions
       (let ([transitions runtime-transitions])
         (define known-steps-str
           (string-join (map symbol->string (sort (hash-keys known-steps) symbol<?)) ", "))
         (define steps*
           (for/list ([s (in-list steps)])
             (define sid (step-id s))
             (cond
               [(assoc sid transitions)
                => (lambda (p)
                     (define fn-or-id (cdr p))
                     (define transition
                       (cond
                         [(symbol? fn-or-id)
                          (unless (hash-has-key? known-steps fn-or-id)
                            (error 'make-study
                                   "expected a known step~n  unknown step: ~a~n  known steps: ~a~n  study: ~a"
                                   fn-or-id known-steps-str name))
                          (λ () fn-or-id)]
                         [else fn-or-id]))
                     (cond
                       [(step/study? s)
                        (struct-copy step/study s [transition #:parent step transition])]
                       [(step? s)
                        (struct-copy step s [transition transition])]
                       [else
                        (raise-argument-error 'make-study "step?" s)]))]

               [else
                (error 'make-study "no transition specified for step ~a in study ~a" sid name)])))

         ;; Ensure the first step in the study lines up with the first
         ;; specified transition.
         (define first-step-id (car (car transitions)))
         (define first-step
           (findf (λ (s)
                    (eq? (step-id s) first-step-id))
                  steps*))
         (unless first-step
           (error 'make-study
                  "unknown first step: ~a~n  known steps: ~a~n  study: ~a"
                  first-step-id known-steps-str name))
         (cons first-step (remq first-step steps*)))]

      [else
       steps]))

  (study name requires provides comptime-transitions steps-with-transitions view-hdl failure-hdl))

(define/contract (run-study s
                            [req (current-request)]
                            #:bindings [bindings (hasheq)])
  (->* [study?]
       [request?
        #:bindings (hash/c symbol? any/c)]
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
  (define (failure-hdl e)
    (define hdl (study-failure-handler s))
    (define the-step (exn:fail:study-step e))
    (define the-reason (exn:fail:study-reason e))
    (cond
      [hdl
       (with-handlers ([exn:fail:study? failure-hdl])
         (parameterize ([current-study-stack new-study-stack])
           (define next-step-id (hdl the-step the-reason))
           (define next-step (study-find-step s next-step-id))
           (run-step req s next-step)))]
      [root?
       (error 'run-study "fail~n  step: ~e~n  reason: ~a" the-step the-reason)]
      [else
       (fail the-reason the-step)]))
  ;; NOTE: All these handlers create intermediate frames. So, the more
  ;; handlers there are, the bigger the memory use per user per study
  ;; instance. Once a user stops hitting continuation links for 4 hours,
  ;; these are released.
  (with-handlers ([exn:fail:study? failure-hdl])
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
         (define new-resume-stack (cdr resume-stack))
         (parameterize ([current-resume-stack new-resume-stack])
           (when (null? new-resume-stack)
             (set-box! (current-resume-done?) #t))
           (begin0 (run-step req s the-step)
             (redirect/get/forget/protect)))]))))

(define (run-step req s the-step)
  (log-study-debug "run step ~e for participant ~s" (step-id the-step) (current-participant-id))
  (update-participant-progress! (step-id the-step))
  ;; This continuation is currently necessary because we need to be
  ;; able to return from a substudy once it is finished, without
  ;; generating an intermediate response, which s/s/d/p requires.
  ;;
  ;; Think twice before trying to get rid of this.
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

    [`(to-step ,to-step-id ,paramz)
     (call-with-parameterization
      paramz
      (lambda ()
        (goto-step s the-step to-step-id)))]

    [`(continue ,paramz)
     (call-with-parameterization
      paramz
      (lambda ()
        (goto-next-step s the-step)))]))

(define (goto-step the-study the-step to-step-id)
  (define req (redirect/get/forget/protect))
  (define next-step
    (study-find-step
     the-study to-step-id
     (lambda ()
       (error 'run-step "skipped to a nonexistent step: ~s~n  current step: ~.s~n  current study: ~.s" to-step-id the-step the-study))))
  (run-step req the-study next-step))

(define (goto-next-step the-study the-step)
  ;; Forget here to prevent users from going back and re-running the transition.
  (redirect/get/forget/protect)
  (log-study-debug "running transition for step~n  id: ~e~n  study: ~e~n  resume stack: ~e~n  participant-id: ~s"
                   (step-id the-step)
                   (study-name the-study)
                   (current-resume-stack)
                   (current-participant-id))
  (define next-step
    (match ((step-transition the-step))
      [(? done?) #f]
      [(? next?) (study-find-next-step the-study (step-id the-step))]
      [next-step-id (study-find-step
                     the-study next-step-id
                     (lambda ()
                       (error 'run-step "transitioned to a nonexistent step: ~.s~n  current step: ~.s~n  current study: ~.s" next-step-id (step-id the-step) the-study)))]))

  ;; Forget here to prevent refreshing from re-running the transition.
  (define req (redirect/get/forget/protect))
  (cond
    [next-step => (lambda (the-next-step)
                    (run-step req the-study the-next-step))]
    [else
     (for/hasheq ([id (in-list (study-provides the-study))])
       (values id (get id (λ () (error 'run-study "study did not 'put' provided variable: ~s" id)))))]))

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

(define/contract (view-study s req route reverse-uri-proc)
  (-> study? request? (listof string?) (-> string? string?) response?)
  (define-values (target stack)
    (let loop ([target s]
               [stack '(*root*)]
               [route route])
      (cond
        [(null? route)
         (values target stack)]
        [else
         (define id (string->symbol (car route)))
         (define st (study-find-step target id))
         (unless st
           (next-dispatcher))
         (cond
           [(step/study? st)
            (define next-stack (cons id stack))
            (define next-route (cdr route))
            (define the-substudy
              (if (procedure? (step/study-study st))
                  (parameterize ([current-study-stack stack])
                    ((step/study-study st)))
                  (step/study-study st)))
            (loop the-substudy next-stack next-route)]
           [else
            (values st stack)])])))
  (define hdl
    (cond
      [(study? target)
       (or (study-view-handler target)
           (make-default-study-view target))]
      [(step? target)
       (or (step-view-handler target)
           (make-default-step-view target))]
      [else
       (raise-argument-error 'view-study "(or/c study? step?)" target)]))
  (parameterize ([current-study-stack stack]
                 [current-view-reverse-uri-proc reverse-uri-proc])
    (hdl req)))

(define current-view-reverse-uri-proc
  (make-parameter #f))

(define ((make-default-study-view s) _req)
  (response/xexpr*
   ((current-xexpr-wrapper)
    (haml
     (.container
      (:h1 (study-name s))
      (:ul
       ,@(for/list ([child (in-list (study-steps s))])
           (define route (symbol->string (step-id child)))
           (haml
            (:li
             (:a
              ([:href ((current-view-reverse-uri-proc) route)])
              route))))))))))

(define ((make-default-step-view s) _req)
  (response/xexpr*
   ((current-xexpr-wrapper)
    (haml
     (.container
      (:h1 (symbol->string (step-id s))))))))


;; db ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (schema-out researcher&instance)
 (schema-out study-meta)
 (schema-out study-instance)
 (schema-out study-instance-var)
 (schema-out study-participant)
 (schema-out study-participant/admin)
 (schema-out study-var)
 (schema-out study-instance-link)
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
 list-active-study-instances/by-owner
 list-study-instance-participants/admin
 list-study-instance-vars
 clear-study-instance-data!
 clear-study-instance-data-for-user!
 clear-study-instance-vars!
 bulk-archive-study-instances!
 current-participant-id
 lookup-participant-email
 lookup-participant-identity-url
 current-participant-owner?
 current-participant-identity-user?
 current-study-instance-id
 current-study-instance-name
 current-study-instance-slug
 clear-participant-progress!
 participant-enrolled?
 enroll-participant!
 lookup-study
 lookup-study*
 lookup-study-meta
 lookup-study-meta/by-slug
 lookup-study-instance
 lookup-study-instance/by-slug
 lookup-study-instance-for-researcher
 lookup-study-participant/admin
 lookup-study-participant/by-id
 lookup-study-vars)

(define-schema study-meta
  #:table "studies"
  ([id id/f #:primary-key #:auto-increment]
   [owner-id id/f]
   [name string/f #:contract non-empty-string?]
   [(type 'racket) symbol/f #:contract (or/c 'racket 'dsl)]
   [slug string/f #:contract non-empty-string?]
   [racket-id symbol/f]
   [(dsl-source "") string/f #:contract string?]
   [(dsl-archive-path sql-null) string/f #:contract string? #:nullable]
   [(created-at (now/moment)) datetime-tz/f]))

(define-schema study-instance
  #:table "study_instances"
  ([id id/f #:primary-key #:auto-increment]
   [study-id id/f]
   [owner-id id/f]
   [name string/f #:contract non-empty-string?]
   [slug string/f #:contract non-empty-string?]
   [(enrollment-code (generate-random-string 16)) string/f]
   [(status 'active) symbol/f #:contract (or/c 'active 'inactive 'archived)]
   [(created-at (now/moment)) datetime-tz/f]))

(define-schema researcher&instance
  #:virtual
  ([researcher-id id/f]
   [researcher-username string/f]
   [instance-id id/f]
   [instance-name string/f]
   [created-at datetime-tz/f]))

(define-schema study-instance-var
  #:table "study_instance_data"
  ([instance-id id/f]
   [stack (array/f string/f) #:name "study_stack"]
   [id symbol/f #:name "key"]
   [value binary/f]
   [git-sha string/f]
   [first-put-at datetime-tz/f]
   [last-put-at datetime-tz/f])
  #:methods gen:jsexprable
  [(define/generic ->jsexpr/super ->jsexpr)
   (define (->jsexpr v)
     (match-define (study-instance-var _meta _ stack id value git-sha first-put-at last-put-at) v)
     (hash 'stack (vector->list stack)
           'id (symbol->string id)
           'value (->jsexpr/super (study-instance-var-value/deserialized v))
           'git-sha git-sha
           'first-put-at (moment->iso8601 first-put-at)
           'last-put-at (moment->iso8601 last-put-at)))])

(define-schema study-participant
  #:table "study_participants"
  ([id id/f #:primary-key #:auto-increment]
   [user-id id/f]
   [instance-id id/f]
   [(progress #()) (array/f string/f)]
   [(enrolled-at (now/moment)) datetime-tz/f]))

(define-schema study-participant/admin
  #:virtual
  ([id id/f]
   [user-id id/f]
   [instance-id id/f]
   [email string/f]
   [roles (array/f symbol/f)]
   [progress (array/f string/f)]
   [enrolled-at datetime-tz/f]))

(define-schema study-var
  #:virtual
  ([stack (array/f string/f)]
   [round-stack (array/f string/f)]
   [group-stack (array/f string/f)]
   [id symbol/f]
   [value binary/f]
   [git-sha string/f]
   [first-put-at datetime-tz/f]
   [last-put-at datetime-tz/f])
  #:methods gen:jsexprable
  [(define/generic ->jsexpr/super ->jsexpr)
   (define (->jsexpr v)
     (match-define (study-var _ stack round-stack group-stack id _ git-sha first-put-at last-put-at) v)
     (hash 'stack (vector->list stack)
           'round (vector->list round-stack)
           'group (vector->list group-stack)
           'id (symbol->string id)
           'value (->jsexpr/super (study-var-value/deserialized v))
           'git-sha git-sha
           'first-put-at (moment->iso8601 first-put-at)
           'last-put-at (moment->iso8601 last-put-at)))])

(define-schema study-instance-link
  #:table "study_instance_links"
  ([study-instance-id-a id/f]
   [study-instance-id-b id/f]
   [pseudonym-b symbol/f]
   [relationship symbol/f #:contract (or/c 'source 'reporter)]))

(define study-instance-var-value/deserialized
  (compose1 deserialize* study-instance-var-value))

(define study-var-value/deserialized
  (compose1 deserialize* study-var-value))

(struct study-manager ([participant #:mutable] db)
  #:transparent)

(module+ private
  (provide
   current-study-manager
   (struct-out study-manager)))

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

(define (current-study-instance-slug)
  (study-instance-slug
   (lookup-study-instance (current-database) (current-study-instance-id))))

(define (current-study-instance-owner-id)
  (study-instance-owner-id
   (lookup-study-instance (current-database) (current-study-instance-id))))

(define current-participant-user-id
  (compose1 study-participant-user-id current-participant))

(define (current-participant-owner?)
  (equal? (current-participant-user-id) (current-study-instance-owner-id)))

(define (current-participant-identity-user?)
  (lookup-participant-identity-url (current-participant-user-id)))

(define/contract (list-studies db #:owner [owner-id #f])
  (->* (database?)
       (#:owner (or/c #f id/c))
       (listof study-meta?))
  (define q
    (~> (from study-meta #:as s)
        (order-by ([s.created-at #:desc]))))
  (with-database-connection [conn db]
    (sequence->list
     (in-entities conn (if owner-id (~> q (where (= s.owner-id ,owner-id))) q)))))

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

(define/contract (list-active-study-instances/by-owner db owner-id)
  (-> database? id/c (listof study-instance?))
  (with-database-connection [conn db]
    (sequence->list
     (in-entities conn (~> (from study-instance #:as i)
                           (where (and (= i.status "active")
                                       (= i.owner-id ,owner-id)))
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
          (select p.id u.id p.instance-id u.username u.roles p.progress p.enrolled-at)
          (project-onto study-participant/admin-schema)))

    (sequence->list
     (in-entities conn (if for-bot-set
                           (where q (= u.bot-set-id ,for-bot-set))
                           (where q (or (not (array-contains? u.roles (array "bot")))
                                        (and (array-contains? u.roles (array "bot"))
                                             (is u.bot-set-id null)))))))))

(define/contract (clear-study-instance-data! db instance-id)
  (-> database? id/c void?)
  (with-database-transaction [conn db]
    (clear-study-instance-vars! db instance-id)
    (~> (from "study_data" #:as d)
        (where (in d.participant-id (subquery
                                     (~> (from "study_participants" #:as p)
                                         (where (= p.instance-id ,instance-id))
                                         (select p.id)))))
        (delete)
        (query-exec conn _))
    (~> (from "study_participants" #:as p)
        (where (= p.instance-id ,instance-id))
        (delete)
        (query-exec conn _))))

(define/contract (clear-study-instance-data-for-user! db instance-id user-id)
  (-> database? id/c id/c void?)
  (with-database-transaction [conn db]
    (~> (from "study_data" #:as d)
        (where (= d.participant-id (subquery
                                    (~> (from "study_participants" #:as p)
                                        (where (and (= p.instance-id ,instance-id)
                                                    (= p.user-id ,user-id)))
                                        (select p.id)))))
        (delete)
        (query-exec conn _))
    (~> (from "study_participants" #:as p)
        (where (= p.instance-id ,instance-id))
        (delete)
        (query-exec conn _))))

(define/contract (clear-study-instance-vars! db instance-id)
  (-> database? id/c void?)
  (with-database-connection [conn db]
    (~> (from study-instance-var #:as v)
        (where (= v.instance-id ,instance-id))
        (delete)
        (query-exec conn _))))

(define/contract (bulk-archive-study-instances! db owner-id instance-ids)
  (-> database? id/c (listof id/c) void?)
  (with-database-connection [conn db]
    (query-exec conn (~> (from study-instance #:as i)
                         (where (and (= i.owner-id ,owner-id)
                                     (in i.id ,@instance-ids)))
                         (update [status "archived"])))))

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

            (and participant (list (lookup-study* meta) participant)))]

      [else #f])))

(define/contract (lookup-study* meta)
  (-> study-meta? study?)
  (case (study-meta-type meta)
    [(racket)
     (lookup-registered-study
      (study-meta-racket-id meta)
      (lambda (id)
        (error 'lookup-registered-study "No such registered study: ~s~n. Did you install the necessary congame-studies?" id)))]
    [(dsl)
     (if (sql-null? (study-meta-dsl-archive-path meta))
         (dsl-require
          (study-meta-dsl-source meta)
          (study-meta-racket-id meta))
         (dsl-require
          `(archive ,(study-meta-dsl-archive-path meta))
          (study-meta-racket-id meta)))]))

(define/contract (lookup-study-meta db study-id)
  (-> database? id/c (or/c #f study-meta?))
  (with-database-connection [conn db]
    (lookup conn (~> (from study-meta #:as s)
                     (where (= s.id ,study-id))))))

(define/contract (lookup-study-meta/by-slug db slug)
  (-> database? string? (or/c #f study-meta?))
  (with-database-connection [conn db]
    (lookup conn (~> (from study-meta #:as s)
                     (where (= s.slug ,slug))))))

(define/contract (lookup-study-instance db instance-id)
  (-> database? id/c (or/c #f study-instance?))
  (with-database-connection [conn db]
    (lookup conn (~> (from study-instance #:as i)
                     (where (= i.id ,instance-id))))))

(define/contract (lookup-study-instance/by-slug db slug)
  (-> database? string? (or/c #f study-instance?))
  (with-database-connection [conn db]
    (lookup conn (~> (from study-instance #:as i)
                     (where (= i.slug ,slug))))))

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
                     (select p.id u.id p.instance-id u.username u.roles p.progress p.enrolled-at)
                     (project-onto study-participant/admin-schema)))))

(define/contract (lookup-study-participant/by-id db participant-id)
  (-> database? id/c (or/c #f study-participant?))
  (with-database-connection [conn db]
    (lookup conn (~> (from study-participant #:as p)
                     (where (= p.id ,participant-id))))))

(define/contract (lookup-participant-email pid)
  (-> id/c string?)
  (define email
    (with-database-connection [conn (study-manager-db (current-study-manager))]
      (query-value
       conn
       (~> (from "study_participants" #:as p)
           (join "users" #:as u #:on (= u.id p.user-id))
           (where (and (= p.id ,pid)
                       (= p.instance-id ,(current-study-instance-id))))
           (select u.username)))))
  (unless email
    (error 'lookup-participant-email
           "pid ~a is not a member of study instance ~a"
           pid (current-study-instance-id)))
  email)

(define/contract (lookup-participant-identity-url pid)
  (-> id/c (or/c #f string?))
  (with-database-connection [conn (study-manager-db (current-study-manager))]
    (define r
      (query-value
     conn
     (~> (from "users" #:as u)
         (join "study_participants" #:as p #:on (= u.id p.user-id))
         (where (= p.id ,pid))
         (select u.identity-service-url))))
    (if (sql-null? r) #f r)))

(define/contract (lookup-study-vars db participant-id)
  (-> database? id/c (listof study-var?))
  (with-database-connection [conn db]
    (sequence->list
     (in-entities conn (~> (from "study_data" #:as d)
                           (select d.study-stack d.round-stack d.group-stack d.key d.value d.git-sha d.first-put-at d.last-put-at)
                           (project-onto study-var-schema)
                           (where (= d.participant-id ,participant-id))
                           (order-by ([d.first-put-at #:asc])))))))

(define/contract (clear-participant-progress! db participant-id)
  (-> database? id/c void?)
  (with-database-transaction [conn db]
    (query-exec conn (~> (from "study_participants" #:as p)
                         (where (= p.id ,participant-id))
                         (update [progress ,(list->pg-array null)])))
    (query-exec conn (~> (from "study_data" #:as d)
                         (where (= d.participant-id ,participant-id))
                         (delete)))
    #;(query-exec conn (~> (from "payments" #:as p)
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

(module+ accessors
  (provide
   get*
   put*
   get/instance*
   put/instance*
   make-get/root
   make-put/root)

  (define-syntax-rule (define-top-proc id proc-id)
    (define id
      (procedure-rename
       (make-keyword-procedure
        (lambda (kws kw-args . args)
          (parameterize ([current-study-stack '(*root*)])
            (keyword-apply proc-id kws kw-args args))))
       'id)))
  (define-syntax-rule (define-top-procs [id proc-id] ...)
    (begin (define-top-proc id proc-id) ...))

  (define-top-procs
    [get* get]
    [put* put]
    [get/instance* get/instance]
    [put/instance* put/instance])

  (define (make-get/root getter root-id)
    (make-keyword-procedure
     (lambda (kws kw-args . args)
       (keyword-apply getter kws kw-args args #:root root-id))))

  (define (make-put/root putter root-id)
    (make-keyword-procedure
     (lambda (kws kw-args . args)
       (keyword-apply putter kws kw-args args #:root root-id)))))
