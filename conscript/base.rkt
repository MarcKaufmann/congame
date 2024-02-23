#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         (prefix-in bot: congame/components/bot-maker)
         (prefix-in bot: (submod congame/components/bot actions))
         (prefix-in congame: congame/components/struct)
         (prefix-in congame: congame/components/study)
         (prefix-in congame: (submod congame/components/study accessors))
         (except-in congame/components/study button form)
         congame/components/for-study
         congame/components/transition-graph
         net/url
         racket/contract/base
         racket/format
         racket/lazy-require
         racket/list
         "form.rkt"
         "html.rkt"
         "markdown.rkt"
         "var.rkt")

(lazy-require
 [congame-web/components/identity (put/identity)])

;; kernel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 ;; Racket Syntax
 #%app
 #%datum
 #%module-begin
 #%top
 #%top-interaction
 apply
 define
 quote
 quasiquote
 unquote
 unquote-splicing
 provide

 if case cond else unless when
 set!

 ;; Racket Runtime
 lambda λ
 void sleep
 for for/list in-range in-inclusive-range in-list
 list list? list* null null? cons pair? car cdr map for-each shuffle
 display displayln print println printf eprintf write writeln
 + - * / modulo quotient remainder add1 sub1 abs max min round floor ceiling
 = < > <= >= equal? eq?
 sqrt expt exp log
 regexp-match?
 format ~a number->string symbol->string

 ;; Congame Syntax
 --> goto for/study

 ;; Congame Runtime
 (all-from-out
  congame/components/bot-maker
  (submod congame/components/bot actions))
 (rename-out
  [congame:with-study-transaction with-study-transaction]
  [congame:get/linked/instance get/linked/instance]
  [congame:get/instance get/instance]
  [congame:get* get*]
  [congame:put/instance put/instance]
  [congame:put* put*])
 make-step make-step/study
 get put put/identity
 done
 )

;; syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (rename-out [conscript-require require])
 (all-from-out "form.rkt")
 (all-from-out "html.rkt")
 (all-from-out "markdown.rkt")
 defstep
 defstep/study
 defstudy
 defvar)

(begin-for-syntax
  ;; Be careful what you provide here. Avoid providing access to
  ;; things that might allow the user to "escape" #lang conscript, eg.
  ;; racket/system, ffi/unsafe or any system-level functionality.
  (define whitelist
    '(racket/format
      racket/list
      racket/match
      racket/math
      racket/random
      racket/string
      racket/vector))
  (define (check-module-whitelisted mod-stx)
    (unless (memq (syntax->datum mod-stx) whitelist)
      (raise-syntax-error 'require "required module not whitelisted" mod-stx))))

(define-syntax (conscript-require stx)
  (syntax-parse stx
    [(_ mod:id ...+)
     (for-each check-module-whitelisted (syntax-e #'(mod ...)))
     #'(require mod ...)]))

(begin-for-syntax
  (define-splicing-syntax-class function-arg
    {pattern arg:id}
    {pattern [arg:id default-expr:expr]}
    {pattern {~seq kwd:keyword arg:id}}
    {pattern {~seq kwd:keyword [arg:id default-expr:expr]}})
  (define-syntax-class function-header
    {pattern (nested:function-header arg:function-arg ...) #:with name 'nested.name}
    {pattern (name:id arg:function-arg ...)}
    {pattern (name:id . args-id:id)}))

(define-syntax (defstep stx)
  (syntax-parse stx
    [(_ (name:id) #:bot bot-expr . body)
     #'(define name
         (make-step
          'name (lambda () . body)
          #:for-bot bot-expr))]
    [(_ head:function-header . body)
     #'(define head . body)]))

(define-syntax (defstep/study stx)
  (syntax-parse stx
    [(_ id:id
        {~alt
         {~seq #:study study-expr}
         {~optional {~seq #:require-bindings (require-binder ...)}}
         {~optional {~seq #:provide-bindings (provide-binder ...)}}} ...)
     #'(define id
         (make-step/study
          #:require-bindings `(require-binder ...)
          #:provide-bindings `(provide-binder ...)
          'id study-expr ...))]))

(begin-for-syntax
  ;; Extends the regular transition graph syntax with support for
  ;; binders in the middle of a transition. Rewrites binders to drop the
  ;; rhs before passing the stx to `transition-graph' (via `tg-e').
  (define-syntax-class transition-arrow-expr
    #:literals (--> unquote)
    {pattern (unquote e)
             #:with (tg-e ...) #'((unquote e))
             #:with ((step-id step-e) ...) #'()}
    {pattern step:id
             #:with (tg-e ...) #'(step)
             #:with ((step-id step-e) ...) #'((step step))}
    {pattern [step:id step-expr:expr]
             #:with (tg-e ...) #'(step)
             #:with ((step-id step-e) ...) #'((step step-expr))})

  (define-splicing-syntax-class transition-arrow
    {pattern e:transition-arrow-expr
             #:with (tg-e ...) #'{e.tg-e ...}
             #:with ((step-id step-e) ...) #'((e.step-id e.step-e) ...)}
    {pattern {~seq lhs:transition-arrow-expr --> rhs:transition-arrow}
             #:with (tg-e ...) #'{lhs.tg-e ... --> rhs.tg-e ...}
             #:with ((step-id step-e) ...) #'((lhs.step-id lhs.step-e) ... (rhs.step-id rhs.step-e) ...)})

  (define-syntax-class transition
    {pattern [arrow:transition-arrow]
             #:with tg-e #'(arrow.tg-e ...)
             #:with ((step-id step-e) ...) #'((arrow.step-id arrow.step-e) ...)}))

(define-syntax (defstudy stx)
  (syntax-parse stx
    [(_ id:id
        {~alt
         {~optional {~seq #:requires requires}}
         {~optional {~seq #:provides provides}}} ...
        transition-e:transition ...+)
     #:with ((step-id step-expr) ...)
     (for/fold ([stxs null]
                [seen (hash)]
                #:result (reverse stxs))
               ([step-id-stx (in-list (syntax-e #'(transition-e.step-id ... ...)))]
                [step-expr-stx (in-list (syntax-e #'(transition-e.step-e ... ...)))])
       (define step-id
         (syntax->datum step-id-stx))
       (define binder?
         (not (eq? step-id-stx step-expr-stx)))
       (define seen?
         (hash-ref seen step-id #f))
       (define seen-binder?
         (eq? seen? 'binder))
       (when (and binder? seen-binder?)
         (with-syntax ([step-id step-id-stx]
                       [step-expr step-expr-stx])
           (define binder-stx
             (syntax/loc step-id-stx
               [step-id step-expr]))
           (raise-syntax-error 'defstudy "step already has a binding expression" stx binder-stx)))
       (values
        (if seen? stxs (cons (list step-id-stx step-expr-stx) stxs))
        (hash-set
         seen step-id
         (cond
           [seen-binder? 'binder]
           [binder? 'binder]
           [else 'id]))))
     #'(define id
         (make-study
          (symbol->string 'id)
          #:requires {~? requires null}
          #:provides {~? provides null}
          #:transitions
          (transition-graph
           transition-e.tg-e ...)
          (list (make-step* 'step-id step-expr) ...)))]))

(define (make-step* id v)
  (cond
    [(step/study? v)
     (struct-copy congame:step/study v [id #:parent congame:step id])]
    [(step? v)
     (struct-copy congame:step v [id id])]
    [(study? v)
     (make-step/study id v)]
    [else
     (make-step id v)]))

(define-syntax (defvar stx)
  (syntax-parse stx
    [(_ id:id unique-id:id)
     #`(begin
         (ensure-var-id-is-unique! #,(syntax-source stx) 'unique-id)
         (define-syntax id
           (make-set!-transformer
            (lambda (stx)
              (syntax-case stx (set!)
                [(set! id v) #'(put-var 'unique-id 'id v)]
                [id (identifier? #'id) #'(get-var 'unique-id 'id)])))))]))

(define (put-var uid k v)
  (parameterize ([current-study-stack null])
    (put #:root (string->symbol (format "*dynamic:~a*" uid)) k v)))

(define (get-var uid k)
  (parameterize ([current-study-stack null])
    (get #:root (string->symbol (format "*dynamic:~a*" uid)) k)))


;; util ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [~url (->* [string?]
             [#:params (listof (cons/c symbol? string?))]
             string?)]))

(define (~url urlish #:params [params null])
  (define u
    (string->url
     (if (not (regexp-match? #rx"^[^:]+?://" urlish))
         (string-append "https://" urlish)
         urlish)))
  (url->string (struct-copy url u [query params])))


;; widgets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 button)

(define button
  (make-keyword-procedure
   (lambda (kws kw-args action-or-label . args)
     (define-values (action label args*)
       (if (null? args)
           (values void action-or-label null)
           (values action-or-label (car args) (cdr args))))
     (keyword-apply congame:button kws kw-args action label args*))))


;; functionality for students - provide elsewhere? ;;;;;;;;;;;;;;;;;;;;;

(provide
 assigning-treatments)

(define (assigning-treatments
         treatments
         #:treatments-key [treatments-key 'treatments]
         #:role-key [role-key 'role])
  (unless (congame:get* role-key #f)
    (with-study-transaction
      (when (empty? (congame:get/instance* treatments-key '()))
        (congame:put/instance* treatments-key (shuffle treatments)))
      (define remaining-treatments
        (congame:get/instance* treatments-key))
      (define role
        (first remaining-treatments))
      (congame:put* role-key role)
      (define updated-treatments
        (rest remaining-treatments))
      (congame:put/instance* treatments-key updated-treatments))))
