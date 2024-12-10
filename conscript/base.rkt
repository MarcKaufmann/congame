#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         (prefix-in bot: congame/components/bot)
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
         racket/random
         racket/string
         (only-in web-server/http response/xexpr)
         "form.rkt"
         "html.rkt"
         "markdown.rkt"
         "matchmaking.rkt"
         "resource.rkt"
         "var-box.rkt")

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
 define-values
 quote
 quasiquote
 unquote
 unquote-splicing
 provide

 begin begin0 let
 if case cond else unless when
 or and not
 set!
 values

 parameterize
 with-handlers

 ;; Racket Runtime
 lambda λ case-lambda
 void sleep
 for for/fold for/hash for/list in-range in-inclusive-range in-list
 for/and for/or for/sum for/product
 list list? list* null null? cons pair? car cdr map member for-each shuffle length findf filter rest reverse
 first second third fourth fifth sixth seventh eighth ninth tenth
 display displayln print println printf eprintf write writeln
 current-seconds

 in-hash
 hash hash-count hash-ref hash-remove hash-set hash-update hash-values hash-keys

 + - * / modulo quotient remainder add1 sub1 abs max min round floor ceiling truncate
 = < > <= >= equal? eq?
 sqrt expt exp log random random-ref

 regexp-match?

 format ~a string number->string string->number symbol->string string->symbol string->list string->bytes/utf-8 string-upcase string-downcase string-titlecase string=? string>=? string<=? string>? string<?
 string-join
 bytes? bytes->string/utf-8 char?


 exn:fail? exn-message

 ;; Congame Syntax
 --> goto for/study

 ;; Congame Runtime
 (all-from-out
  congame/components/bot-maker
  (submod congame/components/bot actions))
 (rename-out
  [bot:current-user-bot? current-user-bot?]
  [congame:current-participant-id current-participant-id]
  [congame:current-participant-owner? current-participant-owner?]
  [congame:current-participant-identity-user? current-participant-identity-user?]
  [congame:call-with-study-transaction call-with-study-transaction]
  [congame:with-study-transaction with-study-transaction]
  [congame:get/linked/instance get/linked/instance]
  [congame:skip skip]
  [congame:with-namespace with-namespace]
  [congame:defvar defvar]
  [congame:defvar* defvar*]
  [congame:defvar/instance defvar/instance]
  [congame:defvar*/instance defvar*/instance]
  [congame:undefined undefined]
  [congame:undefined? undefined?]
  [congame:if-undefined if-undefined]
  [congame:get-current-group-name get-current-group-name]
  [congame:put-current-group-name put-current-group-name]
  [congame:map-step map-step]
  [congame:map-study map-study])
 make-step make-step/study
 put/identity
 done

 ;; Conscript Syntax
 define-var-box

 ;; Matchmaking
 make-matchmaker
 get-ready-groups
 get-current-group
 reset-current-group)

;; syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (rename-out [conscript-require require])
 only-in prefix-in rename-in

 (all-from-out "form.rkt")
 (all-from-out "html.rkt")
 (all-from-out "markdown.rkt")
 (all-from-out "resource.rkt")
 defstep
 defstep/study
 defview
 defstudy
 with-bot)

(begin-for-syntax
  ;; Be careful what you provide here. Avoid providing access to
  ;; things that might allow the user to "escape" #lang conscript, eg.
  ;; racket/system, ffi/unsafe or any system-level functionality.
  (define whitelist
    '(buid
      congame-web/components/study-bot
      congame-web/components/uploaded-file
      conscript/matchmaking
      conscript/survey-tools
      data/monocle
      gregor
      hash-view
      koyo/haml
      racket/format
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
  (define-syntax-class mod
    #:literals (only-in prefix-in rename-in)
    (pattern id:id)
    (pattern (only-in id:id bind ...))
    (pattern (prefix-in p:id id:id))
    (pattern (rename-in id:id bind ...)))

  (syntax-parse stx
    [(_ m:mod ...+)
     (for-each check-module-whitelisted (syntax-e #'(m.id ...)))
     #'(require m ...)]))

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
    [(_ name:id body:expr)
     #'(define name body)]
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

(define-syntax (defview stx)
  (syntax-parse stx
    [(_ (name:id req-name:id) body ...+)
     #'(define (name req-name)
         (define responder
           (let () body ...))
         (response/xexpr
          (responder)))]))

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

(define-syntax (with-bot stx)
  (syntax-parse stx
    [(_ step-expr:expr bot-expr:expr)
     #'(make-step 'anon step-expr #:for-bot bot-expr)]))

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


;; util ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [~current-view-uri (-> string?)]
  [~url (->* [string?]
             [#:params (listof (cons/c symbol? string?))]
             string?)]))

(define (~current-view-uri)
  (format "/study/~a/view/~a"
          (current-study-instance-slug)
          (string-join
           (for/list ([id (in-list (append
                                    (cdr (current-study-stack))
                                    (list (step-id (current-step)))))])
             (symbol->string id))
           "/")))

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
