#lang racket/base

(require (for-syntax racket/base
                     racket/runtime-path
                     syntax/parse/pre)
         (prefix-in congame: congame/components/struct)
         (prefix-in congame: congame/components/study)
         (except-in congame/components/study button form)
         congame/components/transition-graph
         "form.rkt"
         "html.rkt"
         "var.rkt")

;; kernel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 ;; Racket Syntax
 #%app
 #%datum
 #%module-begin
 #%top
 #%top-interaction
 define
 quote
 quasiquote
 unquote
 provide

 if case cond else unless when
 set!

 ;; Racket Runtime
 lambda λ
 void
 list list? null null? cons pair? car cdr
 display displayln print println printf eprintf write writeln

 ;; Congame Syntax
 --> goto

 ;; Congame Runtime
 make-step make-step/study
 get put
 done
 )

;; syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (rename-out [conscript-require require])
 (all-from-out "form.rkt")
 (all-from-out "html.rkt")
 defstep
 defstep/study
 defstudy
 defvar)

(begin-for-syntax
  (define-runtime-path module-whitelist.rktd
    "module-whitelist.rktd")
  (define whitelist
    (call-with-input-file module-whitelist.rktd read))
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
    {pattern (nested:function-header arg:function-arg ...)}
    {pattern (name:id arg:function-arg ...)}
    {pattern (name:id . args-id:id)}))

(define-syntax (defstep stx)
  (syntax-parse stx
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
  (define-splicing-syntax-class transition-arrow
    #:literals (--> unquote)
    {pattern (unquote e)
             #:with (tg-e ...) #'((unquote e))
             #:with ((step-id step-e) ...) #'()}
    {pattern step:id
             #:with (tg-e ...) #'(step)
             #:with ((step-id step-e) ...) #'((step step))}
    {pattern [step:id step-expr:id]
             #:with (tg-e ...) #'(step)
             #:with ((step-id step-e) ...) #'((step step-expr))}
    {pattern {~seq lhs:transition-arrow --> rhs:transition-arrow}
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
       (if (hash-has-key? seen step-id)
           (values stxs seen)
           (values (cons (list step-id-stx step-expr-stx) stxs)
                   (hash-set seen step-id #t))))
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


;; widgets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 button)

(define button
  (case-lambda
    [(label) (congame:button void label)]
    [args (apply congame:button args)]))
