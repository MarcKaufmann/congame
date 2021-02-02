#lang racket/base

(require (for-syntax racket/base
                     racket/match
                     setup/getinfo
                     syntax/parse)
         congame/components/registry)

(define-syntax-rule (reprovide mod ...)
  (begin
    (require mod ...)
    (provide (all-from-out mod) ...)))

(begin-for-syntax
  (define study-descriptions
    (for*/list ([path (find-relevant-directories '(congame-studies))]
                [desc (in-list ((get-info/full path) 'congame-studies))])
      desc))

  (define bot-descriptions
    (for*/list ([path (find-relevant-directories '(congame-bots))]
                [desc (in-list ((get-info/full path) 'congame-bots))])
      desc)))

(define-syntax (comptime-require-studies stx)
  (syntax-parse stx
    [(_)
     #:with (req-spec ...) (for/list ([desc (in-list study-descriptions)])
                             #`(only-in #,(car desc) #,(cadr desc)))
     #:with (reg-spec ...) (for/list ([desc (in-list study-descriptions)])
                             (define id (cadr desc))
                             #`(register-study! (quote #,id) #,id))
     #'(begin
         (require req-spec ...)
         reg-spec ...)]))

(define-syntax (comptime-require-bots stx)
  (syntax-parse stx
    [(_)
     #:with (req-spec ...) (for/list ([desc (in-list bot-descriptions)])
                             (match-define `(,module-path ,bot-id #:for ,_ #:models (,model-ids ...)) desc)
                             #`(only-in #,module-path #,bot-id #,@model-ids))
     #:with (reg-spec ...) (for/list ([desc (in-list bot-descriptions)])
                             (match-define `(,_ ,bot-id #:for ,study-id #:models (,model-ids ...)) desc)
                             #`(register-bot! (quote #,bot-id) (quote #,study-id) #,bot-id (list #,@model-ids)))
     #'(begin
         (require req-spec ...)
         reg-spec ...)]))

(comptime-require-studies)
(comptime-require-bots)

(displayln (list "Bot registry: " (get-registered-bots)))
(flush-output)
