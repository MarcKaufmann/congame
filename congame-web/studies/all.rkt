#lang racket/base

(require (for-syntax racket/base
                     racket/match
                     setup/getinfo
                     syntax/parse)
         congame/components/registry
         racket/promise
         racket/runtime-path)

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
     #:with (mod-path ...) (for/list ([desc (in-list study-descriptions)])
                             (datum->syntax stx (car desc)))
     #:with (mod-id ...) (generate-temporaries #'(mod-path ...))
     #:with (study-id ...) (for/list ([desc (in-list study-descriptions)])
                             (datum->syntax stx (cadr desc)))
     #:with (reg-spec ...) (for/list ([desc (in-list study-descriptions)])
                             (define id (cadr desc))
                             #`(register-study! (quote #,id) #,id))
     #'(begin
         (begin
           (define-runtime-module-path-index mod-id 'mod-path)
           (define study-id (delay (dynamic-require mod-id 'study-id)))) ...

         reg-spec ...)]))

(define-syntax (comptime-require-bots stx)
  (syntax-parse stx
    [(_)
     #:with (mod-path ...) (for/list ([desc (in-list bot-descriptions)])
                             (datum->syntax stx (car desc)))
     #:with (mod-id ...) (generate-temporaries #'(mod-path ...))
     #:with ((study-id bot-id model-id ...) ...)
     (for/list ([desc (in-list bot-descriptions)])
       (match-define `(,_ ,bot-id #:for ,study-id #:models (,model-ids ...)) desc)
       #`(#,study-id #,bot-id #,@model-ids))
     #:with (reg-spec ...) (for/list ([desc (in-list bot-descriptions)])
                             (match-define `(,_ ,bot-id #:for ,study-id #:models ,_) desc)
                             #`(register-bot! (quote #,bot-id) (quote #,study-id) #,bot-id))
     #'(begin
         (begin
           (define-runtime-module-path-index mod-id 'mod-path)
           (define bot-id (delay
                            (let ([local-mod-id mod-id])
                              (bot-info
                               'bot-id
                               (dynamic-require mod-id 'bot-id)
                               (make-hash
                                (list
                                 (cons 'model-id (dynamic-require local-mod-id 'model-id))) ...)))))) ...

         reg-spec ...)]))

(comptime-require-studies)
(comptime-require-bots)
