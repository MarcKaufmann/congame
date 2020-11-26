#lang racket/base

(require (for-syntax racket/base
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
      desc)))

(define-syntax (comptime-require-pkgs stx)
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

(comptime-require-pkgs)
