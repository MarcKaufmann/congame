#lang racket/base

(require (for-syntax racket/base
                     syntax/for-body
                     syntax/parse)
         congame/components/study)

(provide
 for/study)

(define-syntax (for/study stx)
  (syntax-parse stx
    [(_ {~optional {~and #:substudies use-substudies?}}
        {~optional {~seq #:provides provides:expr}}
        clauses body-e ... tail-e)
     #:with ((pre-body-e ...)
             (post-body-e ...))
     (split-for-body stx #'(body-e ... tail-e))
     #`(do-for-study
        {~? {~@ #:provides provides}}
        #,(if (attribute use-substudies?)
              #'make-step/study
              #'make-step)
        (for/fold/derived #,stx
                          ([procs null] #:result (reverse procs))
                          clauses
          pre-body-e ...
          (cons (λ () post-body-e ...) procs)))]))

(define (do-for-study make-step-proc procs #:provides [provides '()])
  (make-study
   "for/study"
   #:provides provides
   (for/list ([(proc i) (in-indexed (in-list procs))])
     (make-step-proc (string->symbol (format "iter-~a" i)) proc))))
