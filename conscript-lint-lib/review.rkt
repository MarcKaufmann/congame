#lang racket/base

(require review/ext
         syntax/parse/pre)

#|review: ignore|#

(provide
 should-review-syntax?
 review-syntax)

(define (should-review-syntax? stx)
  (syntax-parse stx
    [({~datum defstep} . _rest) #t]
    [({~datum defstuy} . _rest) #t]
    [({~datum defvar} . _rest) #t]
    [_ #f]))

(define-syntax-class defstep-header
  (pattern (hdr:defstep-header arg-id:id ...)
           #:with id #'defstep-hdr.id)
  (pattern (id:id arg-id:id ...))
  (pattern id:id))

(define-syntax-class defstep
  #:datum-literals (defstep)
  (pattern (defstep
             {~do (push-scope)}
             header:defstep-header
             e:expression ...+
             {~do (pop-scope)})
           #:do [(track-binding #'header.id #:check-usages? #t)]))

(define-syntax-class defstudy
  #:datum-literals (defstudy)
  (pattern (defstudy id:id . _rest)
           #:do [(track-binding #'id #:check-usages? #t)]))

(define-syntax-class defvar
  #:datum-literals (defvar defvar* defvar/instance defvar*/instance)
  (pattern ({~or defvar defvar* defvar/instance defvar*/instance} id:id . _rest)
           #:do [(track-binding #'id #:check-usages? #t)]))

(define (review-syntax stx)
  (syntax-parse stx
    [s:defstep #'s]
    [S:defstudy #'S]
    [v:defvar #'v]
    [_ (track-error stx "expected a conscript form")]))
