#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(define (make-element id required-kwds)
  (make-keyword-procedure
   (lambda (kws kw-args . args)
     (for ([kwd (in-list required-kwds)])
       (unless (memq kwd kws)
         (error id "missing required keyword argument: ~a" kwd)))
     (define attributes
       (for/list ([kw (in-list kws)]
                  [kw-arg (in-list kw-args)])
         `(,(string->symbol (keyword->string kw)) ,kw-arg)))
     `(,id ,attributes ,@args))))

(define-syntax (define-element stx)
  (syntax-parse stx
    [(_ (id:id {~optional (required-kwd:keyword ...)}))
     #'(begin
         (provide id)
         (define id (make-element 'id {~? '(required-kwd ...) null})))]))

(define-syntax (define-elements stx)
  (syntax-parse stx
    [(_ def ...+)
     #'(begin (define-element def) ...)]))

(define-elements
  [a (#:href)]
  [aside]
  [audio (#:src)]
  [blockquote]
  [br]
  [dd]
  [div]
  [dt]
  [em]
  [h1]
  [h2]
  [h3]
  [h4]
  [h5]
  [h6]
  [img (#:alt #:src)]
  [label]
  [li]
  [ol]
  [p]
  [section]
  [span]
  [strong]
  [table]
  [tbody]
  [td]
  [th]
  [thead]
  [tr]
  [u]
  [ul]
  [video (#:src)])
