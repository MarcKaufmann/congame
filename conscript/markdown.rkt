#lang at-exp racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         commonmark
         congame/components/study)

(provide
 md
 md*)

(define-syntax (md stx)
  (syntax-parse stx
    [(_ content ...)
     #'(page
        `(div
          ([class "container"])
          ,(md* content ...)))]))

(define-syntax (md* stx)
  (syntax-parse stx
    [(_ content ...)
     #'(markdown->xexprs content ...)]))

(define (markdown->xexprs . all-content)
  `(div () ,@(apply append (for/list ([content (in-list all-content)])
                             (if (string? content)
                                 (document->xexprs (string->document content))
                                 (list content))))))

(module+ test
  (require rackunit)

  (check-equal?
   @md*{# Start

        Hello.

        @list['br]}
   '(div
     (h1 "Start")
     (p "Hello.")
     (br))))
