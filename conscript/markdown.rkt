#lang at-exp racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         commonmark
         congame/components/study
         racket/string
         xml)

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
  (define doc
    (string->document
     (string-join
      (for/list ([content (in-list all-content)])
        (if (string? content)
            content
            (xexpr->string content)))
      "")))
  `(div () ,@(document->xexprs doc)))

(module+ test
  (require koyo/haml
           racket/port
           rackunit)

  (check-equal?
   @md*{# Start

        Hello.

        @list['br]}
   `(div
     ()
     (h1 "Start")
     (p "Hello.")
     ,(cdata #f #f "<br/>")))

  (define name "Marc")
  (check-equal?
   @md*{# Start

        Hello @|name|.}
   '(div
     ()
     (h1 "Start")
     (p "Hello Marc.")))

  (define (button label)
    (haml
     (:button
      ([:type "submit"])
      label)))

  (check-equal?
   (call-with-output-string
    (lambda (out)
      (write-xexpr
       @md*{# Start

            Hello @|name|.

            @button{test}}
       out)))
   "<div><h1>Start</h1><p>Hello Marc.</p><p><button type=\"submit\">test</button></p></div>"))
