#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         congame/components/study
         racket/match
         "html-element.rkt")

(provide
 (all-from-out "html-element.rkt")
 html)

(define-syntax (html stx)
  (syntax-parse stx
    [(_ elt ...)
     #'(page (html* elt ...))]))

(define-syntax (html* stx)
  (syntax-parse stx
    [(_ elt ...)
     #'(paragraphify `(div () ,elt ...))]))

(define (paragraphify? e)
  (memq (car e) '(aside div form section)))

(define (swallow? e)
  (match e
    [(? string?) #t]
    [`(,id ,_ . ,_)
     (memq id '(a blockquote button em img input label select span strong textarea u))]
    [_ #f]))

(define (paragraphify e)
  (cond
    [(paragraphify? e)
     (match-define `(,id ,attrs . ,content) e)
     (define (commit p-content c)
       (cons `(p () . ,(reverse p-content)) c))
     (define para-content
       (for/fold ([para-content null]
                  [paragraph null]
                  [newlines 0]
                  #:result (reverse
                            (if (null? paragraph)
                                para-content
                                (commit paragraph para-content))))
                 ([e (in-list content)])
         (cond
           [(swallow? e)
            (if (and (string? e)
                     (string=? "\n" e))
                (values para-content paragraph (add1 newlines))
                (if (> newlines 1)
                    (values (commit paragraph para-content) (list e) 0)
                    (if (null? paragraph)
                        (values para-content (list e) 0)
                        (values para-content (cons e (cons " " paragraph)) 0))))]
           [(null? paragraph)
            (values (cons (paragraphify e) para-content) null 0)]
           [else
            (values (cons (paragraphify e) (commit paragraph para-content)) null 0)])))
     `(,id ,attrs . ,para-content)]
    [else e]))

(module+ test
  (require rackunit)
  (define tests
    (list
     `(,(html*)
       (div ()))
     `(,(html* "hello")
       (div () (p () "hello")))
     `(,(html* "hello" "there")
       (div () (p () "hello" " " "there")))
     `(,(html*
         (h1 "Hi")
         "How's it going?")
       (div
        ()
        (h1 () "Hi")
        (p () "How's it going?")))
     `(,(html*
         "Hi"
         "\n"
         "there")
       (div
        ()
        (p () "Hi" " " "there")))
     `(,(html*
         "Hi"
         "\n"
         "\n"
         "there")
       (div
        ()
        (p () "Hi")
        (p () "there")))
     `(,(html*
         (h1 "Hello")
         "there"
         (h2 "How's it")
         "going"
         "\n"
         "\n"
         "well, I hope")
       (div
        ()
        (h1 () "Hello")
        (p () "there")
        (h2 () "How's it")
        (p () "going")
        (p () "well, I hope")))
     `(,(html*
         "Hello"
         "\n"
         "\n"
         "  "
         "There")
       (div
        ()
        (p () "Hello")
        (p () "  " " " "There")))
     `(,(html*
         (p "Hello")
         "World")
       (div
        ()
        (p () "Hello")
        (p () "World")))
     `(,(html*
         (p "Hello"
            "\n"
            "\n"
            "World"))
       (div
        ()
        (p () "Hello" "\n" "\n" "World")))
     `(,(html*
         "Hello"
         (div
          "There"
          "\n"
          "\n"
          "World"))
       (div
        ()
        (p () "Hello")
        (div
         ()
         (p () "There")
         (p () "World"))))))
  (for ([t (in-list tests)])
    (check-equal?
     (car t)
     (cadr t))))
