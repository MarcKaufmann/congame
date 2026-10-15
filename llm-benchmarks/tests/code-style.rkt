#lang racket/base

(require rackunit
         "../private/code-style.rkt")

(test-case "syntax spans preserve Unicode offsets and distinguish token kinds"
  (define source "(define λ 42) ; comment\n\"hello\"")
  (define spans (code-spans source))
  (define (color-of token)
    (for/first ([span (in-list spans)]
                #:when (equal? token (substring source (code-span-start span) (code-span-end span))))
      (code-span-color span)))
  (check-not-false (color-of "define"))
  (check-not-false (color-of "λ"))
  (check-not-false (color-of "42"))
  (check-not-false (color-of "\"hello\""))
  (check-not-equal? (color-of "define") (color-of "\"hello\""))
  (check-not-equal? (color-of "42") (color-of "; comment")))

(test-case "incomplete submissions still have a highlightable preview"
  (check-not-exn (lambda () (code-spans "#lang conscript\n(defstep (intro) @md{Hello @name}")))
  (check-not-exn (lambda () (code-spans "(define value \"unterminated")))
  (check-equal? (code-spans "") null))

(test-case "Conscript prose is not mistaken for invalid Racket tokens"
  (define source "#lang conscript\n(defstep (intro) @md{# Hello @name})")
  (check-false (for/or ([span (in-list (code-spans source))])
                 (equal? (code-span-color span) "#C03639"))))
