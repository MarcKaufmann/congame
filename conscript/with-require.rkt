#lang racket/base

(require (except-in "base.rkt" require))

(provide
 (all-from-out "base.rkt")

 ;; Racket Syntax
 require only-in except-in prefix-in)

(module reader syntax/module-reader
  conscript/with-require
  #:read (lambda (in) (do-read-syntax #f in))
  #:read-syntax do-read-syntax
  (require scribble/reader)
  (define (do-read-syntax src in)
    (parameterize ([current-readtable (make-at-readtable)])
      (read-syntax src in))))
