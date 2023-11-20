#lang racket/base

(module reader syntax/module-reader
  conscript/base
  #:read (lambda (in) (do-read-syntax #f in))
  #:read-syntax do-read-syntax
  (require scribble/reader)
  (define (do-read-syntax src in)
    (parameterize ([current-readtable (make-at-readtable)])
      (read-syntax src in))))
