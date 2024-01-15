#lang racket/base

;; SECURITY: This is the entrypoint for the restricted conscript
;; language. This language is not allowed to import arbitrary racket
;; modules and care must be taken to avoid giving it too much power (eg.
;; the ability to interact with the congame database at a level beyond
;; basic gets and puts -- we don't want #lang conscript study writers
;; to be able to drop the database). We can't sandbox #lang conscript
;; studies because our architecture is such that certain modules (db,
;; congame/core/study, and others) would need to be shared with the
;; sandbox, thereby giving #lang conscript study writers a way to get to
;; the database and do dangerous things.

(module reader syntax/module-reader
  conscript/base
  #:read (lambda (in) (do-read-syntax #f in))
  #:read-syntax do-read-syntax
  #:info (lambda (key defval proc)
           ((dynamic-require 'conscript/tool 'get-info) key defval proc))
  (require scribble/reader)
  (define (do-read-syntax src in)
    (parameterize ([current-readtable (make-at-readtable)])
      (read-syntax src in))))
