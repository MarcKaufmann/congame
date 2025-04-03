#lang info

(define collection "congame")
(define deps
  '("at-exp-lib"
    "base"
    "racket-index"
    "scribble-lib"
    "threading-lib"))
(define build-deps
  '("buid"
    "buid-lib"
    "drracket-core"
    "scribble-doc"
    "slideshow-doc"
    "congame-core"
    "congame-web"
    "conscript"
    "debug"
    "forms-lib"
    "marionette-doc"
    "marionette-lib"
    "racket-doc"
    "scribble-lib"
    "threading-lib"
    "web-server-lib"))
(define scribblings
  '(("congame.scrbl" (multi-page))))
