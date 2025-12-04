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
    "forms-doc"
    "gregor-doc"
    "koyo-lib"
    "koyo-doc"
    "marionette-doc"
    "marionette-lib"
    "racket-doc"
    "scribble-lib"
    "threading-lib"
    "threading-doc"
    "web-server-doc"
    "web-server-lib"))
(define scribblings
  '(("congame.scrbl" (multi-page))))
