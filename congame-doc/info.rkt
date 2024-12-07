#lang info

(define collection "congame")
(define deps '("at-exp-lib"
               "base"
               "scribble-lib"))
(define build-deps '("buid"
                     "buid-lib"
                     "drracket"
                     "scribble-doc"
                     "slideshow-doc"
                     "congame-core"
                     "congame-web"
                     "conscript"
                     "forms-doc"
                     "forms-lib"
                     "koyo-doc"
                     "koyo-lib"
                     "racket-doc"
                     "scribble-lib"
                     "web-server-lib"
                     "web-server-doc"))
(define scribblings '(("congame.scrbl" (multi-page))))
