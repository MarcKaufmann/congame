#lang info

(define collection "congame")
(define deps '("base"
               "scribble-lib"))
(define build-deps '("congame-core"
                     "congame-example-study"
                     "congame-web"
                     "congame-identity"
                     "congame-smtp-proxy"
                     "forms-doc"
                     "forms-lib"
                     "koyo-doc"
                     "koyo-lib"
                     "racket-doc"
                     "scribble-lib"
                     "web-server-lib"
                     "web-server-doc"))
(define scribblings '(("congame.scrbl" (multi-page))
                      ("formular.scrbl")))
