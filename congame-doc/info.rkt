#lang info

(define collection "congame")
(define deps '("base"))
(define build-deps '("congame-core"
                     "congame-web"
                     "congame-identity"
                     "congame-smtp-proxy"
                     "forms-lib"
                     "koyo-doc"
                     "koyo-lib"
                     "racket-doc"
                     "scribble-lib"
                     "web-server-lib"
                     "web-server-doc"))
(define scribblings '(("congame.scrbl")
                      ("formular.scrbl")))
