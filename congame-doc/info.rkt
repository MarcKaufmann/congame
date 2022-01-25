#lang info

(define collection "congame")
(define deps '("base"))
(define build-deps '("congame-core"
                     "congame-web"
                     "congame-identity"
                     "congame-smtp-proxy"
                     "racket-doc"
                     "scribble-lib"
                     "web-server-lib"
                     "web-server-doc"))
(define scribblings '(("congame.scrbl")))
