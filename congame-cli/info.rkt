#lang info

(define collection "congame")
(define deps
  '("base"
    "koyo-lib"
    ["http-easy-lib" #:version "0.8"]
    "web-server-lib"))
(define raco-commands
  '(("congame" (submod congame/cli main) "congame command line utilities" #f)))
