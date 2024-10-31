#lang info

(define collection "congame")
(define deps
  '("base"
    "koyo-lib"
    "web-server-lib"))
(define raco-commands
  '(("congame" (submod congame/cli main) "congame command line utilities" #f)))
