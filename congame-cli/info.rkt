#lang info

(define collection "congame")
(define deps
  '("base"
    "conscript"
    "drracket-plugin-lib"
    "gui-lib"
    "gui-easy-lib"
    "koyo-lib"
    ["http-easy-lib" #:version "0.8"]
    "marionette-lib"
    "net-lib"
    "threading-lib"
    "web-server-lib"))
(define raco-commands
  '(("congame" (submod congame/cli main) "congame command line utilities" #f)))

(define drracket-tool-names (list "Congame"))
(define drracket-tools (list (list "tool.rkt")))
