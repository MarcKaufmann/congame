#lang info

(define collection "conscript")
(define deps
  '("at-exp-lib"
    "base"
    "buid-lib"
    "commonmark-lib"
    "component-lib"
    "congame-core"
    "db-lib"
    "deta-lib"
    ["forms-lib" #:version "0.10"]
    "hash-view-lib"
    "koyo-lib"
    "marionette-lib"
    "math-lib"
    "monocle-lib"
    "threading-lib"
    "syntax-color-lib"
    "web-server-lib"))
(define build-deps
  '("rackunit-lib"))
