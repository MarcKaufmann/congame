#lang info

(define collection "conscript")
(define deps
  '("base"
    "at-exp-lib"
    "buid-lib"
    "commonmark-lib"
    "congame-core"
    ["forms-lib" #:version "0.9"]
    "hash-view-lib"
    "koyo-lib"
    "monocle-lib"
    "threading-lib"
    "web-server-lib"))
(define build-deps
  '("rackunit-lib"))
