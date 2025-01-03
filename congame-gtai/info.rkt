#lang info

(define collection "congame-gtai")
(define deps
  '("base"
    "component-lib"
    "congame-core"
    "congame-web"
    "conscript"
    "gregor-lib"
    "forms-lib"
    "koyo-lib"
    ["monocle-lib" #:version "0.3"]
    "sentry-lib"
    "threading-lib"
    "web-server-lib"))
(define build-deps '())
(define congame-studies
  '((congame-gtai/grade-game-lecture grade-game-lecture)))
(define congame-bots
  '())
