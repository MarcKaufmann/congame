#lang info

(define collection "congame-gtai")
(define deps
  '("base"
    "congame-web"
    "conscript"
    ["monocle-lib" #:version "0.3"]
    "threading-lib"))
(define build-deps '())
(define congame-studies
  '((congame-gtai/grade-game grade-game-lecture)))
(define congame-bots
  '())
