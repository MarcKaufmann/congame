#lang info

(define collection "studies")
(define deps '("base"
               "component-lib"
               "congame-core"
               "congame-web"
               "forms-lib"
               "gregor-lib"
               "koyo-lib"
               "marionette"
               "racket/match"
               "racket/random"
               "racket/runtime-path"
               "racket/serialize"
               "racket/string"
               "sentry-lib"
               "web-server-lib"))
(define build-deps '())
(define congame-studies
  '(("studies/real-effort/tasks task-study")))
