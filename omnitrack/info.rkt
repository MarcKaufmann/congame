#lang info

(define collection "omnitrack")
(define deps '("base"
               "component-lib"
               "congame-core"
               "congame-web"
               "gregor-lib"
               "forms-lib"
               "koyo-lib"
               "sentry-lib"
               "threading-lib"
               "web-server-lib"))
(define build-deps '())
(define congame-studies
  '((omnitrack/track-physical sleep-tracker)))
