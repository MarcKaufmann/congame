#lang info

(define collection "congame-pjb-studies")
(define deps '("base"
               "component-lib"
               "congame-core"
               "congame-price-lists"
               "forms-lib"
               "gregor-lib"
               "htdp-lib"
               "koyo-lib"
               "marionette-lib"
               "sentry-lib"))
(define build-deps '())
(define congame-studies
  '((congame-pjb-studies/pjb-pilot pjb-pilot-study)))
(define congame-bots
  '((congame-pjb-studies/pjb-pilot-bot pjb-pilot-bot #:for pjb-pilot-study #:models (pjb-pilot-bot-model))))
