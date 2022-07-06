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
               "sentry-lib"
               "web-server-lib"))
(define build-deps '("rackunit-lib"))
(define congame-studies
  '((congame-pjb-studies/pjb-pilot pjb-pilot-study)
    (congame-pjb-studies/pjb-pilot relax-test-study)))
(define congame-bots
  '((congame-pjb-studies/pjb-pilot-bot make-pjb-pilot-bot #:for pjb-pilot-study #:models (pjb-pilot-bot-model))
    (congame-pjb-studies/pjb-pilot-bot make-relax-test-bot #:for relax-test-study #:models (relax-test-bot-model))))
