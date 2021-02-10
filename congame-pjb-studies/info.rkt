#lang info

(define collection "congame-pjb-studies")
(define deps '("base"
               "congame-core"
               "congame-price-lists"
               "2htpd/image"))
(define build-deps '())
(define congame-studies
  '((congame-pjb-studies/pjb-pilot pjb-pilot-study)))
(define congame-bots
  '((congame-pjb-studies/pjb-pilot-bot pjb-pilot-bot #:for pjb-pilot-study #:models (pjb-pilot-bot-model))))
