#lang info

(define collection "congame-price-lists")
(define deps '("base"
               "congame-core"
               "study-tools"
               "forms-lib"
               "koyo-lib"
               "marionette-lib"
               "web-server-lib"))
(define build-deps '())
(define congame-studies
  '((congame-price-lists/price-lists pl-study)))
(define congame-bots
  '((congame-price-lists/price-lists pl-bot #:for pl-study #:models (pl-bot-model))))
