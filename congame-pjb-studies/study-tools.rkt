#lang racket

(provide
 pp-money)

(define/contract (pp-money amount)
  (-> number? string?)
  (format "$~a" amount))
