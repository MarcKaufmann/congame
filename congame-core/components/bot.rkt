#lang racket/base

(require racket/contract)

(provide
 current-user-bot?)

(define/contract current-user-bot?
  (parameter/c boolean?)
  (make-parameter #f))
