#lang racket/base

(require koyo/config)

(current-option-name-prefix "CONGAME")

(define-option debug
  (equal? debug "x"))

(define-option support-email
  #:default "admin@totalinsightmanagement.com")
