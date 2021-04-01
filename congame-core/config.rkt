#lang racket/base

(require koyo/config)

(define-option debug
  (equal? debug "x"))

(define-option support-email
  #:default "admin@totalinsightmanagement.com")
