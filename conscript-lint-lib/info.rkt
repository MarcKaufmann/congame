#lang info

(define collection "conscript")
(define deps
  '("base"
    ["review" #:version "0.2"]))
(define review-exts
  '((conscript/review should-review-syntax? review-syntax)))
