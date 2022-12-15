#lang racket/base

(require racket/lazy-require)

(lazy-require
 [congame/components/study (make-step/study)])

(provide
 make-step/study

 add1
 sub1
 format)
