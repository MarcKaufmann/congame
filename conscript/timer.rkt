#lang racket/base

(require congame/components/resource
         koyo/haml
         racket/runtime-path)

(provide
 timer)

(define-static-resource timer.js "timer.js")

(define (timer n)
  (haml
   (:div
    (:div#timer-target)
    (:script
     ([:data-timer-n (number->string n)]
      [:src (resource-uri timer.js)])))))
