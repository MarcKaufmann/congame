#lang racket/base

(require congame/components/study
         koyo/haml)

(provide prolific-redirect)

(define (skip-landing-page)
  (skip))

(define (hello)
  (haml
   (.container
    (:h1 "Hello")

    (:p "You should not have been redirected here immediately."))))

(define prolific-redirect
  (make-study
   "prolific-redirect"
   (list
    (make-step 'skip skip-landing-page)
    (make-step 'hello hello))))
