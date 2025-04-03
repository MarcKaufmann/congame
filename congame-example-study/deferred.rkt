#lang racket/base

(require congame/components/study
         congame/components/transition-graph
         koyo/haml
         racket/format)

(provide
 deferred-study
 wrapped-deferred-study)

(define (init)
  (haml
   (button
    (λ () (put 'a 42))
    "Continue")))

(define ((make-show a))
  (haml
   (:p "a = " (~a a))))

(define deferred-study
  (make-study
   "deferred"
   #:transitions
   (transition-graph
    [init --> substudy]
    [substudy --> ,done])
   (list
    (make-step 'init init)
    (make-step/study 'substudy (lambda ()
                                 (define a (get 'a))
                                 (make-study
                                  "substudy"
                                  (list
                                   (make-step 'show-a (make-show a)))))))))

(define wrapped-deferred-study
  (map-study
   deferred-study
   (lambda (hdl)
     (lambda ()
       (define the-page-thunk (hdl))
       (haml
        (:div
         (:h1 "Wrapper")
         (the-page-thunk)))))))
