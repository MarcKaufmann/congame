#lang racket/base

(require congame/components/study
         congame/components/transition-graph
         koyo/haml)

(provide edpb-pilot)

(define (welcome)
  (page
   (haml
    (.container
     (:h1 "Welcome")))))

(define (stub)
  (page
   (haml
    (.container
     (:h1 "Stub")
     (button void "Next")))))

(define edpb-pilot
  (make-study
   "edpb pilot"
   #:transitions
   (transition-graph
    [welcome --> welcome])
   (list
    (make-step 'welcome welcome))))
