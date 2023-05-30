#lang racket/base

(require congame/components/formular
         congame/components/study
         (only-in forms required)
         koyo/haml)

(provide
 inline-study)

(define (info)
  (page
   (haml
    (.container
     (formular
      (haml
       (:div
        "I emit "
        (#:emissions (input-number #:required? "You need to specify emissions."))
        " per year. And I live in "
        (#:location (input-text #:required? "You need to specify a location."))
        "."
        ,@(~all-errors)
        (:button ([:type "submit"]) "Submit"))))))))

(define (done)
  (page
   (haml
    (.container
     (:h1 "Byeee")))))

(define inline-study
  (make-study
   "inline-study"
   (list
    (make-step 'info info)
    (make-step 'done done))))
