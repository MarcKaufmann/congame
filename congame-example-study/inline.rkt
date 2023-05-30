#lang racket/base

(require congame/components/formular
         congame/components/study
         (only-in forms required)
         koyo/haml
         racket/list
         racket/match)

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

(define (dynamic)
  (page
   (haml
    (.container
     (formular
      #:fields ([a (input-text "field a")]
                [b (input-text "field b")])
      (match (shuffle (list a b))
        [(list f1 f2)
         (haml
          (:div
           (:ul
            (:li "F1: " f1)
            (:li "F2: " f2))
           (:button ([:type "submit"]) "Submit")))]))))))

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
    (make-step 'dynamic dynamic)
    (make-step 'done done))))
