#lang racket/base

(require congame/components/study
         congame/components/formular
         (submod congame/components/formular tools)
         koyo/haml)

(provide
 calibration)

(define (calibration-form)
  (page
   (haml
    (.container
     (:h1 "Calibration")

     (formular
      (haml
       (:div
        (:div
         (#:wtw
          (input-number #:attributes `([data-calibration "calibration" ]
                                       [data-calibrationPrefix "£"]) #:min 0 #:max 4 "WTW" #:step 0.1)))
        (:div#confirm-choice
         (:span#confirmation-message "")
         (:a#reset-button.button ([:href ""])"Reset")
        (:button.button.next-button ([:type "submit"]) "Yes, proceed to next step")))))))))

(define (end)
  (page
   (haml
    (.container
     (:h1 "The End")))))

(define calibration
  (make-study
   "calibration"
   (list
    (make-step 'calibration calibration-form)
    (make-step 'end end))))
