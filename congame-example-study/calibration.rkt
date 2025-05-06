#lang racket/base

(require congame/components/formular
         congame/components/study
         koyo/haml)

(provide
 calibration)

;; Interactively identifies the indifference point (calibrates) for one
;; option versus the other. This was used in an unused pilot for the
;; narrow bracketing study. The data-calibration attribute on the input
;; fires the associated unpoly compiler in (calibration.ts), making the
;; UI interactive.
(define (calibration-form)
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
        (:button.button.next-button ([:type "submit"]) "Yes, proceed to next step"))))))))

(define (end)
  (haml
   (.container
    (:h1 "The End"))))

(define calibration
  (make-study
   "calibration"
   (list
    (make-step 'calibration calibration-form)
    (make-step 'end end))))
