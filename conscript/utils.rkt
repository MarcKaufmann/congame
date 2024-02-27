#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         congame/components/formular

         koyo/haml)

(define-syntax (make-sliders stx)
  (syntax-parse stx
    [(_ n:nat)
     (with-syntax ([(name ...)
                    (build-list
                     (syntax-e #'n)
                     (lambda (i) (format "slider-~a" i)))])
     #'(haml
        (:div
         (:div ([:class "slider"])
          (list
           (string->keyword name)
           (input-range "")
           (haml
            (:span "Value: " (:output ""))))) ... )))]))

(make-sliders 2)
