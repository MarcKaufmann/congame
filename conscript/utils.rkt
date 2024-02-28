#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         congame/components/formular

         koyo/haml)

(define-syntax (make-sliders stx)
  (syntax-parse stx
    [(_ n:nat)
     (with-syntax ([(kwd ...)
                    (map
                     string->keyword
                     (build-list
                      (syntax-e #'n)
                      (lambda (i) (format "slider-~a" i))))])
       #'(haml
          (:div
           (:div ([:class "slider"])
                 (list
                  'kwd
                  (input-range "")
                  (haml
                   (:span "Value: " (:output ""))))) ... )))]))

(make-sliders 2)

;(make-sliders 2 slider-builder)

#;(define-syntax (make-sliders-2 stx)
  (syntax-parse stx
    [(_ n:nat tmp)
     (with-syntax ([(kwd ...)
                    (map
                     string->keyword
                     (build-list
                      (syntax-e #'n)
                      (lambda (i) (format "slider-~a" i))))])
       #'(haml
          (:div
           tmp ... )))]))

#;(make-sliders-2
 2
 (:div ([:class "slider"])
       (list
        kwd
        (input-range "")
        (haml
         (:span "Value: " (:output ""))))))


(define-for-syntax (make-slider kwd)
  `(haml
    (:div ([:class "slider"])
          (list
           ,kwd
           (input-range "")
           (haml
            (:span "Value: " (:output "")))))))

(define-syntax (make-sliders-3 stx)
  (syntax-parse stx
    [(_ n:nat)
     (with-syntax ([(kwd ...)
                    (map
                     string->keyword
                     (build-list
                      (syntax-e #'n)
                      (lambda (i) (format "slider-~a" i))))])
       #`(haml
          (:div
           (#,make-slider 'kwd) ... )))]))

(make-sliders-3 2)

(define-syntax (make-sliders-4 stx)
  (syntax-parse stx
    [(_ n:nat)
     (with-syntax ([(kwd ...)
                    (map
                     string->keyword
                     (build-list
                      (syntax-e #'n)
                      (lambda (i) (format "slider-~a" i))))])
       #`(haml
          (:div
           (#,make-slider 'kwd) ... )))]))
