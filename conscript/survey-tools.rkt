#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         (prefix-in congame: congame/components/formular)
         (prefix-in congame: (submod congame/components/formular tools))
         (prefix-in congame: congame/components/study)
         (submod congame/components/study accessors)
         congame/components/resource
         (except-in forms form)
         koyo/haml
         racket/format
         racket/list
         racket/runtime-path)



;; Helper Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 ~$ ~pound ~euro)

(define ((~currency [currency "$"]) a)
  (format "~a~a" currency (~r a #:precision 2)))

(define ~$ (~currency "$"))
(define ~pound (~currency "£"))
(define ~euro (~currency "€"))

;; Slider ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-sliders
 slider-js)

(define-static-resource slider.js
  "resources/js/slider.js")

(define slider-js
  (haml
    (:script
     ([:defer ""]
      [:src (resource-uri slider.js)]))))

(define-syntax (make-sliders stx)
  (syntax-parse stx
    [(_ n:nat {~optional make-widget-proc-expr:expr})
     (with-syntax ([((slider-id kwd) ...)
                    (build-list
                     (syntax-e #'n)
                     (lambda (i)
                       (list i (string->keyword (format "slider-~a" i)))))])
       #`(let ([make-widget-proc {~? make-widget-proc-expr (λ (_) (congame:input-range))}])
           (congame:formular
            (haml
             (:div
              slider-js
              (:div
               (:div
                ([:class "slider"])
                (kwd (make-widget-proc slider-id))
                (:span "Value: " (:output ""))) ... )
              congame:submit-button)))))]))


;; Multiple Checkboxes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-multiple-checkboxes)

; FIXME: We have no way to require a certain number of choices. Implement.
(define (render-checkbox-list options render-checkbox)
  `(div
    ()
    ,@(for/list ([o (in-list options)])
        (define v (car o))
        (define l (cdr o))
        (render-checkbox v l))))

(define (make-multiple-checkboxes options [render-proc render-checkbox-list]
                                  #:n [n 0]
                                  #:required? [required? #t])
  (congame:make-checkboxes
   options
   render-proc
   #:n n
   #:required? required?))

;; radio-button validator

(provide
 is-equal)

(define ((is-equal a #:message [message #f]) v)
  (if (equal? v a)
      (ok v)
      (err (or message (format "Should be equal to ~a" a)))))

;; Timer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 timer)

(define-static-resource timer.js
  "resources/js/timer.js")

(define (timer n)
  (haml
   (:div
    (:div#timer-target)
    (:script
     ([:data-timer-n (number->string n)]
      [:src (resource-uri timer.js)])))))


;; Treatment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 assigning-treatments)

(define (assigning-treatments
         treatments
         #:treatments-key [treatments-key 'treatments]
         #:role-key [role-key 'role])
  (unless (get* role-key #f)
    (congame:with-study-transaction
      (when (null? (get/instance* treatments-key '()))
        (put/instance* treatments-key (shuffle treatments)))
      (define remaining-treatments
        (get/instance* treatments-key))
      (define role
        (car remaining-treatments))
      (put* role-key role)
      (define updated-treatments
        (cdr remaining-treatments))
      (put/instance* treatments-key updated-treatments))))

;;; Survey questions
(provide
 questions)

(define questions
  (hash 'occupation (list
                     "What is your occupation?"
                     '(
                       ("1"  . "Management, professional, and related")
                       ("2"  . "Service")
                       ("3"  . "Sales and office")
                       ("4"  . "Farming, fishing, and forestry")
                       ("5"  . "Constuction, extraction, and maintenance")
                       ("6"  . "Production, transportation, and material moving")
                       ("7"  . "Government")
                       ("8"  . "Retired")
                       ("9"  . "Unemployed")
                       ("10" . "Student")
                       ("11" . "Other")))))

;;;; Mathjax

(provide
 mathjax-scripts)

(define (mathjax-scripts)
  (haml
   (:div
    (:script
     ([:src "https://polyfill.io/v3/polyfill.min.js?features=es6"]))
    (:script
     ([:id "MathJax-script"]
      [:async ""]
      [:src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"])))))
