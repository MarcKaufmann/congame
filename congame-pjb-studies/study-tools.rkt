#lang racket

(require (except-in forms form)
         (prefix-in forms: (only-in forms form))
         koyo/haml
         congame/components/study)

(provide
 pp-money
 render-consent-form)

(define/contract (pp-money amount #:currency [currency "€"])
  (-> number? string?)
  (~a currency (~r amount #:precision '(= 2))))

(define (render-consent-form)
  (define the-form
    ; TODO: Check with Bogdan that this always yields #t or #f, unlike the HTML version which returns nothing
    ; if the form isn't checked. Is it safe to rely on such behavior?
    (form* ([consent? (ensure binding/boolean)])
           consent?))
  (haml
    (form
     the-form
     ; after successful submit
     (λ (consent?) (put 'consent? consent?))
     ; renderer: (-> rw xexpr)
     (λ (rw)
       `(form ((action "")
               (method "POST"))
              (label
               "I agree to participate in the study"
               ,(rw "consent?" (widget-checkbox))
              ,@(rw "consent?" (widget-errors))
              (button ((type "Submit")) "Submit")))))))
