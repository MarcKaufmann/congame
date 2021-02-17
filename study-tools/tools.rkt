#lang racket/base

(require (except-in forms form)
         (prefix-in forms: (only-in forms form))
         racket/contract
         racket/format
         marionette
         koyo/haml
         congame/components/study
         (prefix-in bot: (submod congame/components/bot actions)))

(provide
 pp-money
 render-consent-form
 consent/bot)

(define/contract (pp-money amount #:currency [currency "$"])
  (-> number? string?)
  (~a currency (~r amount #:precision '(= 2))))

(define (render-consent-form)
  (define the-form
    (form* ([consent? (ensure binding/boolean (required #:message "You can only continue with the study if you agree to participate."))])
           consent?))
  (haml
   (:div
    (:h2 "Consent Form")
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
               (button ([type "Submit"] [class "button"]) "Submit"))))))))

(define (consent/bot)
  (define consent-checkbox (bot:find "input[type=checkbox]"))
  (element-click! consent-checkbox)
  (element-click! (bot:find "button[type=submit]")))
