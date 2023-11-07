#lang racket/base

(require koyo/haml
         web-server/http)

(provide
 widget-sl-radios)

(define ((widget-sl-radios options) name binding _errors)
  (define value
    (and binding (bytes->string/utf-8 (binding:form-value binding))))
  (haml
   (:sl-radio-group
    ([:name name]
     [:value (or value "")])
    ,@(for/list ([option (in-list options)])
        (define radio-value (car option))
        (define radio-label (cdr option))
        (haml
         (:sl-radio
          ([:value radio-value])
          radio-label))))))
