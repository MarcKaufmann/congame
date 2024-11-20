#lang racket/base

(require racket/format
         racket/lazy-require
         racket/match
         racket/pretty
         racket/string
         (only-in web-server/http/xexpr response/xexpr)
         (only-in xml valid-char?))

(lazy-require
 ["formular.rkt" (formular-field?)])

(provide
 response/xexpr*
 validate-xexpr)

(define (validate-xexpr e
                        #:on-attr-err [on-attr-err default-attr-err]
                        #:on-xexpr-err [on-xexpr-err default-xexpr-err])
  (define (validate-attribute attr ctxt)
    (match attr
      [`(,(? symbol? _name)
         ,(? string? _value))
       attr]
      [_ (on-attr-err attr ctxt)]))
  (let loop ([e e] [ctxt #f] [ctxt2 #f])
    (match e
      ;; intentionally ignoring `cdata' and `misc', because we never use them
      [(? valid-char?) e]
      [(? string?) e]
      [(? symbol?) e]
      [(list
        (? symbol? tag-name)
        (list (list attr-names attr-values) ...)
        children ...)
       ;; In addition to validating the attributes, if `on-attr-err'
       ;; returns #f for any attribute, then that attribute is skipped
       ;; in the resulting xexpr.
       (define validated-attributes
         (filter values
                 (for/list ([attr (in-list (map list attr-names attr-values))])
                   (validate-attribute attr e))))
       (define validated-children
         (for/list ([child (in-list children)])
           (loop child e ctxt)))
       `(,tag-name (,@validated-attributes) ,@validated-children)]
      [(list
        (? symbol? tag-name)
        children ...)
       (loop `(,tag-name () ,@children) ctxt ctxt2)]
      [_
       (if (procedure-arity-includes? on-xexpr-err 3)
           (on-xexpr-err e ctxt (or ctxt2 ctxt))
           (on-xexpr-err e ctxt))])))

(define (response/xexpr* e #:validator [validator validate-xexpr])
  (with-handlers ([exn:fail:xexpr? (λ (_e) (response/xexpr (validator e)))])
    (response/xexpr #:preamble #"<!DOCTYPE html>" e)))

(define (exn:fail:xexpr? e)
  (and (exn? e)
       (regexp-match? #rx"Not an Xexpr" (exn-message e))))

(define (~pretty e [indent 2])
  (define indent-str (make-string indent #\space))
  (define sep (~a "\n" indent-str))
  (~a indent-str (string-join (string-split (pretty-format e 40) "\n") sep)))

(define (default-attr-err invalid-attr ctxt)
  (error 'validate-expr "invalid attribute: ~e~n in immediate context:~n~a" invalid-attr (~pretty ctxt)))

(define (default-xexpr-err invalid-e ctxt ctxt2)
  (cond
    [(formular-field? invalid-e)
     (error
      'validate-xexpr
      (string-join
       '("invalid x-expression: ~e"
         " in immediate context:"
         "~a"
         " in extended context:"
         "~a"
         " hint: you may have forgotten to (set! ...) this form field")
       "\n")
      invalid-e
      (~pretty ctxt)
      (~pretty ctxt2))]
    [else
     (error 'validate-xexpr "invalid x-expression: ~e~n in immediate context:~n~a" invalid-e (~pretty ctxt))]))
