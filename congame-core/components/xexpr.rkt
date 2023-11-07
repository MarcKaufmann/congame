#lang racket/base

(require racket/format
         racket/match
         racket/pretty
         racket/string
         (only-in web-server/http/xexpr response/xexpr)
         (only-in xml valid-char?))

(provide
 response/xexpr*
 validate-xexpr)

(define (validate-xexpr e
                        #:on-attr-err [on-attr-err (λ (invalid-attr ctxt)
                                                     (error 'validate-expr "invalid attribute: ~e~n in immediate context:~n~a" invalid-attr (~pretty ctxt)))]
                        #:on-xexpr-err [on-xexpr-err (λ (invalid-e ctxt)
                                                       (error 'validate-xexpr "invalid x-expression: ~e~n in immediate context:~n~a" invalid-e (~pretty ctxt)))])
  (define (validate-attribute attr ctxt)
    (match attr
      [`(,(? symbol? _name)
         ,(? string? _value))
       attr]
      [_ (on-attr-err attr ctxt)]))
  (define (help e [ctxt #f])
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
           (help child e)))
       `(,tag-name (,@validated-attributes) ,@validated-children)]
      [(list
        (? symbol? tag-name)
        children ...)
       (help `(,tag-name () ,@children) ctxt)]
      [_
       (on-xexpr-err e ctxt)]))
  (help e))

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
