#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         forms
         koyo/haml
         racket/match (prefix-in study: "study.rkt"))

(provide
 formular
 checkbox
 radios)

(define-syntax (formular stx)
  (syntax-parse stx
    [(_ form action-e)
     #:with rw (format-id stx "rw")
     #:with tbl (format-id stx "tbl")
     #:with ((kwd fld) ...)
     (let loop ([stx #'form]
                [pairs null])
       (syntax-parse stx
         [(kwd:keyword fld)
          (cons #'(kwd fld) pairs)]

         [(e ...)
          (apply append (map (λ (stx) (loop stx null))
                             (syntax->list #'(e ...))))]

         [_ pairs]))
     #:with (field-id ...)
     (for/list ([idx (in-naturals 1)]
                [kwd (in-list (syntax-e #'(kwd ...)))])
       (format-id kwd "input_~a" idx))
     #:with patched-form
     (let loop ([stx #'form])
       (syntax-parse stx
         [(kwd:keyword _)
          #'(let ([entry (hash-ref tbl 'kwd)])
              (rw (car entry) ((cdr entry) 'widget)))]

         [(e ...)
          #`(#,@(map loop (syntax-e #'(e ...))))]

         [e #'e]))
     #'(let ([action-fn action-e]
             [field-id fld] ...)
         (let ([tbl (make-hasheq
                     (list (cons 'kwd (cons (symbol->string 'field-id) field-id)) ...))])
           (study:form
            (form* ([field-id (field-id 'validator)] ...)
              (cons
               (list 'kwd ...)
               (list field-id ...)))
            (lambda (res)
              (define vals-by-kwd
                (for/hasheq ([k (in-list (car res))]
                             [v (in-list (cdr res))])
                  (values k v)))
              (define sorted-kwds
                (sort (car res) keyword<?))
              (define sorted-vals
                (for/list ([k (in-list sorted-kwds)])
                  (hash-ref vals-by-kwd k)))
              (keyword-apply action-fn sorted-kwds sorted-vals null))
            (lambda (rw)
              patched-form))))]))

(define ((checkbox label) meth)
  (match meth
    ['validator
     (ensure binding/boolean (required))]

    ['widget
     (lambda (name value errors)
       (haml
        (.group
         (:label ((widget-checkbox) name value errors) label)
         ,@((widget-errors) name value errors))))]))

(define ((radios label options #:validators [validators null]) meth)
  (match meth
    ['validator
     (apply ensure binding/text (required) validators)]

    ['widget
     (lambda (name value errors)
       (haml
        (.group
         (:label.radio-group
          label
          ((widget-radio-group options) name value errors))
         ,@((widget-errors) name value errors))))]))
