#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/unit)

(provide
 congame^)

(define-signature congame^
  ((define-syntaxes (defvar)
     (lambda (stx)
       (syntax-parse stx
         [(_ id:id)
          #'(begin
              (define-syntax id
                (make-set!-transformer
                 (lambda (stx)
                   (syntax-case stx (set!)
                     [(set! id v) #'(put-var 'id v)]
                     [id (identifier? #'id) #'(get-var 'id)])))))])))
   get-var put-var
   (define-syntaxes (defvar/instance)
     (lambda (stx)
       (syntax-parse stx
         [(_ id:id)
          #`(begin
              (define-syntax id
                (make-set!-transformer
                 (lambda (stx)
                   (syntax-case stx (set!)
                     [(set! id v) #'(put-var/instance 'id v)]
                     [id (identifier? #'id) #'(get-var/instance 'id)])))))])))
   get-var/instance put-var/instance
   (define-syntaxes (defvar*)
     (lambda (stx)
       (syntax-parse stx
         [(_ id:id unique-id:id)
          #'(begin
              (define-syntax id
                (make-set!-transformer
                 (lambda (stx)
                   (syntax-case stx (set!)
                     [(set! id v) #'(put-var* 'unique-id 'id v)]
                     [id (identifier? #'id) #'(get-var* 'unique-id 'id)])))))])))
   get-var* put-var*
   (define-syntaxes (defvar*/instance)
     (lambda (stx)
       (syntax-parse stx
         [(_ id:id unique-id:id)
          #'(begin
              (define-syntax id
                (make-set!-transformer
                 (lambda (stx)
                   (syntax-case stx (set!)
                     [(set! id v) #'(put-var*/instance 'unique-id 'id v)]
                     [id (identifier? #'id) #'(get-var*/instance 'unique-id 'id)])))))])))
   get-var*/instance put-var*/instance
   undefined? skip call-with-study-transaction))
