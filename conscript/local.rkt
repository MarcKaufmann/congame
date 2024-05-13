#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         (only-in congame/components/study
                  make-study-participant
                  done?
                  next?)
         (only-in (submod congame/components/study private)
                  current-embed/url
                  current-request
                  current-return
                  current-study-manager
                  response/step
                  study-manager
                  study-steps
                  step/study?
                  step/study-study
                  step-id
                  step-handler
                  step-transition)
         koyo/continuation
         net/sendurl
         net/url
         racket/async-channel
         racket/match
         web-server/dispatchers/dispatch
         (only-in web-server/http request-uri)
         (only-in web-server/servlet send/suspend/dispatch servlet-prompt)
         web-server/servlet-dispatch
         web-server/web-server
         (prefix-in conscript: "base.rkt")
         (except-in "base.rkt" defvar form))

(provide
 (all-from-out "base.rkt"))


;; preview ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 preview)


(define (preview a-study)
  (define port-or-exn-ch
    (make-async-channel))
  (define stop
    (parameterize ([current-continuation-key-cookie-secure? #f])
      (serve
       #:port 0
       #:listen-ip "127.0.0.1"
       #:dispatch (dispatch/servlet
                   (wrap-protect-continuations
                    (lambda (req)
                      (unless (equal? (url-path (request-uri req))
                                      (list (path/param "" null)))
                        (next-dispatcher))
                      (define manager
                        (study-manager
                         (make-study-participant
                          #:id 1
                          #:user-id 1
                          #:instance-id 1)
                         #f))
                      (parameterize ([current-request req]
                                     [current-study-manager manager])
                        (run-study a-study)))))
       #:confirmation-channel port-or-exn-ch)))
  (define port-or-exn
    (sync port-or-exn-ch))
  (when (exn:fail? port-or-exn)
    (raise port-or-exn))
  (send-url (format "http://127.0.0.1:~a" port-or-exn))
  (with-handlers ([exn:break? void])
    (sync never-evt))
  (stop))

(define (run-study a-study)
  (define paramz (current-parameterization))
  (parameterize ([current-vars (or (current-vars) (make-hash))]
                 [current-data (make-hash)])
    (let loop ([this-step (car (study-steps a-study))])
      (if (not this-step)
          `(continue ,paramz)
          (match (run-step this-step)
            [`(continue ,paramz)
             (match ((step-transition this-step))
               [(? done?)
                `(continue ,paramz)]
               [(? next?)
                (call-with-parameterization paramz
                  (lambda ()
                    (loop (find-next-step a-study (step-id this-step)))))]
               [next-step-id
                (define next-step
                  (find-step a-study next-step-id))
                (call-with-parameterization paramz
                  (lambda ()
                    (loop next-step)))])]
            [`(to-step ,to-step-id ,paramz)
             (define next-step
               (find-step a-study to-step-id))
             (call-with-parameterization paramz
               (lambda ()
                 (loop next-step)))])))))

(define (run-step a-step)
  (define paramz #f)
  (call-with-current-continuation
   (lambda (return)
     (send/suspend/dispatch/protect
      (lambda (embed/url)
        (parameterize ([current-embed/url (lambda (hdl)
                                            (embed/url (λ (req)
                                                         (call-with-parameterization paramz
                                                           (lambda ()
                                                             (parameterize ([current-request req])
                                                               (hdl req)))))))]
                       [current-return return])
          (set! paramz (current-parameterization))
          (if (step/study? a-step)
              (return (run-study (step/study-study a-step)))
              (response/step a-step))))))
   servlet-prompt))

(define (find-next-step a-study id)
  (let loop ([steps (study-steps a-study)])
    (match steps
      [(? null?) #f]
      [`(,this-step ,next-step . ,_other-steps)
       #:when (eq? (step-id this-step) id)
       next-step]
      [`(,_this-step . ,other-steps)
       (loop other-steps)])))

(define (find-step a-study id)
  (define the-step
    (for/first ([s (in-list (study-steps a-study))]
                #:when (eq? (step-id s) id))
      s))
  (begin0 the-step
    (unless the-step
      (error 'find-step "step ~a not found" id))))


;; stubs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 form
 defvar
 defvar*
 defvar*/instance
 with-transaction)

(define current-vars (make-parameter #f))
(define current-instance-vars (make-parameter (make-hash)))
(define current-data (make-parameter #f))

(define-syntax (defvar stx)
  (syntax-parse stx
    [(_ id:id)
     #`(begin
         (define-syntax id
           (make-set!-transformer
            (lambda (stx)
              (syntax-case stx (set!)
                [(set! id v) #'(put 'id v)]
                [id (identifier? #'id) #'(get 'id)])))))]))

(define (get id)
  (hash-ref (current-data) id undefined))

(define (put id v)
  (hash-set! (current-data) id v))

(define-syntax (defvar* stx)
  (syntax-parse stx
    [(_ id:id unique-id:id)
     #`(begin
         (define-syntax id
           (make-set!-transformer
            (lambda (stx)
              (syntax-case stx (set!)
                [(set! id v) #'(put* 'unique-id 'id v)]
                [id (identifier? #'id) #'(get* 'unique-id 'id)])))))]))

(define (get* uid k)
  (hash-ref
   (current-vars)
   (cons uid k)
   undefined))

(define (put* uid k v)
  (hash-set!
   (current-vars)
   (cons uid k)
   v))

(define-syntax (defvar*/instance stx)
  (syntax-parse stx
    [(_ id:id unique-id:id)
     #`(begin
         (define-syntax id
           (make-set!-transformer
            (lambda (stx)
              (syntax-case stx (set!)
                [(set! id v) #'(put*/instance 'unique-id 'id v)]
                [id (identifier? #'id) #'(get*/instance 'unique-id 'id)])))))]))

(define (get*/instance uid k)
  (hash-ref
   (current-instance-vars)
   (cons uid k)
   undefined))

(define (put*/instance uid k v)
  (hash-set!
   (current-instance-vars)
   (cons uid k)
   v))

(define-syntax (form stx)
  (syntax-parse stx
    [(_ {~alt
         {~optional {~seq #:action action:expr}}
         {~optional {~seq #:bot bot}}
         {~optional {~seq #:fields fields}}} ...
        body ...+)
     #'(conscript:form
        {~@ #:action {~? action default-form-action}}
        {~? {~@ #:bot bot}}
        {~? {~@ #:fields fields}}
        body ...)]))

(define default-form-action
  (make-keyword-procedure
   (lambda (kws kw-args . _args)
     (for ([kwd (in-list kws)]
           [arg (in-list kw-args)])
       (put (string->symbol (keyword->string kwd)) arg)))))

(define-syntax-rule (with-transaction body0 body ...)
  (begin body0 body ...))

(module reader syntax/module-reader
  conscript/local
  #:read (lambda (in) (do-read-syntax #f in))
  #:read-syntax do-read-syntax
  #:info (lambda (key defval proc)
           ((dynamic-require 'conscript/tool 'get-info) key defval proc))
  (require scribble/reader)
  (define (do-read-syntax src in)
    (parameterize ([current-readtable (make-at-readtable)])
      (read-syntax src in))))
