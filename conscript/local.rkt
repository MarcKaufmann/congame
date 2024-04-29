#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         (only-in congame/components/study
                  make-study-participant)
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
                  step-handler)
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
         (except-in "base.rkt" defvar get get/instance put put/instance form))

(provide
 (all-from-out "base.rkt"))


;; preview ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 preview)

(define (preview a-study)
  (define port-or-exn-ch
    (make-async-channel))
  (define stop
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
     #:confirmation-channel port-or-exn-ch))
  (define port-or-exn
    (sync port-or-exn-ch))
  (when (exn:fail? port-or-exn)
    (raise port-or-exn))
  (send-url (format "http://127.0.0.1:~a" port-or-exn))
  (with-handlers ([exn:break? void])
    (sync never-evt))
  (stop))

;; FIXME: Need to make defvar work across studies.
(define (run-study a-study)
  (define paramz (current-parameterization))
  (parameterize ([current-vars (make-hash)]
                 [current-instance-vars (make-hash)])
    (let loop ([steps (study-steps a-study)])
      (if (null? steps)
          `(continue ,paramz)
          (match (run-step (car steps))
            [`(continue ,paramz)
             (call-with-parameterization paramz
               (lambda ()
                 (loop (cdr steps))))])))))

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


;; stubs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 defvar form
 get get/instance
 put put/instance)

(define current-vars (make-parameter #f))
(define current-instance-vars (make-parameter #f))

(define-syntax (defvar stx)
  (syntax-parse stx
    [(_ id:id unique-id:id)
     #`(begin
         (define-syntax id
           (make-set!-transformer
            (lambda (stx)
              (syntax-case stx (set!)
                [(set! id v) #'(put #:root 'unique-id 'id v)]
                [id (identifier? #'id) #'(get #:root 'unique-id 'id)])))))]))

(define (get id
             [default (λ () (error 'get "no value found for key ~s" id))]
             #:root [root '*root*])
  (hash-ref
   (current-vars)
   (cons root id)
   default))

(define (put id v #:root [root '*root*])
  (hash-set!
   (current-vars)
   (cons root id)
   v))

(define (get/instance id [default (λ () (error 'get/instance "no value found for key ~s" id))])
  (hash-ref (current-instance-vars) default))

(define (put/instance id v)
  (hash-set! (current-instance-vars) id v))

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
