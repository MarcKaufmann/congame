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
                  step-transition
                  study-name)
         koyo/continuation
         koyo/http
         koyo/url
         net/mime-type
         net/sendurl
         racket/async-channel
         racket/match
         racket/port
         racket/unit
         web-server/dispatch
         (only-in web-server/http header request-bindings/raw request-uri response? response/output)
         (only-in web-server/servlet send/back send/suspend/dispatch servlet-prompt)
         web-server/servlet-dispatch
         web-server/web-server
         (prefix-in conscript: "base.rkt")
         (except-in "base.rkt" defvar form)
         "congame-sig.rkt"
         "matchmaking-sig.rkt"
         "matchmaking-unit.rkt")

(provide
 (all-from-out "base.rkt"))


;; preview ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 preview)

(define (preview a-study [open-as-owner? #f])
  (define seq (box 0))
  (define port-or-exn-ch
    (make-async-channel))
  (define ((wrap-application-url handler) req)
    (parameterize ([current-application-url-host "127.0.0.1"]
                   [current-application-url-port port-or-exn])
      (handler req)))
  (define-values (app _reverse-uri)
    (dispatch-rules
     [("")
      (lambda (req)
        (define owner?
          (not (not (bindings-ref (request-bindings/raw req) 'owner))))
        (define participant-id
          (let loop ([old (unbox seq)])
            (if (box-cas! seq old (add1 old))
                (add1 old)
                (loop (unbox seq)))))
        (define manager
          (study-manager
           (make-study-participant
            #:id participant-id
            #:user-id participant-id
            #:instance-id participant-id)
           #f))
        (parameterize ([current-request req]
                       [current-participant-id participant-id]
                       [current-participant-owner? owner?]
                       [current-study-manager manager])
          (run-study a-study)))]
     [("dsl-resource" (integer-arg) (string-arg) ...)
      (lambda (_req _instance-id path-elements)
        (define path
          (apply build-path (current-directory) path-elements))
        (response/output
         #:mime-type (path-mime-type path)
         #:headers (list (header #"content-length" (string->bytes/utf-8 (number->string (file-size path)))))
         (lambda (out)
           (call-with-input-file path
             (lambda (in)
               (copy-port in out)
               (close-output-port out))))))]))
  (define stop
    (parameterize ([current-continuation-key-cookie-secure? #f])
      (serve
       #:port 0
       #:listen-ip "127.0.0.1"
       #:dispatch (dispatch/servlet
                   (wrap-application-url
                    (wrap-protect-continuations
                     app)))
       #:confirmation-channel port-or-exn-ch)))
  (define port-or-exn
    (sync port-or-exn-ch))
  (when (exn:fail? port-or-exn)
    (raise port-or-exn))
  (send-url (format "http://127.0.0.1:~a~a" port-or-exn (if open-as-owner? "?owner=x" "")))
  (with-handlers ([exn:break? void])
    (sync never-evt))
  (stop))

(define (run-study a-study [req (current-request)])
  (define paramz (current-parameterization))
  (parameterize ([current-vars (or (current-vars) (make-hash))]
                 [current-data (make-hash)]
                 [current-stack (cons
                                 (study-name a-study)
                                 (current-stack))])
    (let loop ([this-step (car (study-steps a-study))])
      (if (not this-step)
          `(continue ,paramz)
          (match (run-step this-step req)
            [(? response? res)
             (send/back res)]
            [`(to-step ,to-step-id ,paramz)
             (define next-step
               (find-step a-study to-step-id))
             (call-with-parameterization paramz
               (lambda ()
                 (loop next-step)))]
            [`(continue ,paramz)
             (redirect/get/forget/protect)
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
                    (loop next-step)))])])))))

(define (run-step a-step [req (current-request)])
  (define paramz #f)
  (call-with-current-continuation
   (lambda (return)
     (send/suspend/dispatch/protect
      (lambda (embed/url)
        (parameterize ([current-embed/url
                        (lambda (hdl)
                          (embed/url
                           (λ (req) ;; noqa
                             (call-with-parameterization
                               paramz
                               (lambda ()
                                 (parameterize ([current-request req])
                                   (hdl req)))))))]
                       [current-return return]
                       [current-request req])
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
 defvar
 defvar*
 defvar*/instance
 defvar/instance
 put/identity
 form
 call-with-study-transaction
 with-study-transaction
 current-participant-id
 current-participant-owner?
 make-matchmaker
 get-ready-groups
 get-current-group
 reset-current-group)

(define current-stack (make-parameter null))
(define current-vars (make-parameter #f))
(define current-instance-vars (make-parameter (make-hash)))
(define current-data (make-parameter #f))
(define current-participant-id (make-parameter #f))
(define current-participant-owner? (make-parameter #f))

(define-syntax (defvar stx)
  (syntax-parse stx
    [(_ id:id)
     #`(begin
         (define-syntax id
           (make-set!-transformer
            (lambda (stx)
              (syntax-case stx (set!)
                [(set! id v) #'(put-var 'id v)]
                [id (identifier? #'id) #'(get-var 'id)])))))]))

(define (get-var id)
  (hash-ref
   (current-data)
   (list (current-stack) id)
   undefined))

(define (put-var id v)
  (hash-set!
   (current-data)
   (list (current-stack) id)
   v))

(define-syntax (defvar/instance stx)
  (syntax-parse stx
    [(_ id:id)
     #`(begin
         (define-syntax id
           (make-set!-transformer
            (lambda (stx)
              (syntax-case stx (set!)
                [(set! id v) #'(put-var/instance 'id v)]
                [id (identifier? #'id) #'(get-var/instance 'id)])))))]))

(define (get-var/instance id)
  (hash-ref
   (current-instance-vars)
   (list (current-stack) id)
   undefined))

(define (put-var/instance id v)
  (hash-set!
   (current-instance-vars)
   (list (current-stack) id)
   v))

(define-syntax (defvar* stx)
  (syntax-parse stx
    [(_ id:id unique-id:id)
     #`(begin
         (define-syntax id
           (make-set!-transformer
            (lambda (stx)
              (syntax-case stx (set!)
                [(set! id v) #'(put-var* 'unique-id 'id v)]
                [id (identifier? #'id) #'(get-var* 'unique-id 'id)])))))]))

(define (get-var* uid k)
  (hash-ref
   (current-vars)
   (cons uid k)
   undefined))

(define (put-var* uid k v)
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
                [(set! id v) #'(put-var*/instance 'unique-id 'id v)]
                [id (identifier? #'id) #'(get-var*/instance 'unique-id 'id)])))))]))

(define (get-var*/instance uid k)
  (hash-ref
   (current-instance-vars)
   (cons uid k)
   undefined))

(define (put-var*/instance uid k v)
  (hash-set!
   (current-instance-vars)
   (cons uid k)
   v))

(define (put/identity k v)
  (put-var k v))

;; FIXME: Refactor formular to be parameterized over the current put
;; procedure to avoid duplicating some of these checks.
(define-syntax (form stx)
  (syntax-parse stx
    [(_ {~alt
         {~optional {~seq #:action action:expr}}
         {~optional {~seq #:bot bot}}
         {~optional {~seq #:fields fields}}} ...
        body ...+)
     #:attr default-action
     (if (let uses-set!? ([stx stx])
           (syntax-parse stx
             #:literals (set!)
             [(set! . _args) #t]
             [(rator rand ...) (ormap uses-set!? (cons #'rator (syntax-e #'(rand ...))))]
             [_ #f]))
         #f
         #'default-form-action)
     (let check-loop ([inner-stx #'(body ...)])
       (syntax-parse inner-stx
         #:literals (form)
         [(form . _args)
          (raise-syntax-error 'form "forms cannot be nested" stx inner-stx)]
         [(rator rand ...)
          (for-each check-loop (cons #'rator (syntax-e #'(rand ...))))]
         [_ (void)]))
     #'(conscript:form
        {~? {~@ #:action {~? action default-action}}}
        {~? {~@ #:bot bot}}
        {~? {~@ #:fields fields}}
        body ...)]))

(define default-form-action
  (make-keyword-procedure
   (lambda (kws kw-args . _args)
     (for ([kwd (in-list kws)]
           [arg (in-list kw-args)])
       (put-var (string->symbol (keyword->string kwd)) arg)))))

(define (call-with-study-transaction proc)
  (proc))

(define-syntax-rule (with-study-transaction body0 body ...)
  (call-with-study-transaction
   (lambda ()
     body0 body ...)))

(define-values/invoke-unit matchmaking@
  (import congame^)
  (export matchmaking^))

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
