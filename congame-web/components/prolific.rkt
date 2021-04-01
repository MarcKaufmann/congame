#lang racket/base

(require koyo/http
         koyo/session
         racket/contract
         racket/format
         web-server/http)

(provide
 wrap-prolific
 get-prolific-email)

(define pid-session-key
  'prolific:pid)

(define/contract ((wrap-prolific hdl) req)
  (-> (-> request? response?)
      (-> request? response?))
  (define bindings (request-bindings/raw req))
  (define pid (bindings-ref bindings 'PROLIFIC_PID #f))
  (when pid (session-set! pid-session-key pid))
  (hdl req))

(define/contract (get-prolific-email)
  (-> string?)
  (cond
    [(session-ref pid-session-key #f)
     => (lambda (pid)
          (~a pid "@email.prolific.co"))]

    [else ""]))
