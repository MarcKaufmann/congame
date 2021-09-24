#lang racket/base

(require koyo/http
         koyo/session
         racket/contract
         racket/format
         racket/string
         web-server/http)

(provide
 wrap-prolific
 get-prolific-email
 prolific-username?)

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

(define/contract (prolific-username? username)
  (-> string? boolean?)
  (string-contains? username "@email.prolific.co"))
