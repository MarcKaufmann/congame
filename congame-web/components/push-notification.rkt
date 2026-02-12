#lang racket/base

(require base64
         crypto/libcrypto
         crypto/vapid
         crypto/web-push
         json
         koyo/database
         koyo/guard
         net/http-easy
         racket/contract/base
         racket/promise
         threading
         (prefix-in config: "../config.rkt")
         "user.rkt")

(provide
 (contract-out
  [send-push-notification
   (->* [user? string?]
        [#:title (or/c #f string?)
         #:data jsexpr?]
        void?)]))

(define (base64-urldecode str)
  (base64-decode #:endcodes 'url str))

(define pk-promise
  (delay/sync
   (define public-bs (base64-urldecode config:vapid-public-key))
   (define private-bs (base64-urldecode config:vapid-private-key))
   (vapid-key-data->pk public-bs private-bs libcrypto-factory)))

(define (vapid-auth url headers params)
  (define pk (force pk-promise))
  (define subject (format "mailto:~a" config:support-email))
  (define authorization-hdr (format "WebPush ~a" (make-vapid-token pk url #:sub subject)))
  (define crypto-key-hdr (format "p256ecdsa=~a" config:vapid-public-key))
  (values
   (~> headers
       (hash-set 'authorization authorization-hdr)
       (hash-set 'crypto-key crypto-key-hdr))
   params))

(define ((aes128gcm-payload auth-secret ua-public message) headers)
  (define-values (in out)
    (make-pipe))
  (thread
   (lambda ()
     (web-push-encrypt
      (open-input-string message) out
      #:auth-secret auth-secret
      #:user-agent-key ua-public
      #:factories libcrypto-factory)
     (close-output-port out)))
  (values (hash-set headers 'content-encoding "aes128gcm") in))

(define (send-push-notification
         #:title [title #f]
         #:data [data (hasheq)]
         u message)
  (with-guard void
    (define keys (guard (if-null (user-push-keys u) #f)))
    (define auth-secret (base64-urldecode (guard (hash-ref keys 'auth #f))))
    (define ua-public (base64-urldecode (guard (hash-ref keys 'p256dh #f))))
    (define payload
      (~> (hasheq 'body message 'data data)
          (when~> title
            (hash-set 'title title))
          (jsexpr->string)))
    (define res
      (post
       #:auth vapid-auth
       #:data (aes128gcm-payload auth-secret ua-public payload)
       #:headers (hasheq 'TTL "3600")
       (guard (if-null (user-push-endpoint u) #f))))
    (unless (= (response-status-code res) 201)
      (error 'send-push-notification
             "request failed~n status code: ~s~n body: ~.s"
             (response-status-code res)
             (response-body res)))))
