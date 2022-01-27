#lang racket/base

(require koyo/flash
         koyo/haml
         koyo/http
         koyo/l10n
         net/url
         racket/contract
         web-server/http
         "../components/template.rkt"
         (prefix-in config: "../config.rkt"))

(provide
 error-413-page
 expired-page
 not-found-page)

(define/contract (not-found-page _req)
  (-> request? response?)
  (page
   #:subtitle (translate 'subtitle-not-found)
   (haml
    (.container
     (:h1 (translate 'subtitle-not-found))
     (:p (translate 'message-not-found))))))

(define/contract (expired-page req)
  (-> request? response?)
  (flash 'warning (translate 'message-session-expired))
  (redirect-to (url->string (url-scrub (request-uri req)))))

(define/contract ((error-413-page) _req)
  (-> (-> request? response?))
  (page
   #:subtitle (translate 'subtitle-file-too-large)
   (haml
    (.container
     (:h1 (translate 'subtitle-file-too-large)
          (format " (> ~a MB) " (/ config:http-max-file-size 1024 1024)))
     (:p (translate 'message-file-too-large-try-again))))))
