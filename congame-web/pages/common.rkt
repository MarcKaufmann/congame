#lang racket/base

(require congame-web/components/template
         (prefix-in config: congame-web/config)
         koyo/flash
         koyo/haml
         koyo/http
         koyo/l10n
         net/url
         racket/contract
         racket/string
         web-server/http)

(provide
 error-413-page
 expired-page
 not-found-page)

(define/contract (not-found-page _req)
  (-> request? response?)
  (page
   #:subtitle (translate 'subtitle-not-found)
   #:status-code 404
   (haml
    (.container
     (:h1 (translate 'subtitle-not-found))
     (:p (translate 'message-not-found))))))

(define/contract (expired-page req)
  (-> request? response?)
  (define the-uri (url-scrub (request-uri req)))
  (define the-path (path->string (url->path the-uri)))
  (cond
    [(string-prefix? the-path "/study/")
     (page
      (haml
       (.container
        (:h1 "Page Expired")
        (:p "You've already moved on from this step."
            (:a.button
             ([:href (url->string the-uri)])
             "Click here to resume the study.")))))]
    [else
     (flash 'warning (translate 'message-session-expired))
     (redirect-to (url->string the-uri))]))

(define/contract ((error-413-page) _req)
  (-> (-> request? response?))
  (page
   #:subtitle (translate 'subtitle-file-too-large)
   (haml
    (.container
     (:h1 (translate 'subtitle-file-too-large)
          (format " (> ~a MB) " (/ config:http-max-file-size 1024 1024)))
     (:p (translate 'message-file-too-large-try-again))))))
