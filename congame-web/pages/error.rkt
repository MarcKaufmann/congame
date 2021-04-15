#lang racket/base

(require koyo/haml
         koyo/http
         net/url
         web-server/http
         "../components/template.rkt")

(provide
 production-error-page)

(define (production-error-page req _e)
  (page
   #:show-nav? #f
   (haml
    (.container
     (:h1 "An error occurred")
     (:p "Please click the button below to attempt to resume the study.  If this fails, please contact us.")
     (:a.button
      ([:href (url->string (url-scrub (request-uri req)))])
      "Resume")))))
