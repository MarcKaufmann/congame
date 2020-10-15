#lang racket/base

(require koyo/haml
         racket/contract
         web-server/http
         "../components/template.rkt")

(provide
 dashboard-page)

(define/contract (dashboard-page _req)
  (-> request? response?)
  (error "This is a Sentry test")
  (page
   (haml
    (.container
     (:h1 "Hello World!")))))
