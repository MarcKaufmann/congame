#lang racket/base

(require congame-web/components/template
         koyo/haml
         racket/contract
         web-server/http)

(provide
 dashboard-page)

(define/contract (dashboard-page _req)
  (-> request? response?)
  (page
   (haml
    (.container
     (:h1 "Hello World!")))))
