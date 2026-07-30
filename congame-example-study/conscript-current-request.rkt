#lang conscript

(require koyo/http)

(provide
 conscript-current-request-study)

(defstep (start)
  @md{# Current Request

      @(or (request-headers-ref* (current-request) #"host") "No Host header")})

(defstudy conscript-current-request-study
  [start --> start])
