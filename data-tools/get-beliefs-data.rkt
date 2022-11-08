#lang racket/base

(require "get-data.rkt")

(define research-ideas-study-id 3)
(define research-ideas-1-iid 11)
(define api-url "https://totalinsightmanagement.com/api/v1/")
(define beliefs-path "/home/marc/Git/tanitas/beliefs/data")
(define vars-to-keep '(study-id
                       instance-id
                       participant-id
                       submission
                       ))

(define all-data (parameterize ([current-api-url api-url])
                   (get-instance-participant-data
                    research-ideas-study-id
                    research-ideas-1-iid)))

#;(get-and-write-data #:study-id research-ideas-study-id
                      #:iids (list research-ideas-1-iid)
                      #:api-url api-url
                      #:path (build-path beliefs-path "research-ideas-1.csv")
                      #:vars-to-keep vars-to-keep)
