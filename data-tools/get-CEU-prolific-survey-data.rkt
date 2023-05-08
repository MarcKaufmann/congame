#lang racket/base

(require "get-data.rkt")

(define study-id 15)
(define instance-id 42)
(define api-url "https://totalinsightmanagement.com/api/v1/")
(define data-path "/Users/kaufmannm/research/CEU-survey")

(define all-data (parameterize ([current-api-url api-url])
                   (get-instance-participant-data
                    study-id
                    instance-id)))

(define vars-to-keep
  '(study-id
    instance-id
    participant-id
    prolific-id
    country
    has-BA?
    when-got-BA
    BA-in-progress?
    computer-science
    data-science
    economics
    engineering
    humanities
    natural-science
    social-science
    other
    BA-location
    BA-africa
    BA-asia
    BA-australia
    BA-central-eastern-europe
    BA-north-america
    BA-south-america
    BA-western-europe
    study-abroad?
    consider-africa
    consider-asia
    consider-australia
    consider-central-eastern-europe
    consider-north-america
    consider-south-america
    consider-western-europe
    consider-scandinavia
    consider-english
    consider-german
    consider-italy
    consider-france
    consider-iberia
    consider-benelux
    favorite-city
    pass-attention-check?))

(get-and-write-data #:study-id study-id
                    #:iids (list instance-id)
                    #:api-url api-url
                    #:path (build-path data-path "prolific-results2.csv")
                    #:vars-to-keep vars-to-keep)
