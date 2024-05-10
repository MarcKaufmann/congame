#lang conscript/local

(provide
 transition-study)

(defvar consent?)

(define-static-resource images "img")

(defstep (step-a)
  @md{# Step A

      @button[#:to-step-id 'step-c]{Go to Step C}
      @button{Go to Step B}})

(defstep (step-b)
  @md{# Step B

      @button{Go to Step C}})

(defstep (step-c)
  @md{# Step C

      @form{
        @checkbox[#:consent? #:required? #f]{Do you consent to continue?}
        @submit-button}})

(defstep (consented)
  @md{# Consented

      Thanks for consenting to participate. As a thanks, watch this image:

      @img[
        #:alt "Screenshot of Code"
        #:src (resource-uri images "code-screenshot.png")]

      @button{Restart at Step A}})

(defstep (not-consented)
  @md{# Not Consented

      Sorry to see you leave.

      @button[#:to-step-id 'step-a]{Restart at Step A}})

(defstudy transition-study
  [step-a --> step-b --> step-c --> ,(lambda ()
                                       (if consent?
                                           'consented
                                           'not-consented))]
  [consented --> step-a]
  [not-consented --> not-consented])
