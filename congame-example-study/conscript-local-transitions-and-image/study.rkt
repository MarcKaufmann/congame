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
  (define (render-slider _idx name value)
    `(input
      ([name ,name]
       [type "range"]
       [min "0"]
       [max "100"]
       [value ,(or value "50")])))

  @md{# Step C

      @form{
        @checkbox[#:consent? #:required? #f]{Do you consent to continue?}
        @binding[#:sliders (make-sliders 5 render-slider)]
        @submit-button}})

(defstep (consented)
  @md{# Consented

      Thanks for consenting to participate. As a thanks, watch this image:

      @img[
        #:alt "Screenshot of Code"
        #:src (resource-uri images "code-screenshot.png")]

      @button{Restart at Step A}})

(defvar sliders)

(defstep (not-consented)
  @md{# Not Consented

      Sorry to see you leave.

      @~a[sliders]

      @button[#:to-step-id 'step-a]{Restart at Step A}})

(defstudy transition-study
  [step-a --> step-b --> step-c --> ,(lambda ()
                                       (if consent?
                                           'consented
                                           'not-consented))]
  [consented --> step-a]
  [not-consented --> not-consented])
