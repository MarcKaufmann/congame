#lang conscript

(require conscript/survey-tools)

(provide
  how-tos)

(defstep (start)
  @md{
      @button[#:to-step-id 'links]{How to add links}

      @button[#:to-step-id 'payments]{How to display payments}
      })

(defstep (links)
  @md{# Links

      A link to the @a[#:href "https://docs.totalinsightmanagement.com/Conscript_Tutorial.html"]{Conscript Tutorial}.

      Sometimes you want a link to open in a new tab, so you provide the attribute `target` with the value `"_blank"`:

      @a[
        #:href "https://docs.totalinsightmanagement.com/Conscript_Tutorial.html"
        #:target "_blank"
      ]{Open conscript tutorial in new tab}

      @button{Next}})

(define (payments)
  (define bonus 4.25)
  @md{# Bonus

      Your bonus is @(~$ bonus).

      @button{Next}
  })

(defstudy how-tos
  [start --> start]
  [links --> start]
  [payments --> start])
