#lang conscript

(provide
 phd-survey
 research-ideas-study)

(defvar* research-ideas research-ideas-id)

(define (input-research-idea i)
  (textarea (format "Research Idea ~a" i)))

(defstep (init)
  (when (undefined? research-ideas)
    (set! research-ideas '()))
  (skip))

(defstep (get-research-ideas)
  @md{# Research Ideas

      @form{
            @div{
                 @set![research-ideas
                       (input-list
                        (for/list ([i (in-range 5)])
                          (input-research-idea (add1 i))))]}

            @submit-button
       }})

(defstep (the-end)
  @md{# Thank you

      Thanks for participating.

      Your ideas were: @(format "~.s" research-ideas)
      })

(defstudy research-ideas-study
  [init --> get-research-ideas --> ,(λ () done)])

(defstep (welcome)
  @md{# Welcome

      This is a short survey about your PhD plans and expectations.

      @button{Next}})

(define (input-percentage)
  (input-number #:min 0 #:max 100))

(defvar p-finish)
(defvar p-4year)
(defvar p-academic)
(defvar p-phd-job)

(defvar phd-year)
(defvar n-research-projects)

(defstep (status)
  @md{# Status

      @form{
            @div{
                 What year of the PhD program are you in? @set![phd-year (input-number #:min 1)]}

            @submit-button}})

(defstep (research-status)
  @md{# Research

      @form{
            @div{
                 How many active research projects do you have? An active research project is one with a reasonably well-defined research question, so that you could include it in a grant proposal, and one that you are working on or will be working on in the next year.
                 @set![n-research-projects (input-number #:min 0)]}
            @submit-button}})

(defstep (phd-expectations)
  @md{# PhD Expectations

      @form{
            @div{For each of the statements below, report the probability in percentage points (%) that the event occurs.}
            @br{}
            @div{
                You will finish your PhD: @set![p-finish (input-percentage)]%}

            @div{
                 You will graduate within 4 years or less: @set![p-4year (input-percentage)]%}

            @div{
                 You will get an academic job after your PhD (postdoc, tenure track, etc): @set![p-academic (input-percentage)]%}

            @div{
                 You will get a job that requires a PhD: @set![p-phd-job (input-percentage)]%}

            @submit-button}})

(defstep (past-work)
  ; FIXME: Fill in with questions about work life balance
  (skip))

(defvar phd-goal)

(defstep (phd-reasons)
  @md{# PhD Motivations

      @form{
            @div{
              @set![phd-goal @textarea{What was your main reason for starting a PhD?}]}

            @submit-button}})

(defstudy phd-survey
  [welcome --> status
           --> phd-expectations
           --> research-status
           --> phd-reasons
           --> ,(lambda ()
                  (if (> phd-year 1)
                      (goto past-work)
                      (goto the-end)))]

  [past-work --> research-ideas-study --> the-end]
  [the-end --> the-end])
