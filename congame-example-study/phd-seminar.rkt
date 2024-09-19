#lang conscript

(provide
 phd-survey
 research-ideas-study)

(defvar r1)
(defvar r2)
(defvar r3)
(defvar r4)
(defvar r5)

(defvar* research-ideas research-ideas-id)

(define (input-research-idea i)
  (input-text (format "Research Idea ~a" i)))

(defstep (init)
  (when (undefined? research-ideas)
    (set! research-ideas '()))
  (skip))

(defstep (get-research-ideas)
  @md{# Research Ideas

      @form{
            @div{
                 @set![r1 (textarea "Research Idea 1")]}
            @div{
                 @set![r2 (textarea "Research Idea 2")]}
            @div{
                 @set![r3 (textarea "Research Idea 3")]}
            @div{
                 @set![r4 (textarea "Research Idea 4")]}
            @div{
                 @set![r5 (textarea "Research Idea 5")]}
            @submit-button
       }})

(defstep (collect-ideas)
  (set! research-ideas (list r1 r2 r3 r4 r5))
  (skip))

(defstep (the-end)
  @md{# Thank you

      Thanks for participating.})

(defstudy research-ideas-study
  [init --> get-research-ideas
        --> collect-ideas
        --> get-research-ideas])

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

  [past-work --> the-end]
  [the-end --> the-end])
