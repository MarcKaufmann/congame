#lang conscript

(require gregor)

(provide
 phd-survey
 research-surveys)

(defvar* research-ideas research-ideas-id)
(defvar* next-survey-date next-survey-date-id)
(defvar remaining-surveys)
(defvar surveys-answered)

(define (input-research-idea i)
  (textarea (format "Research Idea ~a" i)))

(defstep (init)
  (when (undefined? research-ideas)
    (set! research-ideas (hash)))
  (skip))

(define survey-dates
  (list
   (date 2024 10 1)
   (date 2024 10 2)
   (date 2024 10 3)
   (date 2024 10 6)
   (date 2024 10 10)
   (date 2024 10 17)
   (date 2024 10 24)))

(define (~ymd d)
  (~t d "EEEE, MMMM d"))

(defstep (get-research-ideas)
  @md{# Research Ideas

      Deadline: @(~ymd next-survey-date)

      @form{
            @div{
                 @set![research-ideas
                       (map-result
                        (input-list
                         (for/list ([i (in-range 5)])
                           (input-research-idea (add1 i))))
                        (lambda (v)
                          (hash-set
                           research-ideas
                           next-survey-date
                           v)))]}

            @submit-button
       }})

(defstep (initialize-surveys)
  (define t (today))
  (set! surveys-answered '())
  (set! remaining-surveys
    (filter
     (lambda (d)
       (date>=? d t))
   survey-dates))
  (skip))

(defstep (set-next-survey)
  (set! next-survey-date (car remaining-surveys))
  (set! remaining-surveys (cdr remaining-surveys))
  (skip))

(defstep (wait-survey)
  (define t (today))
  (define (survey-start d)
    (-days d 6))
  (define (survey-open? d)
    (and (date<=? (survey-start d) t)
         (date<=? t d)))

  (define (survey-is-over? d)
    (date>? t d))

  (cond [(survey-open? next-survey-date)
         (skip)]

        [(survey-is-over? next-survey-date)
         (if (null? remaining-surveys)
             (skip 'no-more)
             (skip 'set-next-survey))]

        [else
         @md{# The next survey is not yet open

             The next survey only opens on @(~ymd (survey-start next-survey-date)).}]))

(defstudy research-ideas-study
  [init --> get-research-ideas --> ,(λ () done)])

(defstep (no-more)
  @md{# No more surveys left

      You are done for now.})

(defstudy research-surveys
  [initialize-surveys --> ,(lambda ()
                             (if (null? remaining-surveys)
                                 (goto no-more)
                                 (goto set-next-survey)))]
  [set-next-survey --> wait-survey
                   --> research-ideas-study
                   --> ,(lambda ()
                          (set! surveys-answered
                                (cons next-survey-date surveys-answered))
                          (put/identity 'surveys-answered surveys-answered)
                          (if (null? remaining-surveys)
                              (goto no-more)
                              (goto set-next-survey)))]

  [no-more --> no-more])


(defstep (the-end)
  @md{# Thank you

      Thanks for participating.

      Your ideas were: @(format "~.s" research-ideas)
      })

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
