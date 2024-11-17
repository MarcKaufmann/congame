#lang conscript

(require conscript/survey-tools
         gregor
         #;congame-web/components/uploaded-file
         )

(provide
 phd-survey
 research-surveys)

(defvar* research-ideas research-ideas-id)
(defvar* research-proposals research-proposals-id)
(defvar* next-survey-date next-survey-date-id)
(defvar* research-proposal research-proposal-id)
(defvar* new-research-proposals new-research-proposals-id)
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
   (date 2024 10 3)
   (date 2024 10 10)
   (date 2024 10 24)
   (date 2024 11 7)
   (date 2024 11 14)
   (date 2024 11 21)
   (date 2024 11 28)
   (date 2024 12 5)))

(define (~ymd d)
  (~t d "EEEE, MMMM d"))

; FIXME: I'd much rather pass the date around as a local variable.
; But an argument to the step has to be available at compile time, right?
; Here our old way of passing things around was superior, given that now
; we fill the global namespace with variables like there's no tomorrow.
(defstep (get-research-proposal)
  @md{# Research Proposal

      @form{
            @div{
                 @set![research-proposal
                       (map-result
                        (input-file
                         "Provide your reseach proposal as a pdf."
                         #:required? #f
                         #:validators (list
                                       (lambda (b)
                                         (if b
                                             (valid-pdf? b)
                                             (cons 'ok b)))))
                        (lambda (b)
                          (if b
                              (upload-file! b)
                              b)))]}
            @submit-button}})

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

            @submit-button}})

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

(defstep (show-research-ideas)
  @md{# Research Ideas

      @`@(div
         ,@(for/list ([(k rs) (in-hash research-ideas)])
             (div
              (h2 (~ymd k))

              `@(ul
                 ,@(for/list ([r rs])
                     (li r))))))

      @button{Back}})


(defstep (show-research-proposals)
  @md{@style{
             .button.attachment-button {
                                        display: inline;
                                        padding: 0.3rem;
                                        }
             }

      # Research Proposals

      @`@(ul
          ,@(for/list ([(k v) (in-hash research-proposals)])
              (define fn
                (let ([fn0 (uploaded-file-filename v)])
                  (if (bytes? fn0)
                      (bytes->string/utf-8 fn0)
                      fn0)))
              (li
               (strong (format "~a:" (~ymd k)))
               (span (file-download/link v fn))
               #;(span (uploaded-file-attachment v fn)))))

      @br{}

      @button{Back}})

(define (get-new-rps)
  (define (update-one rp)
    (if (uploaded-file? rp)
        (hash
         'key (uploaded-file-key rp)
         'filename (uploaded-file-filename rp)
         'content-type (uploaded-file-content-type rp))
        rp))
  (for/hash ([(k v) (in-hash research-proposals)])
    (values k (update-one v))))

(define (update-rps)
  (define (update-one rp)
    (uploaded-file
     (hash-ref rp 'key)
     (hash-ref rp 'filename)
     (hash-ref rp 'content-type)))
  (for/hash ([(k v) (in-hash new-research-proposals)])
    (values k (update-one v))))

(defstep (wait-survey)
  (set! new-research-proposals (get-new-rps))
  ;(set! research-proposals (update-rps))
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

           The next survey only opens on @(~ymd (survey-start next-survey-date)).

           @button[#:to-step-id 'show-research-ideas]{Show Research Ideas}
           @button[(λ ()
                     (when (undefined? research-proposals)
                       (set! research-proposals (hash))))
                   #:to-step-id 'show-research-proposals]{Show Research Proposals}

           @(button
             (λ ()
               (set! research-proposal #f)
               (when (undefined? research-proposals)
                 (set! research-proposals (hash))))
             #:to-step-id 'get-research-proposal
             "Submit Research Proposal")}]))

(defstudy research-ideas-study
  [init --> get-research-ideas --> ,(λ () done)])

(defstep (no-more)
  @md{# No more surveys left

      You are done for now.})

(defstep (add-rp-if-submitted)
  (when research-proposal
    (set! research-proposals
          (hash-set
           research-proposals
           next-survey-date
           research-proposal)))
  (skip))

(defstudy research-surveys-base
  [initialize-surveys --> ,(lambda ()
                             (if (null? remaining-surveys)
                                 (goto no-more)
                                 (goto set-next-survey)))]

  [set-next-survey --> wait-survey
                   ; FIXME: we should skip when date is past and we resume
                   ; Use wrap-study for that.
                   --> research-ideas-study
                   --> ,(lambda ()
                          (set! surveys-answered
                                    (cons next-survey-date surveys-answered))
                          (put/identity 'surveys-answered surveys-answered)
                          (if (null? remaining-surveys)
                              (goto no-more)
                              (goto set-next-survey)))]

  [get-research-proposal --> add-rp-if-submitted --> wait-survey]
  [no-more --> no-more]
  [show-research-ideas --> wait-survey]
  [show-research-proposals --> wait-survey])

(define ((check-not-identity-user s))
  (cond [#t ;(current-participant-identity-user?)
         (s)]

        [else
         @md{# You are logged in on the wrong server

             Please log in on the identity server. You cannot continue with this account.}]))

(define research-surveys
  (map-study
   research-surveys-base
   check-not-identity-user))

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
