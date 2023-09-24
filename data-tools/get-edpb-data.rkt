#lang racket/base

(require racket/format
         racket/list
         racket/string
         gregor
         gregor/period
         "get-data.rkt")

(define reasons-study-id 17)
(define reasons-iid 44)
(define api-url "https://totalinsightmanagement.com/api/v1/")
(define reasons-path
"/Users/kaufmannm/research/excuse-driven-present-bias/reasoned-excuses/data/reasons-pilot")

(define vars-to-keep '(study-id
                       instance-id
                       participant-id
                       submission
                       ))

(define reasons-data (parameterize ([current-api-url api-url])
                   (get-participants-data/list
                    reasons-study-id
                    reasons-iid)))

(define (filter-stack st lov)
  (filter (lambda (v)
            (equal? (hash-ref v 'stack) st))
          lov))

;(define (->root-data lov)
;  (define root-only
;    (filter-stack '("*root*") lov))
;  (for/hash ([v root-only])
;    ; FIXME: assumes that the id uniquely identifies and names a value.
;    ; In reality, id + group + round uniquely identify.
;    ()
;    )

(define (->topics p)
  (define vars (hash-ref p 'vars))
  (define root-vars
    (for/hash ([var (filter-stack '("*root*") vars)])
      (values (string->symbol (hash-ref var 'id))
              (hash-ref var 'value))))

  (define pid (hash-ref p 'participant-id))
  (for/list ([topic (hash-ref root-vars 'topics '())]
             [i (in-naturals)])
    (list pid topic (add1 i))))

  ;(id->value root-vars '(("consent?" . "consent")
  ;                       ("prolific-id" . "prolific_id")
  ;                       ("topics" . "topics")))
  ;(hash 'pid (hash-ref p 'participant-id)
  ;      'sid (hash-ref p 'study-id)
  ;      'iid (hash-ref p 'instance-id)
  ;

(define (topics-table ps)
  (cons
   (list "pid" "topic" "topic_order")
   (foldl append '() (map ->topics ps))))

(define (->meta p)
  (define root-vars
    (for/hash ([var (filter-stack '("*root*") (hash-ref p 'vars))])
      (values (string->symbol (hash-ref var 'id))
              (hash-ref var 'value))))

  (list (hash-ref p 'participant-id)
        (hash-ref p 'study-id)
        (hash-ref p 'instance-id)
        (hash-ref root-vars 'consent? null)
        (hash-ref root-vars 'prolific-id "")))

(define (meta-table ps)
  (cons
   (list "pid" "sid" "iid" "consent" "prolific_id")
   (map ->meta ps)))

(define questions
  (list
   "controversial"
   "familiar"
   "heard-of-last-week"
   "how-often-talk-colleagues"
   "how-often-talk-family"
   "important"
   "interesting"
   "learn-more"
   "understandable"))

(define (->opinions p)
  (define pid (hash-ref p 'participant-id))
  (define opinion-vars
    (filter (lambda (v)
              (member (hash-ref v 'id) questions))
            (hash-ref p 'vars)))
  (for/list ([v opinion-vars])
    (list pid
          (second (hash-ref v 'round))
          (hash-ref v 'id)
          (hash-ref v 'value))))

(define (opinions-table ps)
  (cons
   (list "pid" "topic" "question" "answer")
   (foldl append '() (map ->opinions ps))))

(define (frm a)
  (define (escape s)
    (format "\"~a\"" (string-replace a "\"" "\\\"")))
  (cond [(string? a) (escape a)]
        [(symbol? a) (frm (symbol->string a))]
        [(boolean? a) (if (equal? a #f) "FALSE" "TRUE")]
        [(and (list? a) (empty? a)) "NA"]
        [else (~a a)]))

(define (write-data tbl path)
  (call-with-output-file path
    (lambda (out)
      (for ([row tbl])
        (displayln (string-join (map frm row) ",") out)))))

(define (get-and-write-reasons-pilot-data)
  (write-data (meta-table reasons-data) (build-path reasons-path "participants.csv"))
  (write-data (topics-table reasons-data) (build-path reasons-path "topics.csv"))
  (write-data (opinions-table reasons-data) (build-path reasons-path "opinions.csv")))

(define edpb-pilot-study-id 19)
(define edpb-pilot-iid 46)
(define edpb-pilot-path
"/Users/kaufmannm/research/excuse-driven-present-bias/reasoned-excuses/data/edpb-pilot")

(define edpb-pilot-data
  (parameterize ([current-api-url api-url])
                   (get-participants-data/list
                    edpb-pilot-study-id
                    edpb-pilot-iid)))

(define p1 (car edpb-pilot-data))
(define ps (take edpb-pilot-data 2))

(define (get-root-ids p ids #:root [root "*root*"])
  (define vars-to-get
    (filter (lambda (v)
              (define stack (hash-ref v 'stack))
              (and (member (hash-ref v 'id) ids)
                   (equal? (length stack) 1)
                   (equal? (car stack) root)))
            (hash-ref p 'vars)))
  vars-to-get)

;; Avg for the first 15 participants was around 15 minutes for the main study
(define (main-session-time p)
  (define pid
    (hash-ref p 'participant-id))
  (define vars
    (get-root-ids p '("consent-given?" "feedback")))
  (cond [(= (length vars) 2)
         (define consent (car vars))
         (define feedback (cadr vars))
         (define consent-time
           (iso8601->datetime (hash-ref consent 'first-put-at)))
         (define feedback-time
           (iso8601->datetime (hash-ref feedback 'first-put-at)))
         (list pid (period-ref (period-between consent-time feedback-time) 'minutes))]

        [else
         #f]))

(define (->abstracts-time p)
  (define pid
    (hash-ref p 'participant-id))
  (define as
    (let ([abstracts-maybe (get-root-ids p #:root "*timer*" '("abstracts-duration"))])
      (case (length abstracts-maybe)
        [(0) null]
        [(1) (hash-ref (car abstracts-maybe) 'value)])))
  (define n (length as))
  (for/list ([a (in-list as)]
             [i (in-naturals)])
    ; pid  |  time in seconds  | correctly categorized? |  rank (the how manyeth abstract this was that they did)
    (list pid (last a) (third a) (- n i))))

(define (all-abstract-times ps)
  (cons
   (list "pid" "time_in_seconds" "is_correct" "order")
   (foldl append '() (map ->abstracts-time ps))))

(define (write-edpb-pilot ps)
  (write-data (all-abstract-times ps) (build-path edpb-pilot-path "abstract-times.csv")))

;(define ids-to-get
;  '(("*root*" "prolific-ID")
;    ("*root*" "patience")
;    ("*root*" "risk")
;  ("*root*" "tutorial-abstracts")
;  ("*root*" "tutorial-n")
;  ("*root*" "tutorial-category")
;  ("*round*" "round-stack")
;  ("*timer*" "start-time")
;  ("*timer*" "end-time")
;  ("*timer*" "abstracts-duration")
;  ("*abstracts*" "tutorial-correct-answers")
;  ("*root*" "completed-answers")
;  ("*root*" "abstract-task-score")
;  ("*root*" "attempt")
;  ("*root*" "last-attempt")
;  ("*root*" "comprehension-test-score")
;  ("*round*" "round-stack")
;  ("*root*" "pass-test?")
;  ("*root*" "pass-comprehension-test?")
;  ("*root*" "consent-given?")
;  ("*root*" "entered-completion-code?")
;  ("*root*" "choices-to-make")
;  ("*root*" "remaining-choices")
;  ("*root*" "total")
;  ("*root*" "choice-number")
;  ("*root*" "reasons")
;  ("*timer*" "start-time")
;  ("*timer*" "end-time")
;  ("*timer*" "choice-durations")
;  ("*root*" "work-choices")
;  ("*timer*" "switch-choice-durations")
;  ("*timer*" "reveal-reasons-durations")
;  ("*root*" "choices-made")
;  ("*root*" "choice-that-counts")
;  ("*root*" "additional-n")
;  ("*root*" "additional-category")
;  ("*root*" "additional-non-category")
;  ("*root*" "feedback")
;  ("*root*" "how-choose-reason")
;  ("*root*" "how-long-abstracts")
;  ("*root*" "how-meaningful-reasons")
;  ("*root*" "how-much-did-reasons-affect-choices")
;  ("*root*" "baseline-abstracts")
;  ("*root*" "baseline-n")
;  ("*root*" "baseline-category")
;  ("*round*" "round-stack")
;  ("*timer*" "start-time")
;  ("*timer*" "end-time")
;  ("*abstracts*" "baseline-incorrect-answers")
;  ("*root*" "completed-answers")
;  ("*abstracts*" "baseline-correct-answers")
;  ("*root*" "additional-work-abstracts")
;  ("*root*" "additional-work-n")
;  ("*root*" "additional-work-category")
;  ("*round*" "round-stack")
;  ("*timer*" "start-time")
;  ("*timer*" "end-time")
;  ("*abstracts*" "additional-work-correct-answers")
;  ("*root*" "completed-answers")
;  ("*abstracts*" "additional-work-incorrect-answers")
;  ("*root*" "boring")
;  ("*root*" "careful")
;  ("*root*" "understandable")
;  ("*root*" "feedback")
;  ("*root*" "how-choose"))
