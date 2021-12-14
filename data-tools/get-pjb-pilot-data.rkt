#lang racket/base

(require net/http-easy
         racket/format
         racket/hash
         racket/list
         racket/string)

(define base-api-url "https://totalinsightmanagement.com/api/v1/")

;; gets the list of instances of the study
(define (api-study-instances study-id)
  (string-append base-api-url
                 "studies/" (number->string study-id) "/"
                 "instances.json"))

;; gets the participant data for this instance of the study
(define ((api-instance/participant-data study-id) instance-id)
  (string-append base-api-url
                 "studies/" (number->string study-id) "/"
                 "instances/" (number->string instance-id) "/"
                 "participants.json"))

(define api-key (getenv "CONGAME_API_KEY"))

(define (get-study-instances study-id)
  (get (api-study-instances study-id)
       #:auth (lambda (url headers params)
                (values (hash-set headers 'authorization api-key) params))))

(define pjb-pilot-study-id 1)
; one of the instances was a pure pilot, so I drop it
(define pjb-pilot-instance-ids
  '(2 3 4 5 7 9))

(define (get-instance-participant-data study-id instance-id)
  (get ((api-instance/participant-data study-id) instance-id)
       #:auth (lambda (url headers params)
                (values (hash-set headers 'authorization api-key) params))))

(define (completed-study? p)
  (string=?
   (hash-ref
    (last (hash-ref p 'vars)) 'id)
   "completion-code"))

(define (get-participants-data instance-id)
  (define instance-data
    (let ([res (get-instance-participant-data 1 instance-id)])
      (and (equal? #"HTTP/1.1 200 OK" (response-status-line res))
           (response-json res))))
  (for/list ([p (hash-ref instance-data 'participants)])
    (hash-union (hash 'participant-id (hash-ref p 'participant-id)
                      'study-id (hash-ref p 'study-id)
                      'instance-id (hash-ref p 'instance-id)
                      'completed? (completed-study? p))
                (for/hash ([entry (hash-ref p 'vars)])
                  (values (string->symbol (hash-ref entry 'id)) entry)))))

(define vars-to-keep
  '(study-id
    instance-id
    participant-id
    completed?
    practice-tasks
    tutorial-fee
    required-tasks
    participation-fee
    relax-treatment
    tutorial-success?
    consent?
    rest-treatment
    (relaxation-survey classical-piano-motivating-score)
    (relaxation-survey classical-piano-relaxing-score)
    (relaxation-survey edm-motivating-score)
    (relaxation-survey edm-relaxing-score)
    (relaxation-survey guided-meditation-motivating-score)
    (relaxation-survey guided-meditation-relaxing-score)
    (relaxation-survey wave-sounds-motivating-score)
    (relaxation-survey wave-sounds-relaxing-score)
    (relaxation-survey own-track)
    (WTWs pl5)
    (WTWs pl8)
    (WTWs pl11)
    (WTWs pl15)
    (debrief-survey comments)
    (debrief-survey gender)
    (debrief-survey how-clear-were-instructions)
    (debrief-survey how-do-you-decide-on-extra-work)
    (debrief-survey how-relaxing)
    (debrief-survey other-restful-activity)
    (debrief-survey what-could-be-clearer)
    (debrief-survey work-factors fewer-extra-tasks)
    (debrief-survey work-factors have-more-time)
    (debrief-survey work-factors longer-break)
    (debrief-survey work-factors smaller-matrices)
    ))

(define var-labels
  (map (lambda (ks)
         (if (symbol? ks) ks (last ks)))
       vars-to-keep))

(define ((get-participant-data p) k)
  (cond [(or (equal? p #f) (equal? p '())) '()]
        [(symbol? k)
         (let ([res (hash-ref p k '())])
           (if (and (hash? res) (hash-has-key? res 'value))
               (hash-ref res 'value)
               res))]
        [(and (list? k)
              (empty? k))
         p]
        [(and (list? k)
              (not (empty? k)))
         ((get-participant-data ((get-participant-data p) (car k))) (cdr k))]))

(define (get-clean-data instance-id)
  (for/list ([p (get-participants-data instance-id)])
    (map (get-participant-data p) vars-to-keep)))

(define (frm a)
  (cond [(string? a) (format "\"~a\"" a)]
        [(symbol? a) (frm (symbol->string a))]
        [(boolean? a) (if (equal? a #f) "FALSE" "TRUE")]
        [(and (list? a) (empty? a)) "NA"]
        [else (~a a)]))

(define (get-all-instances-data)
  (for/fold ([all-data '()])
            ([iid pjb-pilot-instance-ids])
    (append (get-clean-data iid) all-data)))

(define (write-data data path)
  (call-with-output-file path
    (lambda (out)
      (displayln (string-join (map frm var-labels) ",") out)
      (for ([p data])
        (displayln (string-join (map frm p) ",") out)))))

;; FIXME: We fail to set some information on the participant for export that we need, such as the status
;; For now, assume that the steps are in order (FIXME: check this -- is there ambiguity with respect to first-put and last-put?) and check if final step has 'id "completion-code"
;; FIXME: Get the anonymized username for debugging
