#lang racket/base

(require racket/format
         racket/list
         racket/string
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

(define p1 (car reasons-data))
(define ps (take reasons-data 2))

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

(write-data (meta-table reasons-data) (build-path reasons-path "participants.csv"))
(write-data (topics-table reasons-data) (build-path reasons-path "topics.csv"))
(write-data (opinions-table reasons-data) (build-path reasons-path "opinions.csv"))
