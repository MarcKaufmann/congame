#lang racket/base

(require json
         racket/format
         racket/string
         "util.rkt")

(provide
 collect-pi-cost
 cost-summary
 write-run-cost!)

(define (href value key [default #f])
  (if (hash? value) (hash-ref value key default) default))

(define (collect-pi-cost input)
  (define total 0)
  (define reported 0)
  (define missing 0)
  (define active? #f)
  (define partial? #f)
  (for ([line (in-lines input)])
    (define event
      (with-handlers ([exn:fail?
                       (lambda (_e)
                         (when (string-prefix? (string-trim line) "{")
                           (set! partial? #t))
                         #f)])
        (string->jsexpr line)))
    (case (href event 'type)
      [("agent_start")
       (when active? (set! partial? #t))
       (set! active? #t)]
      [("agent_end") (set! active? #f)]
      [("message_end")
       (define message (href event 'message))
       (when (equal? (href message 'role) "assistant")
         ;; The same usage also occurs in turn_end and agent_end. Count only
         ;; message_end, including failed attempts that report a cost.
         (define cost (href (href (href message 'usage) 'cost) 'total))
         (cond
           [(and (real? cost) (<= 0 cost) (< cost +inf.0))
            (set! total (+ total cost))
            (set! reported (add1 reported))]
           [else (set! missing (add1 missing))]))]))
  (hasheq
   'schema_version 1
   'source "pi-message-usage"
   'currency "USD"
   'total_cost_usd (and (positive? reported) total)
   'status (cond
             [(zero? reported) "unavailable"]
             [(or partial? active? (positive? missing)) "partial"]
             [else "reported"])
   'reported_messages reported
   'missing_usage_messages missing))

(define (write-run-cost! result-path harness)
  (define log-path (build-path result-path "stdout.log"))
  (define cost
    (if (and (equal? (href (href harness 'labels) 'harness) "pi")
             (file-exists? log-path))
        (call-with-input-file log-path collect-pi-cost)
        (hasheq 'schema_version 1 'source #f 'currency "USD"
                'total_cost_usd #f 'status "unavailable")))
  (write-json-file (build-path result-path "cost.json") cost)
  cost)

(define (cost-summary result-path)
  (define path (build-path result-path "cost.json"))
  (define cost (and (file-exists? path) (read-json-file path)))
  (define total (href cost 'total_cost_usd))
  (if (number? total)
      (format "$~a USD (~a)"
              (~r total #:precision '(= 4))
              (if (equal? (href cost 'status) "partial")
                  "partial reported estimate"
                  "reported estimate"))
      "unavailable"))
