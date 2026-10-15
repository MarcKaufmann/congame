#lang racket/base

(require json
         racket/file
         rackunit
         "../private/cost.rkt"
         "../private/util.rkt")

(define (event type [message #f])
  (string-append (jsexpr->string (hasheq 'type type 'message message)) "\n"))

(define (message cost)
  (hasheq 'role "assistant" 'usage (hasheq 'cost (hasheq 'total cost))))

(test-case "cost counts final messages once across repeated event representations"
  (define result
    (collect-pi-cost
     (open-input-string
      (string-append (event "agent_start")
                     (event "message_end" (message 0.12))
                     (event "turn_end" (message 0.12))
                     (event "message_update" (message 0.12))
                     (event "message_end" (message 0.03))
                     (event "agent_end" (message 0.15))))))
  (check-= (hash-ref result 'total_cost_usd) 0.15 0.000001)
  (check-equal? (hash-ref result 'reported_messages) 2)
  (check-equal? (hash-ref result 'status) "reported"))

(test-case "zero, missing usage, and interrupted retries stay distinct"
  (check-equal? (hash-ref (collect-pi-cost (open-input-string (event "message_end" (message 0))))
                          'total_cost_usd)
                0)
  (check-equal? (hash-ref (collect-pi-cost (open-input-string "plain output\n")) 'status)
                "unavailable")
  (define partial
    (collect-pi-cost
     (open-input-string
      (string-append (event "agent_start")
                     (event "message_end" (message 0.2))
                     (event "agent_start")
                     (event "message_end" (hasheq 'role "assistant"))
                     (event "agent_end")))))
  (check-equal? (hash-ref partial 'status) "partial")
  (check-equal? (hash-ref partial 'missing_usage_messages) 1)
  (check-= (hash-ref partial 'total_cost_usd) 0.2 0.000001))

(test-case "cost summary uses stored metadata without reading the transcript"
  (define root (make-temporary-file "cost-test-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (check-equal? (cost-summary root) "unavailable")
      (write-json-file (build-path root "cost.json")
                       (hasheq 'total_cost_usd 0.529 'status "reported"))
      (check-equal? (cost-summary root) "$0.5290 USD (reported estimate)"))
    (lambda () (delete-directory/files root))))
