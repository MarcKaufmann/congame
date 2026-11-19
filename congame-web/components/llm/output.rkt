#lang racket/base

(require json
         racket/contract/base
         "message.rkt")

(provide
 status/c
 (contract-out
  [struct output
    ([data jsexpr?]
     [usage usage?]
     [status status/c])]
  [struct usage
    ([input-tokens (or/c #f exact-nonnegative-integer?)]
     [output-tokens (or/c #f exact-nonnegative-integer?)]
     [total-tokens (or/c #f exact-nonnegative-integer?)])]
  [jsexpr->output
   (-> jsexpr? output?)]))

(define status/c
  (or/c 'queued 'in-progress 'cancelled 'completed 'incomplete 'failed))

(struct output
  (data usage status)
  #:transparent)

(struct usage
  (input-tokens output-tokens total-tokens)
  #:transparent)

(define (jsexpr->output data)
  (output
   #;data (map jsexpr->message (hash-ref data 'output null))
   #;usage (jsexpr->usage (hash-ref data 'usage))
   #;status (jsexpr->status (hash-ref data 'status))))

(define (jsexpr->usage data)
  (usage
   #;input-tokens (hash-ref data 'input_tokens #f)
   #;output-tokens (hash-ref data 'output_tokens #f)
   #;total-tokens (hash-ref data 'total_tokens #f)))

(define (jsexpr->status data)
  (string->symbol (regexp-replace* #rx"_" data "-")))
