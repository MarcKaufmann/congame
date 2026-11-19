#lang racket/base

(require net/http-easy
         racket/class
         threading
         "message.rkt"
         "output.rkt"
         "provider.rkt")

(provide
 openrouter-provider%)

(define openrouter-provider%
  (class* object% (llm-provider<%>)
    (init api-key)
    (super-new)
    (define auth (bearer-auth api-key))
    (define/public (chat #:model model
                         #:system [system-prompt #f]
                         #:max-output-tokens [max-output-tokens 4096]
                         messages)
      (let ([messages (if (pair? messages) messages (list messages))])
        (define data
          (~> (hasheq
               'model model
               'input (map message->jsexpr messages)
               'max_output_tokens max-output-tokens)
              (when~> system-prompt
                (hash-set 'instructions system-prompt))))
        (~> (post
             #:auth auth
             #:json data
             "https://openrouter.ai/api/v1/responses")
            (check-response 'chat _)
            (response-json)
            (jsexpr->output))))))

(define (check-response who resp [ok '(200)])
  (unless (memv (response-status-code resp) ok)
    (error who "request failed~n status: ~s~n body: ~e"
           (response-status-code resp)
           (response-body resp)))
  resp)
