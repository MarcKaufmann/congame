#lang racket/base

(require racket/class
         racket/contract/base
         "message.rkt"
         "output.rkt")

(provide
 llm-provider<%>)

(define llm-provider<%>
  (interface ()
    [chat (->*m
           [#:model string?
            (or/c message? (listof message?))]
           [#:system string?
            #:max-output-tokens exact-nonnegative-integer?]
           output?)]))
