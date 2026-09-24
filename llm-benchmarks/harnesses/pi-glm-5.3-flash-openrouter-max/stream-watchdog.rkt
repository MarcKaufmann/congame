#lang racket/base

(require json
         racket/string)

(provide configured-whitespace-delta-limit
         guard-pi-stream)

(define default-limit 128)

(define (configured-whitespace-delta-limit)
  (define raw (getenv "PI_WHITESPACE_DELTA_LIMIT"))
  (define parsed (and raw (string->number raw)))
  (if (exact-positive-integer? parsed) parsed default-limit))

(define (href value key [default #f])
  (if (hash? value) (hash-ref value key default) default))

(define (whitespace-thinking-delta? event)
  (define update (href event 'assistantMessageEvent #hasheq()))
  (define delta (href update 'delta #f))
  (and (equal? (href event 'type) "message_update")
       (equal? (href update 'type) "thinking_delta")
       (string? delta)
       (not (string=? delta ""))
       (string=? (string-trim delta) "")))

(define (json-line->event line)
  (with-handlers ([exn:fail? (lambda (_error) #f)])
    (string->jsexpr line)))

(define (guard-pi-stream input output error-output
                         #:limit [limit default-limit])
  (let loop ([consecutive-whitespace-deltas 0])
    (define line (read-line input 'any))
    (cond
      [(eof-object? line) #f]
      [else
       ;; Keep stdout byte-for-byte equivalent at the JSONL record level so the
       ;; runner can retain the original Pi stream for later inspection.
       (displayln line output)
       (flush-output output)
       (define next-count
         (if (whitespace-thinking-delta? (json-line->event line))
             (add1 consecutive-whitespace-deltas)
             0))
       (cond
         [(>= next-count limit)
          (fprintf
           error-output
           (string-append
            "GLM stream watchdog: stopped after ~a consecutive whitespace-only "
            "thinking deltas; retry this task (prefer a lower reasoning effort "
            "if it repeats).\n")
           limit)
          (flush-output error-output)
          #t]
         [else (loop next-count)])])))
