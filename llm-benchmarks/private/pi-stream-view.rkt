#lang racket/base

(require json
         racket/format
         racket/string)

(provide render-pi-stream)

(struct viewer (out
                color?
                [line-start? #:mutable]
                [pending-whitespace #:mutable]
                [whitespace-thinking-count #:mutable]))

(define (href value key [default #f])
  (if (hash? value) (hash-ref value key default) default))

(define (style view code text)
  (if (viewer-color? view)
      (string-append "\u001b[" code "m" text "\u001b[0m")
      text))

(define (emit! view text)
  (unless (string=? text "")
    (display text (viewer-out view))
    (set-viewer-line-start?! view (string-suffix? text "\n"))
    (flush-output (viewer-out view))))

(define (ensure-line-start! view)
  (unless (viewer-line-start? view)
    (emit! view "\n")))

(define (emit-line! view text [color #f])
  ;; Whitespace-only text immediately before a structured event is Pi's block
  ;; separator, not useful terminal output.
  (set-viewer-pending-whitespace! view "")
  (ensure-line-start! view)
  (emit! view (string-append (if color (style view color text) text) "\n")))

(define (emit-text-delta! view text)
  (cond
    [(regexp-match? #px"^\\s*$" text)
     (set-viewer-pending-whitespace!
      view
      (string-append (viewer-pending-whitespace view) text))]
    [else
     (emit! view (viewer-pending-whitespace view))
     (set-viewer-pending-whitespace! view "")
     (emit! view text)]))

(define (truncate text [limit 600])
  (if (> (string-length text) limit)
      (string-append (substring text 0 limit) "\u2026")
      text))

(define (tool-summary tool-call)
  (define name (~a (href tool-call 'name "tool")))
  (define arguments (href tool-call 'arguments #hasheq()))
  (define detail
    (cond
      [(and (hash? arguments) (hash-ref arguments 'command #f))
       (string-append "$ " (~a (hash-ref arguments 'command)))]
      [(and (hash? arguments) (hash-ref arguments 'path #f))
       (~a (hash-ref arguments 'path))]
      [else
       (with-handlers ([exn:fail? (lambda (_e) (~a arguments))])
         (jsexpr->string arguments))]))
  (format "\u25b6 ~a  ~a" name (truncate detail)))

(define (result-text result)
  (string-join
   (for/list ([item (in-list (href result 'content null))]
              #:when (and (hash? item)
                          (equal? (href item 'type) "text")))
     (~a (href item 'text "")))
   "\n"))

(define (format-cost value)
  (~r (if (number? value) value 0) #:precision '(= 4)))

(define (normalize-thinking text)
  ;; Some OpenRouter reasoning streams preserve a newline event between nearly
  ;; every token. Thinking is a compact diagnostic view, so collapse all of its
  ;; whitespace while retaining punctuation. Final assistant text is never
  ;; normalized.
  (regexp-replace*
   #px"\\s+([,.;:!?])"
   (string-trim (regexp-replace* #px"\\s+" text " "))
   "\\1"))

(define (render-event! view event)
  (define event-type (href event 'type ""))
  (define update (href event 'assistantMessageEvent #hasheq()))
  (define update-type (href update 'type ""))
  (unless (and (equal? event-type "message_update")
               (equal? update-type "thinking_delta"))
    (set-viewer-whitespace-thinking-count! view 0))
  (cond
    [(equal? event-type "session")
     (emit-line! view
                 (format "pi session ~a" (href event 'id "unknown"))
                 "1;34")]
    [(equal? event-type "agent_start")
     (emit-line! view "agent started" "1;34")]
    [(equal? event-type "agent_end")
     (emit-line! view "agent finished" "1;34")]
    [(equal? event-type "auto_retry_start")
     (emit-line! view
                 (format "retrying: ~a" (href event 'errorMessage "provider error"))
                 "1;33")]
    [(equal? event-type "auto_retry_end")
     (emit-line! view "retry finished" "1;33")]
    [(equal? event-type "message_update")
     (cond
       [(equal? update-type "thinking_delta")
        (define delta (~a (href update 'delta "")))
        (cond
          [(and (not (string=? delta ""))
                (string=? (string-trim delta) ""))
           (define count (add1 (viewer-whitespace-thinking-count view)))
           (set-viewer-whitespace-thinking-count! view count)
           ;; A warning at powers of two stays useful even for an old log with
           ;; tens of thousands of malformed events, without flooding output.
           (when (and (>= count 32)
                      (zero? (bitwise-and count (sub1 count))))
             (emit-line!
              view
              (format "warning: ~a consecutive whitespace-only thinking deltas"
                      count)
              "1;33"))]
          [else (set-viewer-whitespace-thinking-count! view 0)])]
       [(equal? update-type "thinking_end")
        (define content (normalize-thinking (~a (href update 'content ""))))
        (unless (string=? content "")
          (emit-line! view (string-append "\U0001f4ad " content) "2"))]
       [(equal? update-type "text_delta")
        (emit-text-delta! view (~a (href update 'delta "")))]
       [(equal? update-type "toolcall_end")
        (emit-line! view (tool-summary (href update 'toolCall #hasheq())) "1;36")]
       [else (void)])]
    [(equal? event-type "tool_execution_end")
     (define failed? (href event 'isError #f))
     (define tool-name (~a (href event 'toolName "tool")))
     (emit-line! view
                 (format "~a ~a" (if failed? "\u2717" "\u2713") tool-name)
                 (if failed? "1;31" "1;32"))
     (when failed?
       (define details (string-trim (result-text (href event 'result #hasheq()))))
       (unless (string=? details "")
         (emit-line! view (truncate details 1200) "31")))]
    [(equal? event-type "turn_end")
     (define usage (href (href event 'message #hasheq()) 'usage #hasheq()))
     (define cost (href (href usage 'cost #hasheq()) 'total 0))
     (emit-line!
      view
      (format "tokens \u2191~a \u2193~a R~a C~a  $~a"
              (href usage 'input 0)
              (href usage 'output 0)
              (href usage 'reasoning 0)
              (href usage 'cacheRead 0)
              (format-cost cost))
      "2")]
    [else (void)]))

(define (render-line! view line)
  (with-handlers ([exn:fail?
                   (lambda (_e)
                     (unless (string=? (string-trim line) "")
                       (emit-line! view line "2")))])
    (render-event! view (string->jsexpr line))))

(define (render-pi-stream input [output (current-output-port)]
                          #:copy-to [copy-output #f]
                          #:color? [color? (terminal-port? output)]
                          #:follow? [follow? #f]
                          #:poll-seconds [poll-seconds 0.25])
  (define view (viewer output color? #t "" 0))
  (let loop ()
    (define line (read-line input 'any))
    (cond
      [(eof-object? line)
       (cond
         [follow?
          (sleep poll-seconds)
          (loop)]
         [else
          (set-viewer-pending-whitespace! view "")
          (unless (viewer-line-start? view)
            (emit! view "\n"))
          (when copy-output (flush-output copy-output))])]
      [else
       (when copy-output
         (displayln line copy-output))
       (render-line! view line)
       (loop)])))
