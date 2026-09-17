#lang racket/base

(require rackunit
         "../private/pi-stream-view.rkt")

(define sample
  (string-append
   "{\"type\":\"session\",\"id\":\"session-1\"}\n"
   "{\"type\":\"agent_start\"}\n"
   "{\"type\":\"message_update\",\"assistantMessageEvent\":{\"type\":\"thinking_end\",\"content\":\"Checking\\n files\\n.\\n\"}}\n"
   "{\"type\":\"message_update\",\"assistantMessageEvent\":{\"type\":\"text_delta\",\"delta\":\"Done.\"}}\n"
   "{\"type\":\"message_update\",\"assistantMessageEvent\":{\"type\":\"text_delta\",\"delta\":\"\\n\\n\"}}\n"
   "{\"type\":\"message_update\",\"assistantMessageEvent\":{\"type\":\"toolcall_end\",\"toolCall\":{\"name\":\"bash\",\"arguments\":{\"command\":\"raco test\"}}}}\n"
   "{\"type\":\"tool_execution_end\",\"toolName\":\"bash\",\"result\":{\"content\":[]},\"isError\":false}\n"
   "{\"type\":\"turn_end\",\"message\":{\"usage\":{\"input\":10,\"output\":4,\"reasoning\":2,\"cacheRead\":3,\"cost\":{\"total\":0.01234}}}}\n"
   "{\"type\":\"agent_end\"}\n"))

(test-case "compact Pi stream rendering and raw copying"
  (define rendered (open-output-string))
  (define copied (open-output-string))
  (render-pi-stream (open-input-string sample)
                    rendered
                    #:copy-to copied
                    #:color? #f)
(check-equal? (get-output-string copied) sample)

(define whitespace-loop
  (apply
   string-append
   (for/list ([_index (in-range 32)])
     "{\"type\":\"message_update\",\"assistantMessageEvent\":{\"type\":\"thinking_delta\",\"delta\":\"\\n\\n\"}}\n")))
(define loop-view (open-output-string))
(render-pi-stream (open-input-string whitespace-loop)
                  loop-view
                  #:color? #f)
(check-regexp-match
 #rx"warning: 32 consecutive whitespace-only thinking deltas"
 (get-output-string loop-view))
  (check-equal?
   (get-output-string rendered)
   (string-append
    "pi session session-1\n"
    "agent started\n"
    "\U0001f4ad Checking files.\n"
    "Done.\n"
    "\u25b6 bash  $ raco test\n"
    "\u2713 bash\n"
    "tokens \u219110 \u21934 R2 C3  $0.0123\n"
    "agent finished\n")))

(test-case "non-JSON lines remain visible"
  (define rendered (open-output-string))
  (render-pi-stream (open-input-string "plain harness output\n")
                    rendered
                    #:color? #f)
  (check-equal? (get-output-string rendered) "plain harness output\n"))

(test-case "follow mode renders lines written after an initial EOF"
  (define-values (input output) (make-pipe))
  (define rendered (open-output-string))
  (define worker
    (thread
     (lambda ()
       (render-pi-stream input rendered
                         #:color? #f
                         #:follow? #t
                         #:poll-seconds 0.01))))
  (displayln "{\"type\":\"agent_start\"}" output)
  (flush-output output)
  (let wait-for-output ([attempts 100])
    (unless (or (regexp-match? #rx"agent started" (get-output-string rendered))
                (zero? attempts))
      (sleep 0.01)
      (wait-for-output (sub1 attempts))))
  (kill-thread worker)
  (close-output-port output)
  (close-input-port input)
  (check-equal? (get-output-string rendered) "agent started\n"))
