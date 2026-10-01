#lang racket/base

(require racket/port
         "stream-watchdog.rkt")

(provide run-guarded-command)

(define watchdog-exit-code 86)

(define (run-guarded-command executable arguments
                             _input output error-output
                             #:limit [limit
                                      (configured-whitespace-delta-limit)]
                             #:grace-seconds [grace-seconds 2])
  ;; Own the child's stdout pipe instead of placing the watchdog downstream in
  ;; a shell pipeline. When the guard fires, the read end stays open until the
  ;; child has been stopped, so Node never writes into a closed pipe (EPIPE).
  (define-values (process child-output child-input child-error)
    (apply subprocess #f #f #f executable arguments))
  (close-output-port child-input)
  (define error-drain
    (thread
     (lambda ()
       (copy-port child-error error-output)
       (flush-output error-output))))
  (define stopped-by-watchdog?
    (guard-pi-stream child-output output error-output #:limit limit))
  (cond
    [stopped-by-watchdog?
     ;; Keep draining while first asking Pi to stop gracefully. This lets it
     ;; persist an aborted turn that the next invocation can resume. Escalate
     ;; only if Pi does not honor the interrupt promptly.
     (define output-drain
       (thread
        (lambda ()
          (copy-port child-output output)
          (flush-output output))))
     (subprocess-kill process #f)
     (unless (sync/timeout grace-seconds process)
       (subprocess-kill process #t))
     (subprocess-wait process)
     (thread-wait output-drain)
     (thread-wait error-drain)
     watchdog-exit-code]
    [else
     (subprocess-wait process)
     (thread-wait error-drain)
     (define status (subprocess-status process))
     (if (exact-integer? status) status 1)]))

(module+ main
  (define arguments (vector->list (current-command-line-arguments)))
  (when (null? arguments)
    (eprintf "usage: pi-supervisor.rkt PROGRAM [ARG ...]\n")
    (exit 2))
  (define executable (find-executable-path (car arguments)))
  (unless executable
    (eprintf "pi supervisor: executable not found: ~a\n" (car arguments))
    (exit 127))
  (exit
   (run-guarded-command executable
                        (cdr arguments)
                        (current-input-port)
                        (current-output-port)
                        (current-error-port))))
