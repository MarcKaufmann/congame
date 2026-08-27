#!/usr/bin/env racket
#lang racket/base

(require racket/cmdline
         "../private/pi-stream-view.rkt")

(define color-mode 'auto)
(define follow? #f)

(define log-path
  (command-line
   #:program "view-pi-stream.rkt"
   #:once-each
   [("--color") "always emit ANSI colors" (set! color-mode 'always)]
   [("--no-color") "never emit ANSI colors" (set! color-mode 'never)]
   [("--tail" "-f") "keep following lines appended to the log"
    (set! follow? #t)]
   #:args (stdout-log)
   stdout-log))

(call-with-input-file log-path
  (lambda (input)
    (render-pi-stream
     input
     (current-output-port)
     #:follow? follow?
     #:color? (case color-mode
                [(always) #t]
                [(never) #f]
                [else (terminal-port? (current-output-port))]))))
