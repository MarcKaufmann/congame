#lang racket/base

(require racket/file
         racket/format
         racket/port
         racket/string)

(provide
 (struct-out command-result)
 make-environment
 run-command
 run-command/capture
 start-command
 stop-command-group!)

(struct command-result (exit-code timed-out? duration-seconds) #:transparent)

(define (make-environment [overrides #hasheq()] #:base [base (current-environment-variables)])
  (define env (environment-variables-copy base))
  (for ([(key value) (in-hash overrides)])
    (environment-variables-set!
     env
     (string->bytes/utf-8 (~a key))
     (and value (string->bytes/utf-8 (~a value)))))
  env)

(define (resolve-executable program)
  (or (and (path? program) program)
      (find-executable-path program)
      (error 'run-command "executable not found: ~a" program)))

(define (direct-subprocess-output? destination)
  (or (not destination)
      (eq? destination 'stdout)
      (and (output-port? destination)
           (file-stream-port? destination))))

(define (start-command* program args cwd environment stdout stderr)
  (parameterize ([current-directory cwd]
                 [current-environment-variables environment]
                 [subprocess-group-enabled #t])
    (define stdout-destination
      (and (direct-subprocess-output? stdout) stdout))
    (define stderr-destination
      (and (direct-subprocess-output? stderr) stderr))
    (define-values (proc child-stdout child-stdin child-stderr)
      (apply subprocess
             stdout-destination
             #f
             stderr-destination
             'new
             (resolve-executable program)
             args))
    (when child-stdin (close-output-port child-stdin))
    (define copy-threads
      (filter
       values
       (list
        (and child-stdout
             (thread
              (lambda ()
                (copy-port child-stdout stdout)
                (close-input-port child-stdout))))
        (and child-stderr
             (thread
              (lambda ()
                (copy-port child-stderr stderr)
                (close-input-port child-stderr)))))))
    (values proc copy-threads)))

(define (start-command program args
                       #:cwd [cwd (current-directory)]
                       #:environment [environment (current-environment-variables)]
                       #:stdout [stdout (current-output-port)]
                       #:stderr [stderr (current-error-port)])
  (define-values (proc _copy-threads)
    (start-command* program args cwd environment stdout stderr))
  proc)

(define (stop-command-group! proc #:grace-seconds [grace-seconds 10])
  (unless (sync/timeout 0 proc)
    (subprocess-kill proc #f)
    (unless (sync/timeout grace-seconds proc)
      (subprocess-kill proc #t)
      (subprocess-wait proc)))
  (subprocess-wait proc))

(define (run-command program args
                     #:cwd [cwd (current-directory)]
                     #:environment [environment (current-environment-variables)]
                     #:stdout [stdout (current-output-port)]
                     #:stderr [stderr (current-error-port)]
                     #:timeout-seconds [timeout-seconds #f]
                     #:grace-seconds [grace-seconds 10])
  (define started (current-inexact-monotonic-milliseconds))
  (define-values (proc copy-threads)
    (start-command* program args cwd environment stdout stderr))
  (define finished?
    (cond
      [timeout-seconds (sync/timeout timeout-seconds proc)]
      [else
       (subprocess-wait proc)
       #t]))
  (define timed-out? (not finished?))
  (when timed-out?
    (stop-command-group! proc #:grace-seconds grace-seconds))
  (unless timed-out? (subprocess-wait proc))
  (for-each thread-wait copy-threads)
  (command-result
   (subprocess-status proc)
   timed-out?
   (/ (- (current-inexact-monotonic-milliseconds) started) 1000.0)))

(define (run-command/capture program args
                             #:cwd [cwd (current-directory)]
                             #:environment [environment (current-environment-variables)]
                             #:timeout-seconds [timeout-seconds #f]
                             #:check? [check? #t])
  (define tmp-dir (make-temporary-file "congame-llm-process-~a" 'directory))
  (define stdout-path (build-path tmp-dir "stdout"))
  (define stderr-path (build-path tmp-dir "stderr"))
  (dynamic-wind
    void
    (lambda ()
      (define result
        (call-with-output-file stdout-path
          #:exists 'truncate/replace
          (lambda (out)
            (call-with-output-file stderr-path
              #:exists 'truncate/replace
              (lambda (err)
                (run-command program args
                             #:cwd cwd
                             #:environment environment
                             #:stdout out
                             #:stderr err
                             #:timeout-seconds timeout-seconds))))))
      (define stdout (file->string stdout-path))
      (define stderr (file->string stderr-path))
      (when (and check?
                 (or (command-result-timed-out? result)
                     (not (zero? (command-result-exit-code result)))))
        (error 'run-command/capture
               "command failed (~a): ~a ~a\n~a"
               (command-result-exit-code result)
               program
               (string-join (map ~a args) " ")
               stderr))
      (values result stdout stderr))
    (lambda ()
      (delete-directory/files tmp-dir))))
