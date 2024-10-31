#lang racket/base

(require koyo/http
         net/sendurl
         net/url
         racket/async-channel
         racket/cmdline
         racket/file
         racket/match
         raco/command-name
         web-server/http
         web-server/servlet-dispatch
         web-server/web-server)

(define current-program-name
  (make-parameter (short-program+command-name)))

(define (handle-help)
  (display #<<HELP
usage: raco congame <command> <option> ... <arg> ...

available commands:
  help   display this help message
  login  log into a congame server

HELP
          )
  (exit 0))

(define (handle-login)
  (display "Congame server: [http://127.0.0.1:5100] ")
  (define server (read-line))
  (when (equal? server "")
    (set! server "http://127.0.0.1:5100"))
  (define done-sema (make-semaphore))
  (call-with-web-server
   (lambda (req)
     (define binds (request-bindings/raw req))
     (define congame-key (bindings-ref binds 'key))
     (put-preferences
      '(congame-cli:key)
      (list congame-key))
     (response/output
      #:mime-type #"text/plain"
      (lambda (out)
        (displayln "You've been logged in." out)
        (displayln "You may now close this window." out)
        (semaphore-post done-sema))))
   (lambda (address)
     (with-handlers ([exn:break? void])
       (display "Press ENTER to open a browser and log in...")
       (void (read-line))
       (send-url
        (url->string
         (struct-copy
          url (string->url server)
          [path (list (path/param "_cli-login" null))]
          [query `((return . ,address))])))
       (displayln "Waiting for login...")
       (sync/enable-break done-sema)
       (sync (system-idle-evt))
       (void)))))

(define (call-with-web-server start proc)
  (define ch (make-async-channel))
  (define stop
    (serve
     #:port 0
     #:dispatch (dispatch/servlet start)
     #:confirmation-channel ch))
  (define exn-or-port (sync ch))
  (when (exn:fail? exn-or-port)
    (raise exn-or-port))
  (dynamic-wind
    void
    (lambda ()
      (proc (format "http://127.0.0.1:~a" exn-or-port)))
    (lambda ()
      (stop))))

(module+ main
  (define commands
    (hasheq
     'help handle-help
     'login handle-login))

  (define-values (command handler args)
    (match (current-command-line-arguments)
      [(vector command args ...) ;; noqa
       (define handler
         (hash-ref
          #;ht commands
          #;key (string->symbol command)
          #;fail-proc (lambda ()
                        (eprintf "error: invalid command ~s" command)
                        (exit 1))))
       (values command handler args)]
      [_
       (values "help" handle-help null)]))

  (parameterize ([current-command-line-arguments (list->vector args)]
                 [current-program-name (string-append (current-program-name) " " command)])
    (handler)))
