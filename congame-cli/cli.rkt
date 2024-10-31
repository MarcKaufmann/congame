#lang racket/base

(require file/zip
         koyo/http
         (prefix-in http: net/http-easy)
         net/sendurl
         net/url
         racket/async-channel
         racket/cmdline
         racket/file
         racket/match
         racket/path
         raco/command-name
         threading
         web-server/http
         web-server/servlet-dispatch
         web-server/web-server)

(define current-program-name
  (make-parameter (short-program+command-name)))

(define (get-key)
  (get-preference
   'congame-cli:key
   (lambda ()
     (eprintf #<<MESSAGE
~a: not logged in

Use the "raco congame login" command to log in first.

MESSAGE
              (current-program-name))
     (exit 1))))

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
      (list (cons server congame-key)))
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

(define (handle-upload)
  (define-values (study-id study-path)
    (command-line
     #:program (current-program-name)
     #:args [study-id path]
     (values study-id path)))
  (unless (file-exists? study-path)
    (eprintf "error: <study-path> is not a file")
    (exit 1))
  (match-define (cons server key)
    (get-key))
  (define study-directory (path-only study-path))
  (define tmp-dir (make-temporary-directory))
  (delete-directory tmp-dir)
  (define tmp-path (make-temporary-file))
  (delete-file tmp-path)
  (dynamic-wind
    (lambda ()
      (copy-directory/files study-directory tmp-dir)
      (unless (file-exists? (build-path tmp-dir "study.rkt"))
        (rename-file-or-directory
         (build-path tmp-dir (file-name-from-path study-path))
         (build-path tmp-dir "study.rkt")))
      (parameterize ([current-directory (path-only tmp-dir)])
        (zip tmp-path (file-name-from-path tmp-dir))))
    (lambda ()
      (call-with-input-file tmp-path
        (lambda (in)
          (~>
           (http:post
            #:auth (make-auth key)
            #:data (http:buffered-payload
                    (http:multipart-payload
                     (http:field-part "study-id" study-id)
                     (http:file-part "study-source" in "study.zip" "application/zip")))
            (format "~a/api/v1/cli-studies" server))
           (check-response _ 202)))))
    (lambda ()
      (delete-file tmp-path)
      (delete-directory/files tmp-dir))))

(define ((make-auth k) _ headers params)
  (values (hash-set headers 'authorization k) params))

(define (check-response resp [ok 200])
  (begin0 resp
    (unless (= (http:response-status-code resp) ok)
      (error 'check-response "unexpected response~n  code: ~s~n  body: ~.s"
             (http:response-status-code resp)
             (http:response-body resp)))))

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
     'login handle-login
     'upload handle-upload))

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
