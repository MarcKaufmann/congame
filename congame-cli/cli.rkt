#lang racket/base

(require (submod conscript/resource private)
         file/zip
         koyo/http
         marionette
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

(provide
 (struct-out exn:fail:api)
 (struct-out exn:fail:api:not-authorized)
 handle-login
 handle-logout
 upload-study
 simulate)

(struct exn:fail:api exn:fail (response))
(struct exn:fail:api:not-authorized exn:fail:api ())

(define current-program-name
  (make-parameter (short-program+command-name)))

(define (get-key)
  (define (fail)
    (eprintf #<<MESSAGE
~a: not logged in

Use the "raco congame login" command to log in first.

MESSAGE
             (current-program-name))
    (exit 1))
  (or (get-preference 'congame-cli:key fail) (fail)))

(define (handle-help)
  (display #<<HELP
usage: raco congame <command> <option> ... <arg> ...

available commands:
  help     display this help message
  login    log into a congame server
  logout   log out of a congame server
  upload   upload a study to the server
  simulate simulate multiple concurrent sessions to one study

HELP
          )
  (exit 0))

(define (handle-login [wait-until-idle? #t])
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
       (when wait-until-idle?
         (sync (system-idle-evt)))
       (void)))))

(define (handle-logout)
  (put-preferences
   '(congame-cli:key)
   (list #f)))

(define (handle-upload)
  (define-values (study-id study-path)
    (command-line
     #:program (current-program-name)
     #:args [study-id path]
     (values study-id path)))
  (unless (file-exists? study-path)
    (eprintf "error: <study-path> is not a file")
    (exit 1))
  (let loop ()
    (with-handlers* ([exn:fail:api:not-authorized?
                      (lambda (_e)
                        (handle-login)
                        (loop))])
      (upload-study study-id study-path))))

(define (upload-study id path)
  (match-define (cons server key)
    (get-key))
  (define study-directory (path-only (path->complete-path path)))
  (define tmp-dir (make-temporary-directory))
  (define tmp-path (make-temporary-file))
  (delete-file tmp-path)
  (dynamic-wind
    (lambda ()
      (let ([path (if (string? path)
                      (path->complete-path (string->path path))
                      path)])
        (copy-file path (build-path tmp-dir "study.rkt"))
        (for ([dep (in-list (get-module-dependencies path (string->symbol id)))])
          (define dep-path (build-path study-directory dep))
          (define dst-path (build-path tmp-dir dep))
          ;; Ensure that any intermediate directories required by the
          ;; dep also exist.
          (define-values (dst-dir _filename _must-be-dir?)
            (split-path dst-path))
          (unless (directory-exists? dst-dir)
            (make-directory* dst-dir))
          (if (directory-exists? dep-path)
              (copy-directory/files dep-path dst-path)
              (copy-file dep-path dst-path))))
      (parameterize ([current-directory (path-only tmp-dir)])
        (zip tmp-path (file-name-from-path tmp-dir))))
    (lambda ()
      (call-with-input-file tmp-path
        (lambda (in)
          (with-handlers ([exn:fail:api?
                           (lambda (e)
                             (match (exn:fail:api-response e)
                               [(http:response #:status-code 401)
                                (raise (exn:fail:api:not-authorized
                                        (exn-message e)
                                        (current-continuation-marks)
                                        (exn:fail:api-response e)))]
                               [_
                                (raise e)]))])
            (~> (http:post
                 #:auth (make-auth key)
                 #:data (http:buffered-payload
                         (http:multipart-payload
                          (http:field-part "study-id" id)
                          (http:file-part "study-source" in "study.zip" "application/zip")))
                 (format "~a/api/v1/cli-studies" server))
                (check-response _ 200)
                (http:response-json)
                (hash-ref 'link)
                (send-url))))))
    (lambda ()
      (delete-file tmp-path)
      (delete-directory/files tmp-dir))))

(define ((make-auth k) _ headers params)
  (values (hash-set headers 'authorization k) params))

(define (check-response resp [ok 200])
  (begin0 resp
    (unless (= (http:response-status-code resp) ok)
      (raise
       (exn:fail:api
        (format "unexpected response~n  code: ~s~n  body: ~.s"
                (http:response-status-code resp)
                (http:response-body resp))
        (current-continuation-marks)
        resp)))))

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

;; XXX: Conscript studies can't import relative modules at the moment,
;; so this only needs to list any resources declared after the module is
;; loaded.
(define (get-module-dependencies path id)
  (define mp (make-resolved-module-path path))
  (with-handlers ([(lambda (e)
                     (regexp-match? #rx"name is not provided" (exn-message e)))
                   (lambda (_)
                     (error 'get-module-dependencies "this study does not provide \"~s\"" id))])
    (parameterize ([current-track-resources? #t])
      (dynamic-require mp id)))
  (hash-keys registry))

(define (handle-simulate)
  (define n 2)
  (define host "http://127.0.0.1:5100")
  (define slug
    (command-line
     #:once-each
     [("--host")
      HOST "the congame host"
      (set! host HOST)]
     [("-n")
      NUM "the number of simultaneous sessions to run"
      (set! n (string->number NUM))
      (unless (and n (> n 0))
        (error 'handle-simulate "NUM must be positive"))]
     #:args [slug]
     slug))
  (simulate host n slug))

(define (simulate host n slug)
  (define thds
    (for/list ([(_ idx) (in-indexed (in-range n))])
      (thread
       (lambda ()
         (call-with-marionette/browser/page!
          #:port (+ 2829 idx)
          #:headless? #f
          (lambda (p)
            (page-goto! p (format "~a/_anon-login/~a" host slug))
            (thread-receive)))
         (sleep 1)))))
  (with-handlers ([exn:break? void])
    (sync/enable-break never-evt))
  (for ([thd (in-list thds)])
    (thread-send thd '(stop))
    (thread-wait thd)))

(module+ main
  (define commands
    (hasheq
     'help handle-help
     'login handle-login
     'logout handle-logout
     'upload handle-upload
     'simulate handle-simulate))

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
