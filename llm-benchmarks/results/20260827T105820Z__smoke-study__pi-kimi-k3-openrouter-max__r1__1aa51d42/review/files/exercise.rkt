#lang racket/base

;; Exercises the benchmark-study against the running Congame server:
;;   1. Anonymous login -> introduction page with a Continue button.
;;   2. Continue -> display-name form.
;;   3. Submitting an empty name is rejected ("This field is required.").
;;   4. Submitting a non-empty name -> final page echoes the name.

(require net/http-client
         racket/dict
         racket/list
         racket/match
         racket/port
         racket/string)

(define host "host.docker.internal")
(define port 46639)
(define slug "bench-20260827t105820z-smoke--1aa51d42")

(define cookies (make-hash)) ;; name -> value

(define (cookie-header)
  (string-join (hash-map cookies (lambda (k v) (format "~a=~a" k v))) "; "))

(define (store-cookies! headers)
  (for ([h (in-list headers)])
    (match (bytes->string/utf-8 h)
      [(pregexp "(?i:^set-cookie: *)([^=;]+)=([^;]*)" (list _ n v))
       (hash-set! cookies n v)]
      [_ (void)])))

;; Performs an HTTP request; manually follows redirects (up to 5).
;; Returns the final response body as a string.
(define (request method path [data #f] [content-type #f] [redirects-left 5])
  (define conn (http-conn-open host #:port port #:ssl? #f))
  (define headers
    (append
     (if (zero? (hash-count cookies))
         '()
         (list (string->bytes/utf-8 (format "Cookie: ~a" (cookie-header)))))
     (if content-type
         (list (string->bytes/utf-8 (format "Content-Type: ~a" content-type)))
         '())))
  (define-values (status resp-headers in)
    (http-conn-sendrecv! conn path
                         #:method method
                         #:headers headers
                         #:data (or data #"")))
  (define body (port->string in))
  (close-input-port in)
  (http-conn-close! conn)
  (store-cookies! resp-headers)
  (define status-str (bytes->string/utf-8 status))
  (cond
    [(and (regexp-match? #rx"(?i:^HTTP/[0-9.]+ 30[127])" status-str)
          (positive? redirects-left))
     (define location
       (for/first ([h (in-list resp-headers)]
                   #:when (regexp-match? #rx"(?i:^location:)" h))
         (bytes->string/utf-8 (second (regexp-match #rx"(?i:^location: *)(.*)" h)))))
     (unless location
       (error 'request "redirect without Location header: ~a" status-str))
     (request "GET" location #f #f (sub1 redirects-left))]
    [(regexp-match? #rx"^HTTP/[0-9.]+ 200" status-str)
     body]
    [else
     (error 'request "unexpected status ~a for ~a ~a" status-str method path)]))

(define (multipart fields)
  (define boundary "----exercise-boundary-9d2f1")
  (define body
    (string->bytes/utf-8
     (string-append
      (string-join
       (for/list ([(k v) (in-dict fields)])
         (format "--~a\r\nContent-Disposition: form-data; name=\"~a\"\r\n\r\n~a\r\n"
                 boundary k v))
       "")
      (format "--~a--\r\n" boundary))))
  (values body (format "multipart/form-data; boundary=~a" boundary)))

(define (extract-first pattern page who)
  (match (regexp-match pattern page)
    [(list _ v) v]
    [_ (error who "could not find ~a in page" pattern)]))

(define (check! ok? what)
  (if ok?
      (printf "PASS: ~a\n" what)
      (begin (printf "FAIL: ~a\n" what)
             (exit 1))))

;; 1. Anonymous login.
(void (request "GET" (format "/_anon-login/~a" slug)))
(define study-path (format "/study/~a" slug))

;; 2. Introduction page with Continue button.
(define intro-page (request "GET" study-path))
(check! (regexp-match? #rx"<h1>Welcome</h1>" intro-page)
        "introduction page shows welcome heading")
(define continue-href
  (extract-first (pregexp (string-append "href=\"(" (regexp-quote study-path) "[^\"]*)\""))
                 intro-page 'continue))
(check! (regexp-match? #rx"Continue" intro-page)
        "introduction page has a Continue button")

;; 3. Continue -> display-name form.
(define form-page (request "GET" continue-href))
(check! (regexp-match? #rx"Your Display Name" form-page)
        "name form page is shown")
(define action
  (extract-first #rx"action=\"([^\"]*)\"" form-page 'form-action))
(check! (regexp-match? #rx"name=\"name\"" form-page)
        "form has a 'name' text input")

;; 4. Empty name is rejected.
(define-values (empty-body empty-ctype) (multipart '(("name" . ""))))
(define rejected-page (request "POST" action empty-body empty-ctype))
(check! (regexp-match? #rx"This field is required" rejected-page)
        "empty name is rejected with a validation error")
(define action2
  (extract-first #rx"action=\"([^\"]*)\"" rejected-page 'form-action))

;; 5. Non-empty name -> final page echoes it.
(define test-name "Exercise Tester")
(define-values (ok-body ok-ctype) (multipart `(("name" . ,test-name))))
(define final-page (request "POST" action2 ok-body ok-ctype))
(check! (regexp-match? (regexp-quote (format "Thank You, ~a!" test-name)) final-page)
        "final page greets the submitted name")
(check! (regexp-match? (regexp-quote test-name) final-page)
        "final page includes the submitted name")

(printf "All checks passed.\n")
