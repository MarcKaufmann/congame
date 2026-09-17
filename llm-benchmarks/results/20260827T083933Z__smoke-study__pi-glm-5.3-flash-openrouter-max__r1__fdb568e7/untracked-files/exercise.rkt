#!/usr/bin/env racket
#lang racket/base

;; Exercises benchmark-study against the Congame server over HTTP.
;;
;; Flow checked:
;;   1. Anon login for the active instance -> introduction page w/ Continue.
;;   2. Continue -> name entry form.
;;   3. Empty name submission is rejected (validation error, stays on form).
;;   4. A real submission advances to a final page echoing the submitted name.
;;
;; Usage: racket exercise.rkt   (exits nonzero on any failure)

(module+ main
  (require racket/match
           racket/system
           racket/port
           racket/list
           racket/file
           racket/string)

  ;; --------------------------------------------------------------- config

  (define base (or (getenv "CONGAME_URL")
                   "http://host.docker.internal:45471"))
  (define slug (or (getenv "CONGAME_INSTANCE")
                   "bench-20260827t083933z-smoke--fdb568e7"))

  ;; ---------------------------------------------------------------- utils

  (define failures 0)

  (define (check! ok? label)
    (printf "~a ~a~n" (if ok? "PASS" "FAIL") label)
    (unless ok?
      (set! failures (add1 failures))))

  ;; Full HTTP response incl. headers due to -i.
  (define (curl . args)
    (with-output-to-bytes
      (lambda ()
        (apply system* "/usr/bin/curl" "-sS" "--max-time" "30" args))))

  (define jar (make-temporary-file "congame-bench-cookies~a"))
  (file-or-directory-permissions jar 384) ; rw------- so the session stays private

  ;; Make relative URLs absolute against the study base URL.
  (define (resolve-url url)
    (if (string-prefix? url "http")
        url
        (string-append base url)))

  (define blank-line/c #rx"\r?\n\r?\n")

  (define (headers-of raw)
    (match (regexp-match-positions blank-line/c raw)
      [(list (cons s _)) (substring raw 0 s)]
      [_ raw]))

  (define (body-of raw)
    (match (regexp-match-positions blank-line/c raw)
      [(list (cons _ e)) (substring raw e)]
      [_ raw]))

  ;; Next URL from a 30x Location header, if any.
  (define (location-of raw)
    (for*/first ([line (in-list (string-split (headers-of raw) "\r\n"))]
                 #:when (string-prefix? (string-foldcase line) "location:")
                 [target (in-value (string-trim (substring line 9)))]
                 #:when (non-empty-string? target))
      (resolve-url target)))

  ;; Target of a 200 HTML <meta http-equiv="refresh" ...> interstitial, if any.
  ;; Matches both "N; /target" and "N; url=/target" forms.
  (define (meta-refresh-of raw)
    (match (regexp-match
            #px"<meta\\s+http-equiv=\"refresh\"[^>]*content=\"\\s*[0-9]+\\s*;\\s*(?:url=)?([^\"]+)\""
            raw)
      [(list _ target) (resolve-url target)]
      [_ #f]))

  ;; One single HTTP round-trip. FIELDS is a list of "name=value" strings,
  ;; each sent as its own --data-urlencode argument.
  (define (roundtrip! method url fields)
    (bytes->string/utf-8
     (apply curl "-i"
            "-X" method
            "-b" (path->string jar) "-c" (path->string jar)
            (append
             (append-map (λ (f) (list "--data-urlencode" f)) fields)
             (list url)))))

  ;; Request a URL as a browser would: send the original request (with form
  ;; fields, if given), then keep GETting whatever the server points us at --
  ;; via Location headers or login interstitials -- but never re-submits the
  ;; form body on the way. Only the final response text is returned.
  ;; When CONGAME_DEBUG is set, log each hop's status line + body prefix.
  (define dbg (getenv "CONGAME_DEBUG"))

  (define (report! tag raw)
    (when dbg
      (match (regexp-match #rx"^HTTP[^\r\n]+" raw)
        [(list status)
         (eprintf "[~a] ~a\n  ~a...\n\n"
                  tag status
                  (substring (body-of raw) 0 (min 160 (string-length (body-of raw)))))]
        [_ (void)])))

  (define (request! method url [fields null])
    (let go ([tag "hop0"] [hops 10] [u url] [body fields])
      (when (zero? hops)
        (error 'request! "too many redirects for ~a" url))
      (define raw (roundtrip! method u body))
      (report! tag raw)
      (define next (or (location-of raw) (meta-refresh-of raw)))
      (cond
        [next (go tag (sub1 hops) next null)]
        [else raw])))

  ;; ----------------------------------------------------------- the flow

  (printf "Congame: ~a  instance: ~a~n~n" base slug)

  ;; Step 1: participant login lands on the introduction page.
  (define intro (body-of (request! "GET" (format "~a/_anon-login/~a" base slug))))
  (check! (string-contains? intro "Benchmark Study")
          "introduction page shows the study title")
  (check! (string-contains? intro "Continue")
          "introduction page has a Continue button")

  ;; The Continue button renders as <a class="button next-button" ... href="...">
  ;; with haml emitting attributes in construction order, so href follows class.
  (define continue-target
    (regexp-match
     #px"class=\"[^\"]*next-button[^\"]*\"[^>]*href=\"([^\"]+)\""
     intro))

  (check! (and continue-target (non-empty-string? (second continue-target)))
          "Continue link carries a continuation href")

  ;; Step 2: click Continue -> name entry form.
  (define form-page
    (body-of
     (request! "GET"
               (resolve-url (if continue-target (second continue-target) base)))))
  (check! (and (string-contains? form-page "<form")
               (string-contains? form-page "display-name"))
          "name entry form rendered")

  (define form-action-m
    (regexp-match #px"<form[^>]*action=\"([^\"]+)\"" form-page))
  (check! (and form-action-m (non-empty-string? (second form-action-m)))
          "form action extracted")

  (define action-url
    (resolve-url (if form-action-m (second form-action-m) base)))

  ;; Step 3: empty submission must be rejected.
  (define rejected
    (body-of
     (request! "POST" action-url '("display-name=" "__tt=0" "__ft=0"))))
  (check! (string-contains? rejected "What is your name?")
          "empty submit stays on the name entry form")
  (check! (string-contains? rejected "Please enter your name.")
          "empty submit rejected with a validation error")
  (check! (not (string-contains? rejected "All done"))
          "empty submit does not reach the final page")

  ;; Step 4: whitespace-only submission must be rejected too.
  (define rejected-blank
    (body-of
     (request! "POST" action-url '("display-name=   " "__tt=0" "__ft=0"))))
  (check! (not (string-contains? rejected-blank "All done"))
          "whitespace-only name does not reach the final page")

  ;; Step 5: valid submission reaches the final page showing the name.
  (define tester "Bench Tester")
  (define finished
    (body-of
     (request! "POST" action-url
               (list (format "display-name=~a" tester)
                     "__tt=0" "__ft=0"))))
  (check! (string-contains? finished "All done!")
          "final page reached after submitting a name")
  (check! (string-contains? finished tester)
          "final page echoes the submitted name")

  ;; ------------------------------------------------------------- summary

  (printf "~n~a failures~n" failures)
  (exit (if (zero? failures) 0 1))
  )
