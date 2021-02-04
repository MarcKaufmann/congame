#lang racket/base

(require koyo/flash
         koyo/haml
         koyo/l10n
         koyo/preload
         koyo/profiler
         koyo/url
         racket/format
         web-server/http
         xml
         (prefix-in config: "../config.rkt")
         "auth.rkt"
         (only-in "user.rkt" user-admin?))

(provide
 static-uri
 container
 page
 page/xexpr)

(define (static-uri path)
  (define path/full (format "/static/~a?rev=~a" path config:version))
  (begin0 path/full
    (track-preload-dependency! path/full)))

(define (container . content)
  (haml (.container ,@content)))

(define (nav . items)
  (haml
   (.nav
    ([:up-nav ""])
    (container
     (haml (.nav__items ,@items))))))

(define (nav-item uri label)
  (haml
   (:li.nav__item
    (:a
     ([:href uri]
      [:up-alias uri])
     label))))

(define (page/xexpr #:subtitle [subtitle #f]
                    #:show-nav? [show-nav? #f]
                    . content)
  (haml
   (:html
    (:head
     (:meta ([:charset "utf-8"]))
     (:meta ([:name "viewport"] [:content "width=device-width, initial-scale=1"]))

     (:title (if subtitle (~a subtitle " - congame") "congame"))
     (:link ([:rel "stylesheet"] [:href (static-uri "css/screen.css")]))
     (:link ([:rel "stylesheet"] [:href (static-uri "vendor/unpoly.min.css")]))
     (:script
      ([:defer "defer"]
       [:src (static-uri "vendor/unpoly.min.js")]))
     (:script
      ([:defer "defer"]
       [:scr (static-uri "js/app.js")])))
    (:body
     (when show-nav?
       (cond [(and (current-user) (user-admin? (current-user)))
              (nav
               (nav-item (reverse-uri 'study-instances-page) (translate 'nav-dashboard))
               (nav-item (reverse-uri 'logout-page) (translate 'nav-log-out))
               (nav-item (reverse-uri 'admin:studies-page) (translate 'nav-admin))
               (nav-item "/admin/jobs" (translate 'nav-jobs)) )
              ]
             [(current-user)
              (nav
               (nav-item (reverse-uri 'study-instances-page) (translate 'nav-dashboard))
               (nav-item (reverse-uri 'logout-page) (translate 'nav-log-out)))]
             [else
              (nav (nav-item (reverse-uri 'study-instances-page) (translate 'nav-dashboard))
                   (nav-item (reverse-uri 'login-page) (translate 'nav-log-in))
                   (nav-item (reverse-uri 'signup-page) (translate 'nav-sign-up)))]))

     (unless (null? (current-flash-messages))
       (container
        (haml
         (:ul.flash
          ,@(for/list ([flash (current-flash-messages)])
              (haml
               (:li
                ([:class (format "flash__item flash__item--~a" (car flash))])
                (cdr flash))))))))

     (.content ,@content)))))

(define (page #:subtitle [subtitle #f]
              #:show-nav? [show-nav? #t]
              . content)

  ;; profile-write is called inside a different thread so we have to
  ;; grab the current profile here and then pass it in to ensure that
  ;; the right profile gets rendered.
  (define profile (current-profile))
  (with-timing 'template "(page ...)"
    (define page
      (apply page/xexpr
             #:subtitle subtitle
             #:show-nav? show-nav?
             content))

    (response
     200
     #"OK"
     (current-seconds)
     #"text/html; charset=utf-8"
     (make-preload-headers)
     (lambda (out)
       (parameterize ([current-output-port out])
         (displayln "<!doctype html>")
         (write-xml/content (xexpr->xml page))
         (profile-write profile))))))
