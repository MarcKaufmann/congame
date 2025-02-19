#lang racket/base

(require congame-web/components/auth
         (only-in congame-web/components/user user-admin-like?)
         (prefix-in config: congame-web/config)
         koyo/flash
         koyo/haml
         koyo/l10n
         koyo/preload
         koyo/profiler
         koyo/url
         racket/format
         threading
         web-server/http
         xml)

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
  (define current-user-researcher?
    (and~>
     (current-user)
     (user-admin-like?)))
  (haml
   (:html
    (:head
     (:meta ([:charset "utf-8"]))
     (:meta ([:name "viewport"] [:content "width=device-width, initial-scale=1"]))

     (:title (if subtitle (~a subtitle " - congame") "congame"))
     (:link ([:rel "stylesheet"] [:href (static-uri "app.css")]))

     (unless (getenv "CI")
       (haml
        (:script
         ([:crossorigin "anonymous"]
          [:src "https://js.sentry-cdn.com/fb877e2559424bf292eeb8331b8479b9.min.js"]))))

     (:link
      ([:rel "stylesheet"]
       [:href "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.11.2/cdn/themes/light.css"]))
     (:script
      ([:type "module"]
       [:blocking "render"])
      #<<SCRIPT
import 'https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.11.2/cdn/components/radio-group/radio-group.js';
import 'https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.11.2/cdn/components/radio/radio.js';
SCRIPT
      )

     (:script
      ([:id "MathJax-script"]
       [:async ""]
       [:src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"]))

     (:script ([:src (static-uri "app.js")])))
    (:body
     (when (impostor?)
       (haml
        (.impostor-bar
         (:a
          ([:href (reverse-uri 'admin:stop-impersonation-page)])
          "Stop impersonating user."))))
     (when (or show-nav? current-user-researcher?)
       (cond [current-user-researcher?
              (nav
               (nav-item (reverse-uri 'study-instances-page) (translate 'nav-dashboard))
               (nav-item (reverse-uri 'logout-page) (translate 'nav-log-out))
               (nav-item (reverse-uri 'admin:studies-page) (translate 'nav-admin))
               (nav-item "/admin/jobs" (translate 'nav-jobs)))]
             [(current-user)
              (nav
               (nav-item (reverse-uri 'study-instances-page) (translate 'nav-dashboard))
               (nav-item (reverse-uri 'logout-page) (translate 'nav-log-out)))]
             [else
              (nav (nav-item (reverse-uri 'study-instances-page) (translate 'nav-dashboard))
                   (nav-item (reverse-uri 'login-page) (translate 'nav-log-in))
                   #;(nav-item (reverse-uri 'signup-page) (translate 'nav-sign-up)))]))

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
