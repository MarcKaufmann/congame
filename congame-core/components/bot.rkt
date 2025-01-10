#lang racket/base

(require net/url
         racket/contract/base
         racket/contract/region
         racket/list)

(provide
 current-user-bot?)

(define/contract current-user-bot?
  (parameter/c boolean?)
  (make-parameter #f))


;; bot structs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 bot?
 make-bot

 bot-stepper?
 make-bot-stepper
 make-bot-stepper/study)

(struct bot (steppers)
  #:transparent)

(struct bot-stepper (id action)
  #:transparent)

(define/contract (make-bot . steps)
  (-> (or/c bot? bot-stepper?) ... bot?)
  (bot (for/fold ([steppers (hash)])
                 ([s (in-list steps)])
         (cond
           [(bot? s)
            (for/fold ([steppers steppers])
                      ([(k sub-s) (in-hash (bot-steppers s))])
              (hash-set steppers (append k (list '*root*)) sub-s))]
           [else
            (hash-set steppers (list (bot-stepper-id s) '*root*) s)]))))

(define/contract (make-bot-stepper id action)
  (-> symbol? (-> any) bot-stepper?)
  (bot-stepper id action))

(define/contract (make-bot-stepper/study study-id b)
  (-> symbol? bot? bot?)
  (bot (for/hash ([(k s) (in-hash (bot-steppers b))])
         (values (append (drop-right k 1) (list study-id)) s))))


;; errors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct exn:fail:bot exn:fail ()
  #:transparent)

(define (raise-bot-error message . args)
  (raise (exn:fail:bot (apply format message args) (current-continuation-marks))))


;; actions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-logger congame-bots)

(module+ actions
  (require marionette)

  (provide
   run-bot
   current-page
   completer
   continuer
   click
   click-all
   type-all
   wait-for
   show
   find
   find-all
   find-attribute
   element-find
   element-find-all)

  (define INFINITE-LOOP-THRESHOLD 100)

  ;; runner ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (struct exn:bot:done exn ())

  (define current-page
    (make-parameter #f))

  (define current-delay
    (make-parameter 0))

  (define/contract (run-bot b
                            #:study-url url
                            #:username username
                            #:password password
                            #:delay [delay 0]
                            #:browser [browser #f]
                            #:headless? [headless? #t]
                            #:port [port #f])
    (->* [bot?
          #:study-url string?
          #:username string?
          #:password string?]
         [#:delay real?
          #:browser (or/c browser? #f)
          #:headless? boolean?
          #:port (or/c #f (integer-in 0 65535))]
         void?)
    (define (do-run-bot p)
      (parameterize ([current-page p]
                     [current-delay delay])
        (page-goto! p url)
        (maybe-log-in! username password)
        (execute! b)))

    (if browser
        (do-run-bot (make-browser-page! browser))
        (call-with-marionette/browser/page!
         #:headless? headless?
         #:timeout 30
         #:port port
         do-run-bot)))

  (define (maybe-log-in! username password)
    (when (equal? (url->path (page-url (current-page)))
                  (string->path "/login"))
      (define form (page-wait-for! (current-page) ".form--login"))
      (when form
        (element-type! (find "[name=username]") username)
        (element-type! (find "[name=password]") password)
        (element-click! (find "button[type=submit]")))))

  (define (execute! b [previous-paths null])
    (when (>= (count-path-run previous-paths) INFINITE-LOOP-THRESHOLD)
      (raise-bot-error
       (string-append
        "potential infinite loop at path ~s~n"
        "  see also: https://github.com/MarcKaufmann/congame/issues/92")
       (car previous-paths)))
    (with-handlers ([exn:bot:done? void])
      (page-wait-for!
       #:timeout 10
       #:visible? #f
       (current-page)
       "[data-study-stack]")
      (define study-stack-str (find-attribute "data-study-stack"))
      (unless study-stack-str
        (raise-bot-error "failed to get study stack at ~a" (url->string (page-url (current-page)))))
      (define study-stack (read (open-input-string study-stack-str)))
      (define step-id (string->symbol (find-attribute "data-step-id")))
      (define path (cons step-id study-stack))
      (log-congame-bots-debug "found path ~s" path)
      (define stepper
        (hash-ref (bot-steppers b) path (lambda ()
                                          (raise-bot-error "no stepper for path ~s" path))))
      ((bot-stepper-action stepper))
      (sleep (current-delay))
      (execute! b (cons path previous-paths))))

  (define (count-path-run paths)
    (cond
      [(null? paths) 0]
      [else
       (for/fold ([prev (car paths)] [cnt 0] #:result cnt)
                 ([path (in-list paths)])
         #:break (not (equal? prev path))
         (values path (add1 cnt)))]))


  ;; actions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define/contract (completer)
    (-> void?)
    (raise (exn:bot:done "done" (current-continuation-marks))))

  (define/contract (continuer)
    (-> void?)
    (element-click! (find-widget)))

  (define/contract (click id)
    (-> symbol? void?)
    (element-click! (find-widget id)))


  ;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (click-all elts)
    (page-execute-async!
     (current-page)
     "args[0].forEach((el) => el.click());"
     (map element-handle elts)))

  (define (type-all elts&text)
    (page-execute-async!
     (current-page)
     "args[0].forEach(({el, text}) => el.value = text)"
     (for/list ([(elt text) (in-hash elts&text)])
       (hash 'el (element-handle elt)
             'text text))))

  (define (wait-for selector)
    (page-wait-for! (current-page) selector))

  (define (show selector)
    (page-execute-async!
     (current-page)
     "args[0].style.display = 'block'"
     (element-handle (find selector))))

  (define (find selector)
    (page-query-selector! (current-page) selector))

  (define (find-all selector)
    (page-query-selector-all! (current-page) selector))

  (define (element-find elt selector)
    (element-query-selector! elt selector))

  (define (element-find-all elt selector)
    (element-query-selector-all! elt selector))

  (define (find-attribute attr)
    (define elt (find (format "[~a]" attr)))
    (and elt (element-attribute elt attr)))

  (define (find-widget [id #f])
    (define selector
      (if id
          (format "[data-widget-id=~a]" id)
          "[data-widget-id]"))
    (define maybe-element
      (page-query-selector! (current-page) selector))
    (unless maybe-element
      (raise-bot-error "could not find widget with selector ~s" selector))
    maybe-element))
