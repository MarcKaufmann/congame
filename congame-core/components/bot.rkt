#lang racket/base

(require racket/contract
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
  (-> symbol? (-> void?) bot-stepper?)
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

(module+ actions
  (require marionette)

  (provide
   run-bot
   current-page
   continuer
   click
   find
   find-all
   element-find
   element-find-all)

  (define INFINITE-LOOP-THRESHOLD 25)

  ;; runner ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define current-page
    (make-parameter #f))

  (define current-delay
    (make-parameter 0))

  (define/contract (run-bot b
                            #:study-url url
                            #:username username
                            #:password password
                            #:headless? [headless? #t]
                            #:delay [delay 0])
    (->* (bot?
          #:study-url string?
          #:username string?
          #:password string?)
         (#:headless? boolean?
          #:delay real?)
         void?)
    (call-with-marionette/browser/page!
     #:headless? headless?
     (lambda (p)
       (parameterize ([current-page p]
                      [current-delay delay])
         (page-goto! p url)
         (maybe-log-in! username password)
         (execute! b)))))

  (define (maybe-log-in! username password)
    (define form (page-wait-for! (current-page) ".form--login"))
    (when form
      (element-type! (find "[name=username]") username)
      (element-type! (find "[name=password]") password)
      (element-click! (find "button[type=submit]"))))

  (define (execute! b [previous-paths null])
    (when (>= (count-path-run previous-paths) INFINITE-LOOP-THRESHOLD)
      (raise-bot-error "potential infinite loop at path ~s" (car previous-paths)))
    (define study-done? (find-attribute "data-study-done"))
    (unless study-done?
      (define study-stack (read (open-input-string (find-attribute "data-study-stack"))))
      (define step-id (string->symbol (find-attribute "data-step-id")))
      (define path (cons step-id study-stack))
      ;; TODO: logging
      (displayln path)
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

  (define/contract (continuer)
    (-> void?)
    (element-click! (find-widget)))

  (define/contract (click id)
    (-> symbol? void?)
    (element-click! (find-widget id)))


  ;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (page-wait-for! (current-page) selector #:timeout 5))
    (unless maybe-element
      (raise-bot-error "could not find widget with id ~s" id))
    maybe-element))
