#lang racket/base

(require (prefix-in forms: forms)
         koyo/haml
         marionette
         racket/format
         racket/generic
         racket/match
         racket/serialize
         (prefix-in bot: (submod congame/components/bot actions))
         (submod congame/components/bot actions)
         congame/components/bot-maker
         congame/components/export
         congame/components/study)

;;; TODO: Should we provide just pl-step or pl-study? The latter defines #:requires and #:provides,
;;; which allows for some type checking, unlike steps. Useful for configuring via symbolic models
;;; via types of data to collect. Can this be done via steps - certainly if we define a
;;; generice `step->study`.
;;; TODO: provide a `make-pl` function for other modules to use

(provide
 price-list-step
 price-list-step/bot
 pl-study
 make-pl)

(define-generics describable
  (describe describable))

(serializable-struct option (work money)
  #:transparent
  #:methods gen:describable
  [(define (describe o)
     (format "Work: ~a money: ~a"
             (option-work o)
             (option-money o)))]
  #:methods gen:jsexprable
  [(define (->jsexpr o)
     (hash 'type "option"
           'work (option-work o)
           'money (option-money o)))])

(serializable-struct adjustable-option (work)
  #:transparent
  #:methods gen:jsexprable
  [(define (->jsexpr v)
     (hash 'type "adjustable-option"
           'work (adjustable-option-work v)))])

(define (set-level/adjustable a l)
  (option (adjustable-option-work a) l))

(serializable-struct option/timed (work money work-time decision-time)
  #:transparent
  #:methods gen:describable
  [(define (describe o)
     (format "Work: ~a money: ~a wt: ~a dt: ~a"
             (option/timed-work o)
             (option/timed-money o)
             (option/timed-work-time o)
             (option/timed-decision-time o)))]
  #:methods gen:jsexprable
  [(define (->jsexpr o)
     (match-define (option/timed work money work-time decision-time) o)
     (hash 'type "option/timed"
           'work work
           'money money
           'work-time work-time
           'decision-time decision-time))])

(serializable-struct choice (chosen options)
  #:transparent
  #:methods gen:jsexprable
  [(define/generic ->jsexpr/super ->jsexpr)
   (define (->jsexpr c)
     (hash 'type "choice"
           'chose (->jsexpr/super (choice-chosen c))
           'options (->jsexpr/super (choice-options c))))])

(serializable-struct price-list (name fixed adjustable levels answers)
  #:transparent
  #:methods gen:jsexprable
  [(define/generic ->jsexpr/super ->jsexpr)
   (define (->jsexpr pl)
     (hash 'type "price-list"
           'name (->jsexpr/super (price-list-name pl))
           'fixed (->jsexpr/super (price-list-fixed pl))
           'adjustable (->jsexpr/super (price-list-adjustable pl))
           'levels (->jsexpr/super (price-list-levels pl))
           'answers (->jsexpr/super (price-list-answers pl))))])

(define ((rw-pl pl) rw)
  (haml
   (:table
    ,@(for/list ([(level i) (in-indexed (price-list-levels pl))])
        (define field-name (format "option-~a" i))
        (define option-by-type
          (hash 'fixed (price-list-fixed pl)
                'adjustable (set-level/adjustable (price-list-adjustable pl) level)))
        (haml
         (:tr
          ,@(for/list ([t '(fixed adjustable)])
              (haml
               (:td
                (rw field-name (lambda (name _value _errors)
                                 (haml
                                  (:label
                                   (:input
                                    ([:name name]
                                     [:type "radio"]
                                     [:value (~a t)]))
                                   (describe (hash-ref option-by-type t)))))))))
          ,@(let ([errors (rw field-name (forms:widget-errors))])
              (if (null? errors)
                  null
                  `((tr (td ([colspan "2"]) ,@errors))))))))
    (:tr
     (:td
      (:button
       ([:type "submit"])
       "Submit"))))))

(define (render-pl pl)
  (define the-form
    (forms:form
     list
     (for/list ([(level i) (in-indexed (price-list-levels pl))])
       (define field-name
         (string->symbol (format "option-~a" i)))
       (define option-by-type
         (hash 'fixed (price-list-fixed pl)
               'adjustable (set-level/adjustable (price-list-adjustable pl) level)))
       (cons field-name (forms:ensure forms:binding/symbol
                                      (forms:required)
                                      (lambda (v)
                                        (cond
                                          [(hash-ref option-by-type v #f) (forms:ok (list level v))]
                                          [else (forms:err "Invalid option selected.")])))))))

  (haml
   (form
    the-form
    (lambda (choices)
      ; FIXME: put relies on unique id, so name has to be unique.
      ; Ideally this should be enforced somewhere in the design so it throws an error
      ; before running through half the study.
      ; TODO: Is it sensible to require it to be uniqe? If not, then it may be impossible (or hard)
      ; to infer which specific choice this was, as was the case in study in summer 2020.
      (put (string->symbol
            (string-append
             "price-list-"
             (symbol->string (price-list-name pl))))
            (struct-copy price-list pl (answers choices))))
    (rw-pl pl))))

;; TODO: Improve interface. In fact, option types should be definable by users, since
;; price lists should work with all kinds of options.
(define (make-pl #:name name
                 #:fixed-work fixed-work
                 #:fixed-money fixed-money
                 #:adjustable-work adjustable-work
                 #:levels-of-money levels-of-money)
  (price-list name
              (option fixed-work fixed-money)
              (adjustable-option adjustable-work)
              levels-of-money
              null))

(define ((price-list-step pl))
  (haml
   (:div
    (:h1 "Price List")
    (render-pl pl))))

(define (price-list-step/bot n-fixed)
  (for ([elt (in-list (bot:find-all "tr"))]
        [n (in-naturals 1)])
    (define radios (bot:element-find-all elt "input"))
    (unless (null? radios)
      (if (<= n n-fixed)
          (element-click! (car radios))
          (element-click! (cadr radios)))))
  (element-click! (bot:find "button[type=submit]")))

  (define price-lists
    (hash 'choice1 (price-list 'choice1
                               (option 0 0)
                               (adjustable-option 10)
                               '(0 1 2)
                               null)))

  (define (info-step)
    (haml
     (:div
      (:h1 "You are in the Price Lists study")
      (button void "Continue"))))

  (define pl-study
    (make-study
     #:requires '()
     #:provides '()
     (list
      (make-step 'info info-step)
      (make-step 'price-list
                 (price-list-step (hash-ref price-lists 'choice1))
                 #:for-bot price-list-step/bot
                 ))))

(module+ main

  (define pl-bot
    (study->bot pl-study))

  ;; Default model runs default bot. Alter model to alter bot behavior.
  (define (model id)
    (match id
      ['(*root* price-list)
       0]

      [_ (values)]))

  (run-bot
   #:study-url "http://127.0.0.1:5100/study/pl-test1"
   #:username "bot@example.com"
   #:password "password"
   ;; #:headless? #f
   ;; #:delay 3
   (pl-bot model)))
