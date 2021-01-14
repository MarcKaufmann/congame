#lang racket/base

(require (prefix-in forms: forms)
         koyo/haml
         marionette
         racket/format
         racket/generic
         racket/list
         racket/match
         racket/random
         racket/serialize
         (prefix-in bot: (submod congame/components/bot actions))
         congame/components/export
         congame/components/registry
         congame/components/study)

(provide
 pl-study)

(define-generics describable
  (describe describable))

;; TODO: Work time VS decision time
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

(serializable-struct price-list (treatment choices)
  #:transparent
  #:methods gen:jsexprable
  [(define/generic ->jsexpr/super ->jsexpr)
   (define (->jsexpr pl)
     (hash 'type "price-list"
           'treatment (->jsexpr/super (price-list-treatment pl))
           'choices (->jsexpr/super (price-list-choices pl))))])

(define possible-treatments
  '(((a 0) (a 1))
    ((b 1) (b 2))))

(define (make-price-list-options _sym n)
  (list
   (list (option n 0)
         (option 0 0))
   (list (option n 1)
         (option 0 0))))

(define (render-options optionss)  ;; ss -> list of lists of options
  (define the-form
    (forms:form
     list
     (for/list ([(options i) (in-indexed optionss)])
       (define field-name
         (string->symbol (format "option-~a" i)))
       (define option-by-index
         (for/hash ([(o i) (in-indexed options)])
           (values i o)))
       (cons field-name (forms:ensure forms:binding/number
                                      (forms:required)
                                      (lambda (v)
                                        (cond
                                          [(hash-ref option-by-index v #f) => forms:ok]
                                          [else (forms:err "Invalid option selected.")])))))))

  (haml
   (form
    the-form
    (lambda (choices)
      (define treatments (get 'treatments))
      (define treatment (car treatments))
      (put 'treatments (cdr treatments))

      (define price-lists (get 'price-lists null))
      (put 'price-lists (append
                         price-lists
                         (list (price-list treatment
                                           (for/list ([options (in-list optionss)]
                                                      [chosen (in-list choices)])
                                             (choice chosen options)))))))
    (lambda (rw)
      (haml
       (:table
        ,@(for/list ([(options i) (in-indexed optionss)])
            (define field-name (format "option-~a" i))
            (haml
             (:tr
              ,@(for/list ([(o j) (in-indexed options)])
                  (haml
                   (:td
                    (rw field-name (lambda (name _value _errors)
                                     (haml
                                      (:label
                                       (:input
                                        ([:name name]
                                         [:type "radio"]
                                         [:value (~a j)]))
                                       (describe o))))))))
              ,@(let ([errors (rw field-name (forms:widget-errors))])
                  (if (null? errors)
                      null
                      `((tr (td ([colspan "2"]) ,@errors))))))))
        (:tr
         (:td
          (:button
           ([:type "submit"])
           "Submit")))))))))

(define (info-step)
  (haml
   (:div
    (:h1 "You are in the Price Lists study")
    (button
     (lambda ()
       (define treatments
         (shuffle (random-ref possible-treatments)))

       (put 'treatments/orig treatments)
       (put 'treatments treatments))
     "Continue"))))

(define (price-list-step)
  (define treatment (car (get 'treatments)))
  (haml
   (:div
    (:h1 "Price List")
    (render-options
     (apply make-price-list-options treatment)))))

(define (price-list-step/bot n-fixed)
  (for ([elt (in-list (bot:find-all "tr"))]
        [n (in-naturals 1)])
    (define radios (bot:element-find-all elt "input"))
    (unless (null? radios)
      (if (<= n n-fixed)
          (element-click! (car radios))
          (element-click! (cadr radios)))))
  (element-click! (bot:find "button[type=submit]")))

(define pl-study
  (make-study
   #:requires '()
   #:provides '()
   (list
    (make-step 'info info-step)
    (make-step 'price-list
               price-list-step
               #:for-bot price-list-step/bot
               (lambda ()
                 (if (null? (get 'treatments))
                     next
                     'price-list))))))
