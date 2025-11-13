#lang conscript

(require conscript/form0
         racket/match)

(provide
 range-formlet-study)

(defvar taste)
(defvar price)
(defvar healthiness)

(define range-formlet
  (ensure
   binding/number
   (required)
   (number-in-range 1 5)))

(define (range-widget label)
  (radios
   (for/list ([i (in-inclusive-range 1 5)])
     (cons
      (number->string i)
      (number->string i)))
   label))

(define (render-form rw)
  @md{@rw["taste" @range-widget{Taste}]
      @rw["price" @range-widget{Price}]
      @rw["healthiness" @range-widget{Healthiness}]
      @|submit-button|})

(defstep (statically-defined)
  (define-values (f on-submit)
    (form+submit
     [taste range-formlet]
     [price range-formlet]
     [healthiness range-formlet]))
  @md{# Statically Defined Fields
      @form[f on-submit render-form]})

(defstep (display)
  @md{# Display

      Taste: @~a[taste]
      Price: @~a[price]
      Healthiness: @~a[healthiness]

      Bundled: @~a[(if-undefined bundled-values (hash))]

      @button{Continue}})

(defstep (dynamically-defined)
  (define f
    (dyn:form
     list
     (for/list ([k (in-list '(taste price healthiness))])
       (cons k range-formlet))))
  (define (on-submit vs)
    (match-define (list t p h) vs)
    (set! taste t)
    (set! price p)
    (set! healthiness h))
  @md{# Dynamically Defined and Unbundled Fields
      @form[f on-submit render-form]})

(defvar bundled-values)

(defstep (dynamically-defined-and-bundled)
  (define ks
    '(taste price healthiness))
  (define f
    (dyn:form
     (lambda vs
       (for/hash ([k (in-list ks)]
                  [v (in-list vs)])
         (values k v)))
     (for/list ([k (in-list ks)])
       (cons k range-formlet))))
  (define (on-submit ht)
    (set! bundled-values ht))
  @md{# Dynamically Defined and Bundled Fields
      @form[f on-submit render-form]})

(defstudy range-formlet-study
  [statically-defined
   --> {d1 display}
   --> dynamically-defined
   --> {d2 display}
   --> dynamically-defined-and-bundled
   --> {d3 display}
   --> d3])
