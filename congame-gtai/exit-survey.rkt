#lang conscript

(provide
 exit-survey)

(defvar attend)
(defvar pizza)
(defvar comment)
(defvar/instance attendance)
(defvar/instance pizzas)
(defvar/instance comments)

(define pizza-opts
  '(("margherita" . "Margherita")
    ("funghi"     . "Funghi")
    ("salami"     . "Salami")
    ("tonno"      . "Tonno")
    ("rucola"     . "Rucola")
    ("veggie"     . "Veggie")))

(defstep (survey)
  @md{# Survey

      As you may remember, we will meet on Tuesday, February 25th, at 12:40 (room tbc) to use up the pizza budget that you collected via the in-class games.

      @form{
        @(set! attend @checkbox[#:required? #f]{Are you planning to attend?})
        @(set! pizza @radios[pizza-opts]{Which pizza would you prefer?})
        @(set! comment @input-text[#:required? #f]{Leave a comment if you have a special dietary requirement that is incompatible with any of the pizzas and you plan on attending.})
        @submit-button}})

(defstep (store-data)
  (with-study-transaction
    (set! attendance
          (cons (if attend #t #f) attendance)))
  (with-study-transaction
    (set! pizzas
          (hash-update pizzas pizza add1 0)))
  (with-study-transaction
    (set! comments
          (cons comment comments)))
  (skip))

(defstep (thanks)
  @md{# Thanks})

(defstep (admin)
  @md{# Admin

      People who answered survey: @(~a (length attendance))

      People who plan on attending: @(~a (apply + (map (lambda (x) (if x 1 0)) attendance)))

      Pizza votes:

      @`(ul
         ,@(for/list ([(p n) (in-hash pizzas)])
             (li (format "~a: ~a votes" p n))))})

(defstep (check-owner)
  (if (current-participant-owner?)
      (skip 'admin)
      (skip)))

(defstep (init)
  (with-study-transaction
    (when (undefined? attendance)
      (set! attendance null)))
  (with-study-transaction
    (when (undefined? comments)
      (set! comments null)))
  (with-study-transaction
    (when (undefined? pizzas)
      (set! pizzas (hash))))
  (skip))

(defstudy exit-survey
  [init --> check-owner --> survey --> store-data --> thanks --> thanks]
  [admin --> admin])
