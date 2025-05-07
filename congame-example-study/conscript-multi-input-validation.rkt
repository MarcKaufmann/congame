#lang conscript

(require conscript/form0)

(provide
 sum-percentages)

(defvar beliefs)

(define (input-percent label)
  (input-number label #:attributes '([min "0"] [max "100"])))

(defstep (distribute-100)
  (define-values (f on-submit)
    (form+submit
     [beliefs
      (ensure
       binding/list
       (list-of-length 3)
       (list-of
        (ensure
         binding/number
         (required)
         (range/inclusive 0 100)))
       (lambda (vs)
         (if (= (apply + vs) 100)
             (ok vs)
             (err (for/list ([_ (in-list vs)])
                    "Your answers must sum to 100.")))))]))

  (define (render rw)
    @md*{@rw["beliefs"
             (widget-list
              (lambda (re)
                @md*{@re[@input-percent{How likely is A?}]
                     @re[@input-percent{How likely is B?}]
                     @re[@input-percent{How likely is C?}]}))]

         @|submit-button|})

  @md{# Distribute 100

      One of A, B, or C must happen, so the probabilities must sum to 100.

      @form[f on-submit render]})


(defstep (display-answers)
  (define belief-a (first beliefs))
  (define belief-b (second beliefs))
  (define belief-c (third beliefs))
  (define belief-total (+ belief-a belief-b belief-c))

  @md{# Answers

      In total we get @(~a belief-a) + @(~a belief-b) + @(~a belief-c) = @(~a belief-total).})

(defstudy sum-percentages
  [distribute-100 --> display-answers --> display-answers])
