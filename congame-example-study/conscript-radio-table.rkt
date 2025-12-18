#lang conscript

(require conscript/form0)

(provide
 conscript-radio-table)

(defvar choice)

(defstep (make-choice)
  (define choices
    '(("a" . a)
      ("b" . b)
      ("x" . x)
      ("y" . y)))
  (define-values (f on-submit)
    (form+submit
     [choice (ensure
              binding/text
              (required)
              (one-of choices))]))
  ;; The shape of this table is just to illustrate that we can lay out
  ;; individual radios however we want by leveraging the individual
  ;; radio widget. Note that, if we use this widget, we have to "report"
  ;; errors ourselves (i.e. use the widget-errors widget where we want
  ;; the errors to appear).
  (define (render rw)
    @md{
      @table[@tr[@td[@rw["choice" (radio "a" "A")]]
                 @td[@rw["choice" (radio "x" "X")]]]
             @tr[@td[@rw["choice" (radio "b" "B")]]
                 @td[@rw["choice" (radio "y" "Y")]]]]
      @apply[div @rw["choice" (widget-errors)]]
      @|submit-button|})
  @md{# Make Your Choice

    @form[f on-submit render]})

(defstep (display-choice)
  @md{# Choice

      You chose @~a[choice]})

(defstudy conscript-radio-table
  [make-choice --> display-choice --> display-choice])
