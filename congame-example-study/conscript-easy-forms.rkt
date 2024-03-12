#lang conscript

(require conscript/survey-tools)

(provide
 easy-forms)

(defstep (choose-page)
  @md{
    # Choose which feature you want to see in action

    1. @button[#:to-step-id 'multiple-checkboxes]{Show Multiple Checkboxes}
    2. @button[#:to-step-id 'display-table]{Table}
    3. @button[#:to-step-id 'generate-random-number]{Generate Random Number}
    4. @button[#:to-step-id 'display-math]{Display Math with Latex: Mathjax}
    })

(defstep (display-results)
  (define checkboxes
    (~a (get 'multiple-checkboxes '())))

  @md{
    # Results so far

    1. Result from `Multiple Checkboxes`: @checkboxes

    @button{Go back to choosing Forms}})

(defstep (multiple-checkboxes)
  ; Define the options we will use later
  (define opts
    '((a . "Option a")
      (b . "Option b")
      (c . "Option c")
      (letter-d . "Option d")))

  ; We have to use the special form `binding` and pass it the name of the field, followed by a call to `make-multiple-checkboxes`, which needs to receive a list of options.
  @md{
    # Form with Multiple Checkboxes

    @form{
      @binding[#:multiple-checkboxes @make-multiple-checkboxes[opts]]
      @submit-button}})

(defstep (display-table)
  @md{
    # An example of a Table

    For this, you need to write the table in HTML, although you can write the whole page in markdown (with `md`) and only write the table in HTML (with `html*`, not `html`).

    @html*{
      @table{
        @thead{
          @tr{
            @th{Animal} @th{Legs}}}
        @tbody{
          @tr{
            @td{Dog} @td{4}}
          @tr{
            @td{Spider} @td{8}}}}}

    @button{Go back}
    })

(defstep (generate-random-number)
  ; Generate a new random number every time the page is refreshed and overwrite
  ; the old one. This is usually not the behavior you want.
  (define r
    ; (random n) generates a random integer from 0 to n-1, so we need to `add1` to get a random draw from 1 to 6 inclusive.
    (add1 (random 6)))
  ; This stores the new value in the DB and overwrites the old.
  (put 'refreshed-random r)

  (define r-once
    ; First try to get the value in 'once-random. If this exists, then r-once takes this value. If not, then it takes the value from the `(add1 (random 6))` call - i.e. we set it.
    (get 'once-random (add1 (random 6))))

  ; Now we store the value in the DB, but only if the value doesn't already exist: i.e. `unless` the call to `(get 'once-random #f)` is true --- which means that the value exists and was found --- then we store it.
  (unless (get 'once-random #f)
    (put 'once-random r-once))

  @md{
    # Generate a Random Number

    - The value of `r` is: @(~a r)
    - The value of `r-once` is: @(~a r-once)

    If you refresh the page, the value of `r` will change, while the value of `r-once` will not. You usually don't want it to change based on the refresh.

    @button{Back to Choice}

      })

(defstep (display-math)
  @md{
    @(mathjax-scripts)

    # Display Math

    We know that \\(x^2 + 2 \cdot x + 1 = (x + 1)^2\\). Smart. And as a standalone equation:

    \\[
      x^2 + 2 \cdot x + 1 = (x + 1)^2
    \\]

    To add such mathematical snazziness to your page, you need to include the MathJax script, by writing `@"@"(mathjax-script)` (note the parentheses) at the top of your page.

    To write inline mathematics, you would enclose it in "\\\\(...\\\\)", for an equation all by itself you write "\\\\[...\\\\]" in normal Mathjax, but in conscript, you have to write a double backslash: "\\\\\\\\(...\\\\\\\\)". Don't ask. Just do it.

    @button{Back to Choice}
      })

(defstudy easy-forms
  [choose-page --> choose-page]

  [display-results --> choose-page]

  [multiple-checkboxes --> display-results]

  [display-table --> choose-page]

  [generate-random-number --> choose-page]

  [display-math --> choose-page])
