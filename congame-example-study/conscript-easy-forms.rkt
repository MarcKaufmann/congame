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
    5. @button[#:to-step-id 'labeled-submit-button]{Submit button with custom text}
    6. @button[#:to-step-id 'free-form-forms1]{Free-Form Forms}
    7. @button[#:to-step-id 'vertical-whitespace]{More whitespace between paragraphs}

    The buttons on this page show that you can jump to different pages by providing a `#:to-step-id` argument to `button`.
    })

(defstep (display-results)
  (define checkboxes
    (~a (get 'multiple-checkboxes '())))

  (define free-form
     (get 'n-required #f))

  (define twice-free-form
    (if free-form (* 2 free-form) "no value provided"))

  @md{
    # Results so far

    1. Result from `Multiple Checkboxes`: @checkboxes
    2. Twice the result from `Free-Form Forms`: @(~a twice-free-form)

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

(defstep (labeled-submit-button)
  @md{
    # Submit Button with Custom Label

    @form{
      There is no field here to fill in. What a form.
      @submit-button/label{A Custom Label for Submissions!}}
      })

(defstep (free-form-forms1)
  @md{
    @style{
      .red-asterisk {
        color: red;
        font-weight: bold;
      }

      .green-exclamation {
        border-radius: 50%;
        width: 1rem;
        line-height: 1rem;
        background: green;
        color: white;
        font-weight: bold;
        text-align: center;
        display: inline-block;
      }
    }

    # Forming Free-Form Forms

    Here is a form where the labels and input fields are moved around more freely, and the fields have more advanced styles. The next page illustrates how you can reuse these styles so that you don't have to redefine them over and over.

    @form{
      @div[#:class "question-group"]{
        @div{@span[#:class "red-asterisk"]{*}How many required questions are on this page?}
        @div{@span[#:class "green-exclamation"]{!}Only positive integer values may be entered in this field.}
        @input-number[#:n-required #:min 0] @~error[#:n-required]}
      @submit-button}
      })


(defstep (free-form-forms2)
  (define special*
    @span[#:class "red-asterisk"]{*})
  (define special!
    @span[#:class "green-exclamation"]{!})

  @html{
    @style{
      .red-asterisk {
        color: red;
        font-weight: bold;
      }

      .green-exclamation {
        border-radius: 50%;
        width: 1rem;
        line-height: 1rem;
        background: green;
        color: white;
        font-weight: bold;
        text-align: center;
        display: inline-block;
      }
    }

    @h1{Freeing Forming Free-Form Forms}

    Now suppose you have multiple forms on the previous page. (Just to show how to display twice the value you submitted: it is @(~a (* 2 (get 'n-required))).) It becomes quickly tedious to type all that HTML for each question, especially if multiple questions all take the same styling. Therefore we do what every lazy programmer does, and define a function that wraps the label in the HTML with the right classes, and similarly for requirements.

    @form{
      @div[#:class "question-group"]{
        @span{@special* How many required questions are on this page?}
        @span{@special! Only positive integer values may be entered in this field.}
        @input-number[#:n-required #:min 0] @~error[#:n-required]}

      @div[#:class "question-group"]{
        @radios[
            #:tall
            '(("1" . "Yes")
              ("2" . "No"))
        ]{@md*{
            @special* Are you tall?

            @special! Choose one of the following answers}
      }
      @submit-button}}})

(defstep (vertical-whitespace)
  @md{
    # More Vertical Whitespace

    Let us add more more whitespace after this.
    \
    \
    \
    \
    Lots of it.

    @html*{
      In `html`, you do it via the `br` tag.
      @br{}
      @br{}
      @br{}
      See? Easy.
    }

    @button{Back to Choice Page}})

(defstudy easy-forms
  [choose-page --> choose-page]

  [display-results --> choose-page]

  [multiple-checkboxes --> display-results]

  [display-table --> choose-page]

  [generate-random-number --> choose-page]

  [display-math --> choose-page]

  [labeled-submit-button --> choose-page]

  [free-form-forms1 --> free-form-forms2 --> display-results --> choose-page]

  [vertical-whitespace --> choose-page])
