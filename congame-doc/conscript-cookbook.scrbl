#lang scribble/manual

@(require [for-label (except-in conscript/survey-tools make-sliders)
                     conscript/base
                     (except-in racket/base require)
                     conscript/form0
                     racket/contract]
          "doc-util.rkt")

@title{Conscript Cookbook}

What follows are a bunch of “recipes” or how-tos that explain how to accomplish common tasks when
writing Conscript studies.

These how-tos don’t directly explain everything, but they do give links to more information,
including the links in the code samples themselves.

@;===============================================

@section{Basic Study Recipe}

@codeblock|{
#lang conscript

(provide
 conscript-example)

(defstep (info)
  @html{@h1{Hello!}
        Welcome to the study.
        @button{Continue}})

(defstep (consent)
  @html{@h1{Consent}
        Do you consent to join the study?
        @button{Give Consent}})

(defstep (final)
  @html{@h1{Done}
        You're done.

        @a[#:href (~current-view-uri)
           #:up-layer "new"
           #:up-target ".container"]{Overlay}})

(defview (done-overlay _req)
  @md{# Info

      Blah blah blah.})

(defstudy conscript-example
  [info --> {consent1 consent}
        --> {consent2 consent}
        --> {final (make-step
                   #:view-handler done-overlay
                   'final final)}]
  [final --> final])
}|

@;===============================================

@section[#:tag "forms-recipe"]{Forms Recipe}

@itemlist[#:style 'ordered

@item{Specify the form data and get an on-submit procedure using @racket[form+submit].}

@item{Define a procedure to render the form. This procedure takes one argument, a widget renderer
procedure, and returns an @X-expression .}

@item{Supply these three values to @racket[form] inside a study step.}

]

@codeblock|{
#lang conscript

(require conscript/form0
         racket/match)

(provide example-form-study)

(defstep (intro)
  @md{# Forms

      This study shows how to use `forms-lib` with conscript.

      @button{Continue}})

(defvar name)
(defvar age)
(defvar checkbox-choices)
(defvar radio-choice)
(defvar select-choice)

(defstep (survey)
  (define choices
    '(("a" . "Choice A")
      ("b" . "Choice B")))

  ; Define once for easy reuse below
  (define choice-formlet (ensure binding/text (one-of choices)))

  (define-values (survey-form on-submit)
    (form+submit
     [name (ensure binding/text (required))]
     [age  (ensure binding/number (required))]
     [checkbox-choices (ensure binding/list (list-of choice-formlet))]
     [radio-choice     (ensure choice-formlet (required))]
     [select-choice    (ensure choice-formlet (required))]))

  (define (render rw)
    @md*{@rw["name" @input-text{Enter your name}]   ; Customized label
         @rw["age" (input-number)]
         @rw["checkbox-choices" (checkboxes choices)]
         @rw["radio-choice" (radios choices)]
         @rw["select-choice" (select (cons '("" . "Please select something") choices) "")]
         @|submit-button|})

  @md{# Survey

      @form[survey-form on-submit render]})

(defstep (display-info)
  @md{# Thanks

      * Name: @name
      * Age: @~a[age]
      * Checkbox choices: @(string-join checkbox-choices ", ")
      * Radio choice: @radio-choice
      * Select choice: @select-choice})

(defstudy example-form-study
  [intro --> survey --> display-info --> ,(λ () done)])
}|


@;===============================================

@section{Multi-participant Study Recipe}

@tktk{Example of multi-participant study}

@;===============================================

@section{Bots Recipe}

@tktk{Example of how to use bots}


@;===============================================

@section[#:style 'quiet]{Miscellaneous Recipes}

@;------------------------------------------------

@subsection{How to add links}

To provide a link on a study page, use the anchor tag @racket[a]:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript

(defstep (links)
  @md{# Links

      A link to the @a[#:href "https://example.com/"]{another site}.

      Sometimes you want a link to open in a new tab, so you provide the
      attribute `target` with the value `"_blank"`:

      @a[#:href "https://example.com" #:target "_blank"]{Open in new tab}
  })
}|

@;------------------------------------------------

@subsection{How to display monetary amounts}

To display monetary amounts, first @racket[require] the module
@racketmodname[conscript/survey-tools] which provides @racket[~$] for dollars, @racket[~euro] for
euros, or @racket[~pound] for pounds. These functions convert the number to a string for you,
displayed to two decimal places:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(require conscript/survey-tools)

(define (payments)
  (define bonus 4.25)
  @md{# Bonus

      Your bonus is @(~$ bonus).
  })
}|


@;------------------------------------------------

@subsection{How to add buttons to jump to various pages}

You may want to have buttons to allow participants to select which page to show next. For example,
you might want to allow a participant to go back and change a choice; or while debugging you might
want to have a page that allows you to jump to specific parts for quick testing.

@margin-note{Be careful with skipping to a specific page: jumping back to a form will, by default,
overwrite the original answer; and skipping to a later part in the study may lead to errors. If you
skip a form that would have asked your name, then a later reference to @racketidfont{name} will
fail due to the key @racketidfont{name} not being found.}

Example usage:
@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (choose-page)
  @md{
    # Choose which feature you want to see in action

    1. @button[#:to-step-id 'multiple-checkboxes]{Show Multiple Checkboxes}
    2. @button[#:to-step-id 'generate-random-number]{Generate Random Number}
    3. @button[#:to-step-id 'display-math]{Display Math with Latex: Mathjax}

    The buttons on this page show that you can jump to different pages by
    providing a `#:to-step-id` argument to `button`.
    })
}|

This page assumes that there are steps called @racket['multiple-checkboxes],
@racket['generate-random-number], and so on.


@;------------------------------------------------

@subsection{How to have a form input with multiple checkboxes}

The default @racket[checkbox] provides a single checkbox. You may want to provide multiple options
at once, allowing a person to choose one or more (or zero) options. You can do so with
@racket[make-multiple-checkboxes] from the @racket[conscript/survey-tools] module. Here is an
example of a form to choose between four options, “a” to “d” and how to include it in the form. For
@racket[#:multiple-checkboxes-1], any number of checkboxes can be selected; for
@racket[#:multiple-checkboxes-2] a person has to select 2 or more checkboxes:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
; First, we require the library that provides make-multiple-checkboxes
(require conscript/survey-tools)

(defstep (multiple-checkboxes)
  (define opts
    '((a . "Option a")
      (b . "Option b")
      (c . "Option c")
      (letter-d . "Option d")))

  @md{
    # Form with Multiple Checkboxes

    @form{
      @binding[#:multiple-checkboxes-1 @make-multiple-checkboxes[opts]]
      @binding[#:multiple-checkboxes-2 @make-multiple-checkboxes[opts #:n 2]]
      @submit-button}})
}|

We have to use the special form @racket[binding] and pass it the name of the
field, followed by a call to @racket[make-multiple-checkboxes], which needs to
receive a list of options. Here we define the list of options earlier to make
the code more readable. Note that if a person checks "Option a" and "Option d",
then the list @racket['(a letter-d)] will be stored in the database --- so you
can call the options any names that make sense to you, independent of the label
that participants see.

@;------------------------------------------------

@subsection{How to add the @"@" sign on a page}

Write @"@\"@\"". For example:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(define (step-with-email)
  @md{# My Email

      My email is kaufmannm@"@"ceu.edu.})
}|

Explanation: If you tried to include an email or otherwise type the @"@" symbol,
you will have noticed that it leads to strange behavior. The reason is that the
@"@" symbol stands for "Here is some Racket code". Thus
@tt{kaufmannm@"@"ceu.edu} will be interpreted as the string @tt{kaufmannm}
followed by @tt{find the value bound to the variable 'ceu.edu'}, which will lead
to an error message unless you defined ceu.edu. By writing @"@\"@\"" you say
that you want to include the code @"\"@\"", which is simply the string @"\"@\"",
which is what you want.

@;------------------------------------------------

@subsection{How to add a CSS class to an HTML element}

Example:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (stylish-page)
  @md{
    @style{
      .message {
        color: red;
      }
    }

    # Stylish Page

    A normal paragraph.

    @p[#:class "message"]{This paragraph has a class of "message" which you can
    style.}
  })
}|

In this example, we create two paragraphs. The first one is created implicitly.
The second one we have to define explicitly, so that we can add a class to it
via @code|{@p[#:class "message"]{...}}|.

Adding a class allows us to select this element in our CSS, here via the
@racket[style] tag. Here we select every element with class "message" with the
CSS class selector: a dot ('.') followed by the name of the class, here
".message". Then inside of curly braces, we set all the CSS properties we want
to apply to this class.

Most formatting can be achieved with the right CSS with the help of Google,
ChatGPT, or --- for classroom assignments --- Slack.

@;------------------------------------------------

@subsection{How to add a form with many sliders}

A page with two sliders that don't display its value but can be dragged around
--- the default range is from 0 to 100, but you can change it with the keyword
arguments @racket[#:min] and @racket[#:max]:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(define (simple-slider)
  @md{# A single slider

      @form{
        @div{
          @input-range[#:slider-1]{How much do you like this slider?}
          @input-range[#:slider-2 #:min 95 #:max 100]{No really, how much do you
          like it?}
          @submit-button
        }
      }
  })
}|

Often, you want to display the current value of a slider as it is dragged
around. @racket[conscript/survey-tools] provides a simple script called
@racket[slider-js] that you can load, which looks for every div with the class
@tt{slider}, and then connects the current value of the @racket[input-range]
with the @racket[output] element inside of that div. Here is how to use it,
which requires adding @racket[slider-js] somewhere on the page (here at the
top):

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(define (slider-with-value-displayed)
  @md{@(slider-js)
      # Slider with Value Displayed

      @form{
        @div[#:class "slider"]{
          @input-range[#:slider-1] @span{Value: @output{}}
        }
        @div[#:class "slider"]{
          @input-range[#:slider-2] @span{Value: @output{}}
        }
        @submit-button
      }
  })
}|

The important part here is that there is an @racket[input-range] and an
@racket[output] inside of a @racket[div] with the class "slider",
@racket[slider-js] takes care of the rest.

Finally, we may want to give our participants some work, having them set 48
sliders all at once. @racket[conscript/survey-tools] provides
@racket[make-multiple-checkboxes] for this purpose (with limited ability to
change the formatting):

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(define (many-many-sliders)
  @md{# More sliders than you can handle

      @make-sliders[48]
  })
}|

One big downside of the current sliders is that you cannot add any other fields
to the form - it's sliders only. The values are stored with the keys "slider-0"
to "slider-47".

We can however customize the sliders by passing a function that customizes the
part that displays the slider itself:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(define (many-many-sliders-with-random-starting-point)
  (define (make-custom-slider _)
    (define start-value (random 100))
    @input-range[#:attributes `([value ,start-value])])

  @md{# More sliders than you can handle

      @make-sliders[10 make-custom-slider]
  })

(define (many-many-customized-sliders-with-increasing-starting-point)
  (define (make-custom-slider i)
    (define r (* 2 (random 50 100)))
    @input-range[
      #:max r
      #:attributes `([value ,(~a (* 10 i))]
                     [style ,(format "width: ~apx" r)])
    ])

  @md{# More sliders than you can handle

      @make-sliders[10 make-custom-slider]
  })
}|

Thus we pass in a function that creates a slider, here called
@tt{make-custom-slider}. This function must take a single argument
@racket[i],which is the index of the slider to create. Our first version doesn't
use this index, since we create a random starting value for the slider. Our
second version uses the index, using ten times the index as the starting value,
and also varies the total range, which we use to also set the width of via the
"style" attribute.

@;------------------------------------------------

@subsection{How to add a table to a page}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (table-step)
  @md{# A table

      A manual table:
      @table{
        @thead{
          @tr{
            @th{Animal} @th{Legs}}}
        @tbody{
          @tr{
            @td{Dog} @td{4}}
          @tr{
            @td{Spider} @td{8}}}}


    A table with generated rows:
    @table{
      @thead{
        @tr{
          @th{Animal} @th{Legs}}}
      @(apply
        tbody
        (for/list ([animal '("Dog" "Spider" "Cat" "Fish" "Human" "Ant")]
                   [legs '(4 8 4 0 2 6)])
          @tr{@td{@animal} @td{@(~a legs)}}))}

    @button{Next}})
}|

Note that the numbers in @racket[legs] have to be converted to strings, or else
you will get errors.

@;------------------------------------------------

@subsection[#:tag "image-howto"]{How to add images}

In order to make use of images and other files in a page, you must:

@itemlist[

@item{Bundle those images together with your study code in a @filepath{.zip} file.}

@item{Declare them within your study code using @racket[define-static-resource].}

]

For example, to use an image file @filepath{test.png} in your study:

@itemlist[#:style 'ordered

@item{Create a new, empty folder — the name can be anything, but for this example let’s call it
@filepath{study-bundle}.}

@item{Inside the @filepath{study-bundle/} folder, create a file
@filepath{study.rkt}.@margin-note{When bundling like this, the study source code file @emph{must} be
called @filepath{study.rkt} or it won’t work!}}

@item{Also inside @filepath{study-bundle} --- and next to @filepath{study.rkt} create another folder
@filepath{images}.}

@item{Inside this @filepath{images} subfolder, place an image file named @filepath{test.png}.}

]

Ensure the @filepath{study.rkt} file contains this code:

@filebox["study.rkt"

@codeblock|{
#lang conscript

(provide image-study)

; Create the resource that points at the image.
(define-static-resource screenshot "images/test.png")

(defstep (show-image)
  @html{
    @h1{A Page with an Image}
     @img[
      #:alt "Screenshot"
      #:src (resource-uri screenshot)
    ]})

(defstudy image-study
  [show-image --> show-image])
}|]

@itemlist[

@item{Note in particular the use of @racket[define-static-resource]. This tells Conscript where to find
that file (relative to @filepath{study.rkt}) and gives you a special binding
(@racketidfont{screenshot}) to use when you need to refer to it.}

@item{Then, inside the @racket[img]
element, you use @racket[resource-uri] to generate the URL for that resource on the server where the
study is running.}

]


@;------------------------------------------------

@subsection{How to customize the text of a submit button}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (labeled-submit-button)
  @md{
    # Submit Button with Custom Label

    @form{
      There is no field here to fill in. What a form.
      @submit-button/label{A Custom Label for Submissions!}}
      })
}|

@;------------------------------------------------

@subsection{How to add blank lines}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
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
}|

@;------------------------------------------------

@subsection{How to provide error message when wrong radio button is chosen}

You will need to require @racket[conscript/survey-tools] to use
@racket[is-equal].

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(require conscript/survey-tools)

(define (radio-with-error)
  @md{# Radio with Error

      The correct answer to the next radio button is "Option C", try it out by
      picking first another option:

      @form{
        @radios[
          #:radios-with-error
          '(("a" . "Option A")
            ("b" . "Option B")
            ("c" . "Option C"))
          #:validators (list (is-equal "c" #:message "Wrong answer, LOL!!!"))
        ]{The correct option is C - but try something else first maybe!}
}})
}|

@;------------------------------------------------

@subsection{How to display radio buttons on a single line}

Wrap the @racket[radios] in a @racket[div] with class "radio-horizontal":

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(define (radio-horizontal)
  @md{# Radio with Horizontal Buttons

      @form{
        @div[#:class "radio-horizontal"]{
          @radios[
            #:radios-with-error
            '(("a" . "Option A")
              ("b" . "Option B")
              ("c" . "Option C"))
          ]{Choose Horizontally}}
        @submit-button
}})
}|

@;------------------------------------------------

@subsection{Page with dice roll button}

Example with some custom CSS for the button:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(require conscript/survey-tools)

(defstep (diceroll)
  @md{@diceroll-js
      @style{
        .diceroll > .button {
          width: 5rem;
          display: inline-block;
        }
      }
      # Diceroll

      @div[#:class "diceroll"]{
        @a[#:class "button" #:href ""]{Roll}
        @output{}

      @button{Go Back}
      }})
}|

@;------------------------------------------------

@subsection{How to select a random value from a list of items}

Consider the following study that elicits in steps 1, 2, and 3 how many tasks a
participant is willing to do for $2, $3, and $4 and then on the final page we
pick one of these choices randomly as the choice to implement and then asks the
participant to do those tasks. In addition, let's assume that the completion fee
is $1 by default.

@codeblock|{
#lang conscript

(require racket/list
         racket/random
         conscript/survey-tools)

(provide
 several-payments-random-task)

(define completion-fee 1.00)

(defstep (welcome)
  (define (initialize)
    (put 'payments '(2 3 4)))

  @md{# Welcome

      @button[initialize]{Next}})

(defstep (how-many-tasks)
  (define payments
    (get 'payments))
  (define payment
    (first payments))
  (define (on-submit #:n-tasks n-tasks)
    (define old-answers
      (get 'answers '()))
    (put 'answers
         (cons
           (list payment n-tasks)
           old-answers))
    (put 'payments
         (rest payments)))

  @md{# Tasks

      @form[#:action on-submit]{
        @input-number[#:n-tasks #:min 0 #:max 40]{@md*{How many tasks are you
        willing to do for @(~a payment)?}}

        @submit-button}})

(define (initialize-tasks)
  (put 'remaining-tasks (get 'tasks-that-count)))

(define (display-payment)
  (define maybe-choice-that-counts
    (get 'choice-that-counts #f))

  (define choice-that-counts
    (cond [maybe-choice-that-counts
           maybe-choice-that-counts]

          [else
           (define r
             (random-ref (get 'answers)))
           (put 'choice-that-counts r)
           r]))

  (define payment-that-counts
    (first choice-that-counts))

  (define tasks-that-count
    (second choice-that-counts))

  (put 'payment-that-counts payment-that-counts)
  (put 'tasks-that-count tasks-that-count)

  @md{# The Chosen Payment

      The following choice was picked randomly picked as the choice that counts:

      - Tasks to do: @(~a tasks-that-count)
      - Payment: @(~a payment-that-counts)

      @button[initialize-tasks]{Continue to tasks}
})

(define (tasks)
  (define n
    (get 'tasks-that-count))

  ; Starting point for slider
  (define s
    (number->string (random 100)))

  @md{@(slider-js)
      # Do @n Slider Tasks

      @form{
        @div[#:class "slider"]{
          @input-range[
            #:slider
            #:attributes `([value ,s])
        ] @span{Value: @output{}}
        }
        @submit-button
      }

      @button[process-submission]{Next}})

(define (process-submission)
  (define answer
     (get 'slider))

  (define old-score
    (get 'score 0))

  (define new-score
    (if (= answer 50)
      (add1 old-score)
      old-score))

  (put 'score new-score)

  (put 'remaining-tasks
       (sub1 (get 'remaining-tasks)))

  (skip))

(define (end)
  (define payment
    (get 'payment-that-counts))

  (define total-payment
    (+ payment completion-fee))

  (define score
    (get 'score))

  @md{# The End

      Thanks, you will receive the payment of @(~pound total-payment).

      You got @(~a score) tasks right.})

(defstudy several-payments-random-task
  [welcome --> how-many-tasks
           --> ,(lambda ()
                  (if (null? (get 'payments))
                      'display-payment
                      'how-many-tasks))]

  [display-payment --> ,(lambda ()
                          (if (> (get 'tasks-that-count) 0) 'tasks 'end))]

  [tasks --> process-submission
         --> ,(lambda ()
                (if (> (get 'remaining-tasks) 0)
                    'tasks
                    'end))]

  [end --> end])
}|

@;------------------------------------------------

@subsection{How to generate a random number}

A naive way to generate a random number and display it to a user is by
generating it at the start of the page and displaying it. The problem with this
approach is that every time the user refreshes the page, a new number is
generated, which is often not what you want, since then users can refresh until
they get the number they want.

If we only want to generate the number once, we should generate it only the
first time and then store it, say, with the key @racket['r-once]. If we do this,
then every time we visit the page, we check whether the number already was
stored. If so, then we display this value again; if not, then this is the first
time the user visits this page, so we generate it randomly.

To do this, we use @code|{(get 'r-once (add1 (random 6)))}|. This attempts to
get @racket['r-once], but if this is not found, then it returns a new random
value instead. We then store this value, so that on the next refresh of the
page, the call to @racket[get] is successful.

The study below generates two random numbers: one that changes upon every
refresh, one that gets generated once and stays constant across refreshes.

@codeblock|{
#lang conscript

(require racket/random)

(provide
  generate-random)

(defstep (generate-random-number)
  (define r
    ; (random n) generates a random integer from 0 to n-1, so we need to `add1`
    ; to get a random draw from 1 to 6 inclusive.
    (add1 (random 6)))
  ; This stores the new value in the DB and overwrites the old.
  (put 'refreshed-random r)

  (define maybe-value
    (get 'r-once #f))

  (define r-once
    ; If maybe-value is not #f, then maybe-value is the random number.
    ; Otherwise, generate a new one.
    (if maybe-value maybe-value (add1 (random 6))))

  (unless maybe-value
    (put 'r-once r-once))

  @md{
    # Generate a Random Number

    - The value of `r` is: @(~a r)
    - The value of `r-once` is: @(~a r-once)

    If you refresh the page, the value of `r` will change, while the value of
    `r-once` will not. You usually don't want it to change based on the refresh.

    @button{Back to Choice}
      })
}|

@;------------------------------------------------

@subsection{How to have radio buttons with images}

Suppose that you have two images in the folder @filepath{"img/"} and you upload
a study as a zip file. Then the following code will add images next to the radio
buttons:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(require racket/match)

(define style-img-radio
  @style{
    .img-radio {
      display: inline-block;
    }

    .img-radio img {
      display: block;
    }

    .img-radio label {
      text-align: center;
      display: block;
    }

    .job-description {
      text-align: center;
    }
  })

(define-static-resource path-to-image-a "img/job-a.png")
(define-static-resource path-to-image-b "img/job-b.png")

(defstep (radio-with-images)
  (define (render-proc options make-radio)
    (apply div #:class "img-radio"
           (for/list ([opt options])
             (match-define (list value res job empl years) opt)
             @div{
                  @img[
                    #:alt (format "an image for option ~a" value)
                    #:src (resource-uri res)
                  ]
                  @div[#:class "job-description"]{
                    @div{@job}
                    @div{@empl}
                    @div{@years}
                  }
                  @(make-radio value)})))

  @md{@style-img-radio
      # Radios with Images

      @form{
        @binding[
          #:radio-with-images
          (make-radios
            `((a ,path-to-image-a "Job Title A" "Employer A" "Years: A")
              (b ,path-to-image-b "Job Title B" "Employer B" "Years: B"))
            render-proc)
        ]
        @submit-button}})
}|

@;------------------------------------------------

@subsection{How to add a Radio Button with a button for an Other option}

The following displays radio buttons for the options "A", "B", and "Other",
providing a text-input for "Other". Moreover, if the input for "Other" is filled
in, then the radio button for "Other" is automatically selected.

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (radios-with-other-choice)
  @md{# Radios with "other" choice

      @form{@binding[#:radios-with-other
                     (make-radios-with-other '((a . "A")
                                               (b . "B")))]
            @submit-button}})
}|

@;------------------------------------------------

@subsection{How to have a select button with a default option that cannot be
submitted}

To have a default option for a select button that cannot be selected, provide it
as the first option with value "" and the desired display value, here "--Please
choose an option--":

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (sele
ct-with-default)
  @md{# Select with Default

      @form{
        @select[#:select-with-default
                '((""  . "--Please choose an option--")
                 ("1" . " 1 ")
                 ("2" . " 2 ")
                 ("3" . " 3 "))
        ]{Please choose an option}
        @submit-button}})
}|

@;------------------------------------------------

@subsection{How to add a timer to a page}

We can add a timer to a page by using @racket[timer] from the
@racket[conscript/survey-tools] library. Once the timer ends, it automatically
clicks the first submit button on the page; or if no submit button is found, it
clicks the first next button.

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(require conscript/survey-tools)

(defstep (timer-display)
  @md{# Page with Timer clicking Next

      @timer[10]

      After 10 seconds, this page will automatically move on.

      @button{Next}})

(defstep (timer-form)
  @md{# Page with Timer clicking Submit

      @timer[11]

      After 11 seconds, this page will automatically submit however many sliders
      have been completed:

      @make-sliders[10]})

(defstep (timer-hidden)
  @md{@style{
        #timer {
          display: none;
        }
      }

      # Page with Hidden Timer

      @timer[8]

      After 8 seconds, this page moves on, but you don't see the timer. This is
      done using CSS, so it's easy.

      @button{Next}})
}|

@;------------------------------------------------

@subsection{How to add a timer that spans multiple pages}

The following timer will pick up where it left off on the previous page. To do
so, we store the end time in the database and at the start of each page with a
timer, we compute how many seconds are left until this time and start a new
timer with the appropriate remaining seconds left. Note that you have to
@racket[require] the @racket[gregor] library for @racket[now/moment] and other
time-related functions.

@codeblock[#:keep-lang-line? #t]|{
#lang conscript

(require gregor)

(define (set-timer)
  (put
   'the-timer
   (+seconds (now/moment) 10)))

(defstep (launch-timer)
  @md{# Launch Timer

      Once you click "Next", the timer starts and you have 30 seconds to
      complete the next 3 pages.

      @button[set-timer]{Next}})

(defstep ((timer-page i))
  (define seconds-left
    (add1
     (truncate
      (seconds-between
       (now/moment)
       (get 'the-timer)))))

  ; If there is less than 1 sec left, we might have skipped the earlier
  ; page so we don't display it anymore. This is to avoid some weird
  ; effects.
  (cond [(< seconds-left 1)
         (skip)]

        [else
         @md{# Timer Page @(~a i)

             @timer[seconds-left]

             Check the time!

             @button{Next}}]))

(define (final-page)
  @md{# Final Page

      You made it till the end.})

(defstudy multi-page-timer
  [launch-timer --> [first-page (timer-page 1)]
                --> [second-page (timer-page 2)]
                --> [third-page (timer-page 3)]
                --> final-page])
}|

@;------------------------------------------------

@subsection{How to display a waiting page until some condition is met}

Suppose that we want to let a person move on only once some condition is met, such as that the study
is open. Here we will use the condition that the participant can move on only 10 seconds after
landing on the page for the first time - until then, they simply see a waiting message. We use
@racket[refresh-every] from @racket[survey-tools] for this:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(require conscript/survey-tools
         gregor)

(defstep (waiting)
  (define wait-until
    (get 'wait-until (+seconds (now/moment) 10)))

  (put 'wait-until wait-until)

  (cond [(moment>=? (now/moment) wait-until)
         (skip)]

        [else
         @md{# Please Wait
             @refresh-every[5]

             Your patience is appreciated.}]))

(defstep (wait-is-over)
  @md{# The Wait is Over

      @button{Next}})

(defstudy wait-study
  [waiting --> wait-is-over])
}|

@;------------------------------------------------

@subsection{How to repeat a task until a timer runs out}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(require gregor
         conscript/survey-tools)

(define ((set-timer n))
  (put
   'the-timer
   (+seconds (now/moment) n)))

(defstep ((launch-timer [n 10]))
  @md{# Launch Timer

      Once you click "Next", the timer starts and you have @(~a n) seconds.

      @button[(set-timer n)]{Next}})

(defstep (task-step)
  (define seconds-remaining
    (seconds-between (now/moment) (get 'the-timer)))
  (when (<= seconds-remaining 0)
    (skip))

  (define (on-submit #:slider slider)
    (define old-answers
      (get 'sliders '()))
    (put 'sliders
         (cons
          slider
          old-answers)))

  @md{# Do a Task
      @(slider-js)

      @timer[seconds-remaining]

      @form[#:action on-submit]{
        @div[#:class "slider"]{
          @input-range[#:slider] @span{Value: @output{}}
        }
        @submit-button
      }})

(defstep (final)
  @md{# The End})

(defstudy multi-page-timer-with-tasks
  [[launching (launch-timer 20)] --> task-step
                                 --> ,(lambda ()
                                        (define the-timer (get 'the-timer))
                                        (cond [(moment>=? the-timer (now/moment))
                                               'task-step]

                                              [else
                                               'final]))])
}|

@;------------------------------------------------

@subsection{How to reuse similar steps with different roles}

@codeblock[#:keep-lang-line? #t]|{
#lang conscript

(require conscript/survey-tools)

(provide
 randomized-study)

(define (randomize-treatments)
  (assigning-treatments
   (list 'control 'treatment)))

(define (randomize-game-roles)
  (assigning-treatments
   (list 'role1 'role2)
   #:treatments-key 'game-roles
   #:role-key       'game-role))

(define (assign-roles)
  (randomize-treatments)
  (randomize-game-roles)
  (skip))

(defstep ((same-page i))
  @md{
 # Same Page @(~a i)

 Your role is @(~a (get 'game-role)).

 @button{Continue}})

(defstep (final-page)
  @md{
 # Final Page

 @button{Continue}})


(defstep ((treatment-page i))
  (define treatment
    (~a (get 'role)))

  (define role
    (~a (get 'game-role)))

  @md{
 # @(string-titlecase treatment) Page @(~a i)

 - Your treatment is: @treatment
 - Your role is: @role

 @button{Continue}})

(defstudy randomized-study
  [assign-roles --> [same-page1 (same-page 1)]
   --> [treatment-page1 (treatment-page 1)]
   --> [same-page2 (same-page 2)]
   --> [treatment-page2 (treatment-page 2)]
   --> final-page]

  [final-page --> final-page])
}|
