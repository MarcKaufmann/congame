#lang scribble/manual

@(require (for-label conscript racket/base))

@(define exercise-counter 0)

@(define (exercise . description)
   (set! exercise-counter (add1 exercise-counter))
   (para
    (bold (format "Exercise ~a: " exercise-counter)) description))

@; TODO
@; Be consistent in the use of 'I', 'We', 'you'

@title[#:tag "Tutorial"]{Conscript Tutorial}

You will create a simple study that gets some information from people
(their age, their height), elicits a probability about some belief from
them, and finally asks them to choose their favorite color between
orange and blue, which will determine how the final page looks.

@section{Create researcher account}

To follow along with this tutorial, you need to create an account with
the ``researcher'' or ``admin'' roles on a @tech{congame} server. To
do so, create an account on your @tech{congame} server, then in your
database, set the researcher role in the database. In postgres:

@codeblock|{
  UPDATE users SET roles = '{researcher}' WHERE ID = <id-of-your-user>;
}|

If you have a researcher or admin role, you will see navigation for
``Admin'' and ``Jobs''.

@section{Syntax Hightlighting and Checking in DrRacket}

If you want to get some syntax highlighting and checking in DrRacket for
conscript studies, then you should install the conscript package. Since it is
not yet published as an official package on the catalog, you have to install it
manually. First you have to install the @tt{congame-core} package, then the
@tt{conscript} package. Follow these steps to install @tt{congame-core} in
DrRacket:

@itemize[
  #:style 'ordered
  @item{Go to ``File'' and select ``Package Manager''}
  @item{Copy the following url into the line saying ``Package Source'':
  @tt{git://github.com/MarcKaufmann/congame?path=congame-core}}
  @item{Click on ``Show Details''}
  @item{Under the option for ``Dependencies Mode'', select ``Auto + Update''}
  @item{Click on the ``Install'' button. @bold{Note:} If you only see an
  ``Update'' button, then you already installed @tt{congame-core} and you can
  skip this step.}
]

Once this completed successfully, install @tt{conscript} by following the same
steps, but using @tt{git://github.com/MarcKaufmann/congame?path=conscript} as
the url for the package source.

@section{The first study}

To start, note that @tech{conscript} is based on @tech{scribble} syntax:
this means that all operators start with an @tt{@"@"}, followed by the
name of the operator, followed either by square brackets (@tt{[]})
or curly brackets (@tt{{}}) that contain additional content. To
get started, let us create a @tech{conscript} study that displays a
single page with some text. To do so, store the following text in
@filepath{tutorial.rkt}:

@codeblock|{
#lang conscript

(provide
 tutorial)

(defstep (start)
  @html{
    @h1{The Beginning is the End}

    This is all there is.
  })

(defstudy tutorial
  [start --> start])
}|

This code defines a @tech{step} named @racket[start], and a @tech{study}
named @racket[tutorial], which starts with a single step and ends with a
single step. You can upload the code to your congame server as described
in the sections below, where you have to provide the name of the study
that should be run as the @emph{Study ID}.

@subsection{Create a New Study}

@itemlist[
  @item{Save the code above in a file called @filepath{tutorial.rkt}}
  @item{Log in to your researcher account}
  @item{Go the @emph{Admin} page}
  @item{Click on @emph{New Study}}
  @item{Provide a @emph{Name} such as "Tutorial <your name>"}
  @item{As @emph{Type}, choose @emph{DSL}}
  @item{As @emph{Study ID}, take the ID provided by your source code, which for
  the code above is @emph{tutorial}}
  @item{As @emph{DSL source}, browse to your @filepath{tutorial.rkt} file}
  @item{Click the @emph{Create} button}
]

If everything went well, you will see a page with instances of your
tutorial study, which should be none.

@subsection{Create a New Instance of the Study}

Create a @emph{New Instance}. You can give it whatever name you want,
and don't need to add any other field. Simply click @emph{create}.

If everything went as planned, then when you go to the @emph{Dashboard},
you should see your study with the name you gave it as an instance. You
can now enroll in that study yourself (for testing) and should see the
first page.

Congratulations!

@section{Multi-Step Studies}

Having a study that consists of a single page isn't very interesting.
Let us add two more steps, one intermediate one, where we ask for the
name and age of the person, and a final one to thank the person by name.

There are several new parts in this multi-step study:

@itemize[
  @item{How to write a simple form to get data from the user and store it}
  @item{How to get and use data that is stored}
  @item{How to sequence multiple steps}
]

Let's cover each in turn.

@subsection{How to write a simple form}

The primary goal of studies is to collect data from participants, and
@racket[form]s are the main way of getting input from participants.
The simplest forms will contain one or more input fields, and a
@racket[submit-button]. The input field for free-form text answers (e.g.
when asking for a name) is @racket[input-text]. In order to store the
answer provided by the user, we need to provide an ID for each piece of
data we collect. This ID must be preceded by the characters @tt{#:}:

@codeblock|{
  @input-text[#:first-name]{What is your name?}
}|

This input field ensures that the answer the user provided is a string
(due to using @racket{input-text}) and stores the text under the ID
@racket[first-name].

Finally, we have to state where we want the submit-button to be placed
by using @racket{submit-button}.

Our complete form to get the first name and the age of a person will
then look as follows:

@margin-note{
  Note that every function - such as @racket[form], @racket[input-text],
  etc - used inside curly brackets should be preceded by the
  @"@"-symbol.
}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
@html{
  @form{
    @input-text[#:first-name]{What is your first name?}
    @input-number[#:age]{What is your age (in years)?}
    @submit-button}}
}|

It is important not to confuse square (@tt{[]}) and curly (@tt{{}})
brackets. The main difference is that curly brackets interpret their
content as a string by default. Therefore much of what users see will
be in curly brackets. Square brackets on the other hand interpret their
content as data: therefore identifiers of studies and steps, numbers,
or keys to extract data should be enclosed in square brackets. Square
brackets are optional, but when used have to come before curly brackets
(which are also optional).

@subsection{How to get and use stored data}

Once a study stores data, we can get it by using @code|{@get}|. Suppose
the user provided their first name, then we can get the value with
@code|{@get['first-name]}| -- note the single quote (') in front of
first-name, which identifies it as a @emph{symbol} rather than as the
object named @racket[first-name].

Thus the final step to thank the user will be:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (thank-you)
  @html{
    @h1{Thank you @get['first-name]}

    Thank you for participating in our survey @get['first-name]!})
}|

@subsection{How to sequence multiple steps}

Suppose that we have three steps, creatively named @racket[step1],
@racket[step2], and @racket[step3]. We will define these three steps
below. To create a study with these steps in order, with @racket[step3]
the final one, we write:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstudy three-steps
  [step1 --> step2 --> step3]
  [step3 --> step3])
}|

The first argument of @racket[study] is the ID of the study. It must
be followed by one or more transition entries enclosed in square
brackets. The simplest type of transition entry is a sequence of step
IDs connected by @racket[-->]'s, such as @racket[step1 --> step2]. The
arrow indicates that after completing @racket[step1], we transition to
@racket[step2].

Every step has to explicitly define a transition, even if it is meant to
be the final step. Thus to make @racket[step3] the final step, we have
to write that it transitions to itself: @racket[step3 --> step3].

Moreover, in order to know when to transition to the next step, we have
to add a @racket[button] to each step with a label for the button,
except for pages containing a @racket[form], which transitions to the
next page when the user clicks on the @racket[submit-button].

@subsection{Putting it all together}

Putting all of this together, we can create our first multi-step study
by updating @filepath{tutorial.rkt} as follows:

@codeblock|{
#lang conscript

(provide
 tutorial)

(defstep (description)
  @html{
    @h1{The study}

    Welcome to our study. In this study, we will ask for

    @ul{
      @li{your first name}
      @li{your age}}

    @button{Start Survey}})

(defstep (age-name-survey)
  @html{
    @h1{Survey}

    @form{
      @input-text[#:first-name]{What is your first name?}
      @input-number[#:age]{What is your age (in years)?}
      @submit-button}})

(defstep (thank-you)
  @html{
    @h1{Thank you @get['first-name]}

    Thank you for participating in our survey @get['first-name]!})

(defstudy tutorial
  [description --> age-name-survey --> thank-you]
  [thank-you --> thank-you])
}|

We have to update the code on the congame server to reflect these
changes. Go to the admin page, and follow these steps to update the
study code and the study run for tutorial:

@itemize[
  @item{Click on your existing study instance}
  @item{Click on @emph{Edit DSL}}
  @item{Pick the updated version of @filepath{tutorial.rkt}}
  @item{Click @emph{Update}}]

Try to resume the study. If you enrolled into the first
@racket{tutorial} study, which had a single step called @racket[start],
you should now see an error. This is because when you did enrolled into
@racket{tutorial}, you progressed to the step with the ID @emph{start}.
Since such a step does not exist in the latest version, you get an
error.

To fix this, you have to clear the progress of your user for this study
instance. Go to the admin page of the tutorial instance (@emph{Admin},
click on the name of your tutorial instance). Towards the bottom, you
will see a list of instances under @bold{Instance Name}. Click on your
instance. At the bottom of the next page is the list of participants who
have enrolled in this study. Click on your ID (which you can identify
by the email if you enrolled from your congame server). Then click on
@emph{Clear participant progress}. (Note: it may look like there was no
progress, if the table is empty. That's because the progress shows only
additional data that you store explicitly, not implicit progress such as
the current step you are on.)

Now you can bo back to the dashboard and go through the study.
Congratulations, this is your first survey in @tech{conscript}!


@section{Using Racket functions}

You can use a limited set of @tech{Racket} functions directly in @tech{conscript}.

To illustrate this, let us add a display of the person's age to the previous
study. It may seem straightforward, and you might try to do change the code of
the final @racket[thank-you] step as follows:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (thank-you)
  @html{
    @h1{Thank you, @get['first-name]}

    Thank you for participating in our survey, @get['first-name] - and for
telling us that you are @get['age] years old!}) }|

You might expect this to display the age on the page. Instead, you are likely to
find that the final page does not display the age at all, and you see only "You
are the most awesome &-year old!" (or some other strange character in place of
the @tt{&}) instead. What is going on?

What is going on is that when we are storing a number, we are storing a number
and not a string! So when we write @code|{@get['age]}| to display the age, we
are providing the age as a number and not as a string, and when the number is
sent, as bytes, to your browser, your browser interprets it as some other
special symbol. This leads to the strange display you get.

To fix this, all we need to do is to convert numbers to strings before
displaying them. Fortunately, @racket[~a] is provided by default, which turns
its argument into a string (and more). To use it, call it like @racket[get]:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (thank-you)
  @html{
        @h1{Thank you, @get['first-name]}

        Thank you for participating in our survey, @get['first-name] - and for
telling us that you are @~a[@get['age]] years old!}) }|

@section{Input Types}

Conscript provides the following input types by default, corresponding to their
HTML namesakes, wich can only be used inside of a @racket[@form]:

@itemlist[
  @item{@racket[input-text]: the user enters short text into a single-line box}
  @item{@racket[textarea]: the user enters a  long text into a multi-row box}
  @item{@racket[input-number]: the user enters a number}
  @item{@racket[input-range]: the user selects a number with a slider}
  @item{@racket[input-date]: the user selects or enters a date}
  @item{@racket[input-time]: the user selects or enters a time}
  @item{@racket[input-file]: the user selects a file to upload}
  @item{@racket[checkbox]: the user has to check a box}
  @item{@racket[radios]: the user selects one of several visible options}
  @item{@racket[select]: the user selects one of several options from a dropdown menu}
  ]

By default, every input field is required: a participant will not be able to
submit the form unless they fill in the field. If you want to make a field
optional, you have to set the @racket[#:required?] keyword to @racket[#false],
or its short form @racket[#f]. In addition, some inputs can take further keyword
arguments (those starting with @racket[#:]), such as numbers having a minimum or
maximum that they cannot exceed.

Let us illustrate this with age. First, everyone's age is positive, so let us
put a minimum for age at 0. Morevoer, some people would rather not reveal their
age, so let's make it optional. Then our input for age becomes:

@margin-note{While the order of the other keyword arguments does not matter,
the first keyword argument @emph{must} always be the identifier of the input,
here @racket[#:age]. }

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
@input-number[#:age #:required? #f #:min 0]{What is your age (in years)?}
}|

Sometimes when asking for a range, we may want to offer a slider, especially
when we don't expect people to have a precise number in mind. That's when we can
use @racket[input-range]. Like @racket[input-number], it takes optional keyword
arguments for @racket[#:min] and @racket[#:max]. When none are provided, the
browser will default to a range from 0 to 100. You can try out as follows:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
@input-number[#:agree]{How strongly do you agree with using `input-range` for
this question rather than `input-number`? (0: not at all, 100 completely)}
}|

@exercise{Build a study using all the inputs above except @racket[radio],
@racket[select], and @racket[input-file].}

@section{Writing pages in Markdown}

When writing mostly texts, it is often convenient to write it in markdown. In
markdown, '#', '##', etc indicate headers in decreasing order, enclosing a word
in single asterisks @code{*emphasizes*} it, double asterisks @code{**boldens**}
it, etc. See @hyperlink["https://commonmark.org/help/"]{this markdown help} for
details.

To write markdown, use @racket{@md} to create a whole markdown page. So we can
rewrite the steps in the tutorial study as follows:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (description)
  @md{
    # The study

    Welcome to our study. In this study, we will ask for

    - your first name
    - your age

    @button{Start Survey}})

(defstep (age-name-survey)
  @md{
    # Survey

    @form{
      @input-text[#:first-name]{What is your first name?}
      @input-number[#:age]{What is your age (in years)?}
      @submit-button}})

(defstep (thank-you)
  @md{
    # Thank you @get['first-name]

    Thank you for participating in our survey @get['first-name]!})
}|

Since it is easier to write, I suggest that you write most pages with lots of
text in markdown.

@section{Reusing steps and whole studies}

In many studies, we repeatedly measure the same thing: willingness to pay, mood,
fatigue, and many others. When doing so, we need need to take care in naming the
values that we measure so that we don't overwrite old values by the new ones.

First, let us try to define a step and reuse it 3 times:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(provide
  three-fatigues)

(defstep (description)
  @md{
    # Fatigue Survey

    In this study we will ask you three times how tired you are.

    @button{Start study}})

(defstep (how-tired-are-you)
  @md{
  # Fatigue question

  @form{
    @input-number[#:fatigue #:min 1 #:max 5]{On a scale from 1 (very tired) to 5
    (very awake), how tired are you?}
    @submit-button
  }})

(defstep (end)
  @md{
    # This is the end!

    Thank you for participating.})

(defstudy three-fatigues
  @; This will not work
  [description --> how-tired-are-you
               --> how-tired-are-you
               --> how-tired-are-you
               --> end]
  [end --> end])
}|

@margin-note{If you wonder why we did not decide to let @code|{a --> a --> b}|
stand for ``first step @racket[a], then step @racket[a], then step @racket[b]'',
ask yourself the following: how could you distinguish, in words to a person or
in code, between the first and second instance of step @racket[a]? To avoid
solving this problem implicitly, we prefer to require the study writer to give a
unique and explicitly specified name to every step.}

The above does not work, because @code|{a --> b}| stands for ``the step
@racket[a] is always followed by the step @racket[b]''. So @code|{a --> a --> a
--> b}| is both redundant and inconsistent. It is redundant because the first
@code|{a --> a}| means the same as the second. It is ambiguous because @code|{a
--> b}| contradicts @code|{a --> a}|, since it would mean that the step
@racket[a] is always followed by the step @racket[a] and also always followed by
the step @racket[b].

Moreover, even if this study did run, the problem is that the answer to the
latest question would always overwrite the previous answers, so we can never
store more than one answer.

One way to solve the problem, albeit a bit cumbersome, is to define three
separate steps that do the same thing, but store it in different values:

@codeblock|{
#lang conscript

(provide
  three-fatigues)

(defstep (how-tired-are-you1)
  @md{
  # Fatigue question

  @form{
    @input-number[#:fatigue1 #:min 1 #:max 5]{On a scale from 1 (very tired) to
    5 (very awake), how tired are you?}
    @submit-button
  }})

(defstep (how-tired-are-you2)
  @md{
  # Fatigue question

  @form{
    @input-number[#:fatigue1 #:min 1 #:max 5]{On a scale from 1 (very tired) to
    5 (very awake), how tired are you?}
    @submit-button
  }})

(defstep (how-tired-are-you3)
  @md{
  # Fatigue question

  @form{
    @input-number[#:fatigue3 #:min 1 #:max 5]{On a scale from 1 (very tired) to
    5 (very awake), how tired are you?}
    @submit-button
  }})

(defstudy three-fatigues
  [description --> how-tired-are-you1
               --> how-tired-are-you2
               --> how-tired-are-you3
               --> end]
  [end --> end])
}|

This has several problems. First, because I copy-pasted the code of the first
step and forgot to change the name of the input from @racket[fatigue1] to
@racket[fatigue2], the answer to the second step would overwrite the answer to
the first step. But even if we fixed this, one of the more famous mottos in
software engineering is @emph{Don't Repeat Yourself}, or @emph{DRY} for short.
While one can overdo it with DRY, here we repeated the same operation three
times, manually changing names of steps and of identifiers. This is error-prone
and brittle: imagine doing this for 10 repetitions, only to realize that you
want to change the wording, so now you have to find and change it in 10
different places.

This brings us to one of the nicer features of conscript: we can reuse whole
studies as substudies of a larger study. Moreover, by default data is stored on
substudies, so that different substudies do not overwrite each other's data. So
by defining a study that saves the answer under the id @tt{'fatigue}, we can
reuse it multiple times in a way that saves all the answers.

@codeblock[#:keep-lang-line? #f]|{
#lang conscript

@; take the `description` and `end` steps from above
@; ...

@; Add the following
(defstep (how-tired-are-you)
  @md{
  # Fatigue question

  @form{
    @input-number[#:fatigue #:min 1 #:max 5]{On a scale from 1 (very tired) to 5
    (very awake), how tired are you?}
    @submit-button
  }})

(defstudy fatigue
  @; `,(lambda () done)` is magic sauce that says that this substudy is `done`
  @; now. This exits the substudy and gives control to the parent study.
  [how-tired-are-you --> ,(lambda () done)])

(defstudy three-fatigues
  [description --> [fatigue1 fatigue]
               --> [fatigue2 fatigue]
               --> [fatigue3 fatigue]
               --> end]
  [end --> end])
}|

This is the first time that we define two studies and reuse the first study,
named @tt{fatigue} three times in the second one. To do soe, we define
transtions as follows: @tt{--> [<name-the-step> <study-or-step-to-run>]}, where
the @emph{<name-of-the-step>} is the unique id of this step as part of
@tt{three-fatigues}, while @tt{<study-or-step-to-run>} is the study (or step)
that should be run in this step. Hence we provide three different names, yet
reuse the same study each time.

It is necessary that the substudy ends in a transition @code|{... --> ,(lambda
() done)}|, which indicates that the substudy should exit and continue in the
parent study. I won't explain this code, just include it as a magic incantation.

@section{Other inputs: @racket[radio], @racket[select]}

Let us now implement radio buttons and selects, both of which allow the user to
select a single option from a list of predetermined options. The difference is
that with @racket[radio] buttons we display all the options with a radio button
next to each option, while with a @racket[select] the user chooses the option
from a long dropdown.

We will create a dropdown selector first for the question "What is your
occupation?", taken from the research team ACH91 of the ``Many Designs'' study:

@;FIXME put a proper link to the study here

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(define (socio-demographics)
  @html{
    @h1{Socio-demographics}

    Please answer the following questions:

    @form{
      @select[
        #:occupation
        '(("1"  . "Management, professional, and related")
          ("2"  . "Service")
          ("3"  . "Sales and office")
          ("4"  . "Farming, fishing, and forestry")
          ("5"  . "Constuction, extraction, and maintenance")
          ("6"  . "Production, transportation, and material moving")
          ("7"  . "Government")
          ("8"  . "Retired")
          ("9"  . "Unemployed")
          ("10" . "Student")
          ("11" . "Other"))
      ]{What is your occupation?}
      @radios[
        #:gender
        '(("1" . "Male")
          ("2" . "Female")
          ("3" . "Other"))
      ]{What is your gender?}
      @submit-button}})
}|

If a person chooses ``Government'' and ``Other'', then this will store the value
of @tt{"7"} for @racket[occupation] and @tt{"3"} for @racket[gender]. Notice
that these are both strings, not numbers! Now suppose that you you come across
these values in the database without context, then it is hard to figure out what
they mean. While the descriptions of the occupations are a bit long, those for
genders are short enough, so we can replace it by the following:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
@; the page and form omitted
@radios[
  #:gender
  '(("male"   . "Male")
    ("female" . "Female")
    ("other"  . "Other"))
]{What is your gender?}
}|

In this case, the string @tt{"other"} would have been saved in the database,
which along with the fact that the identifier is @tt{'gender} is pretty
self-documenting.

@section{CSS with #:style}

Often times, we want to format and style our pages. For basic formatting ---
such as @bold{bold}, @italic{italic}, or highlight code --- we can look up HTML
tags (see, e.g., the Mozilla Developer Network on
@hyperlink["https://developer.mozilla.org/en-US/docs/Learn/HTML/Introduction_to_HTML/HTML_text_fundamentals#emphasis_and_importance"]{basic
text formatting}) if you use @racket{html} to generate the page; or the markdown
syntax (e.g., at @hyperlink["https://commonmark.org/help/"]{Commonmark}).

Consider the following page, once using @racket{html}, once @racket{md}:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (basic-style-html)
  @html{
    @h1{A Page with Basic Formatting}

    The HTML tag for @em{emphasizing} a word is @em{em}, while the tag for
@strong{strongly} emphasizing a word is @strong{strong}.})

(defstep (basic-style-md)
  @md{
    # A Page with Basic Formatting

    The syntax for *emphasizing* a word in markdown is to wrap a string in
asterisks ("*"), while the syntax for **strongly** emphasizing a word is to wrap
a string in double asterisks ("**").
})
}|

Now suppose that we want to style this page so that the font of the title is in
red. For this we have to add CSS (Cascading Style Sheet) rules to the items that
we want to style. To learn more about CSS go to
@hyperlink["https://developer.mozilla.org/en-US/docs/Learn/CSS"]{MDN's resource
on CSS}. Here we assume that you know what CSS rules you want to use and show
how to add them to items. For simple styles, you can usually quickly find
appropriate CSS rule.

The most direct way to add CSS styles to an individual HTML element is
@emph{inline}, by defining the style directly on the element. For example, to
change the font color of the header to red, we set the @tt{style} to @tt{"color:
red"} on the @racket{h1} element:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (basic-style-html)
  @html{
    @h1[#:style "color: red"]{A Page with Basic Formatting}

    The HTML tag for @em{emphasizing} a word is @em{em}, while the tag for
@strong{strongly} emphasizing a word is @strong{strong}.
})
}|

To add multiple style properties, we list them all, separating them by a
semicolon. For instance, if we also want to center the header, then we need to
add the additional rule @tt{"text-align: center"}.

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (basic-style-html)
  @html{
    @h1[#:style "color: red; text-align: center;"]{A Page with Basic Formatting}

    The HTML tag for @em{emphasizing} a word is @em{em}, while the tag for
@strong{strongly} emphasizing a word is @strong{strong}.
})
}|

You should now have a red header, centered in the middle of the page.

Adding styles inline becomes old fast if you keep repeating the same style over
and over. Suppose that we had several headers on the same page and want all of
them to be in red and centered. The following monstrosity would work:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (messy-css)
  @html{
    @h1[#:style "color: red; text-align: center;"]{A Page with Multiple Repeated
Inline Styles}

    The HTML tag for @em{emphasizing} a word is @em{em}, while the tag for
@strong{strongly} emphasizing a word is @strong{strong}.

    @h2[#:style "color: red; text-align: center;"]{Another Header}


    @h2[#:style "color: red; text-align: center;"]{And Another}
})
}|

@margin-note{An even better way is to define a single CSS file (or set of files)
that you reuse for your whole study. This however requires that you bundle the
CSS files with the server, so it requires that you have access to the server and
redeploy when you change the files.}
But a much better way is to use an inline style sheet defined inside of a
@racket{style} element like so:


@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (clean-css)
  @html{

    @style{
      h1, h2 {
        color: red;
        text-align: center;
      }
    }

    @h1{A Page with Multiple Repeated Inline Styles}

    The HTML tag for @em{emphasizing} a word is @em{em}, while the tag for
@strong{strongly} emphasizing a word is @strong{strong}.

    @h2{Another Header}

    @h2{And Another}
  })
}|

This defines a style that should hold for the whole page, in this case that
every @racket{h1} and @racket{h2} element should be in red font and centered.

Finally, if you want to reuse this style across multiple steps, you don't want
to keep adding the whole style each time. In that case, you can define the style
element in a reusable way by using @racket{html*}. While @racket{html} defines a
full and complete HTML page --- wrapped inside all the elements needed ---
@racket{html*} defines a piece of an HTML page that can be used to create a
larger page.

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(provide
 reuse-style)

(define my-style
  @html*{
    @style{
      h1, h2 {
        color: red;
        text-align: center;
      }
    }
  })

(defstep (use-style)
  @html{
    @my-style

    @h1{First use}

    @button{Continue}
  })

(defstep (reuse-style)
  @html{
    @my-style

    @h1{Using the same style again}

  })

(defstudy reuse-style
  [use-style --> reuse-style
             --> reuse-style])
}|

@section{Randomizing participants into treatments}

To randomize participants into treatments, we will use the function
@racket[assigning-treatments]. Unlike the rest of the functionality we have used
so far, @racket[assigning-treatments] is not part of the core of
@racket[conscript]. To use it, we therefore first need to @racket[require] the
module that defines this function by including the following line at the top of
the file:

@codeblock|{
#lang conscript

(require conscript/survey-tools)
}|

If you fail to @racket[require] @tt{conscript/survey-tools} and you try to run your
code, then you will get an error that @tt{assigning-treatments} is undefined.

To illustrate how to use this, we will define two toy studies, called
@tt{control-study} and @tt{treatment-study}. Our goal is to randomize
participants in a balanced way: for every pair of participants that arrives, we
want one of them to be assigned to control and one to treatment in a random
fashion. To do so, we create first a landing page where we welcome participants,
and add a function to the @tt{Continue} @racket[button], which takes care of
this. Finally, if the participant is in the @tt{control} treatment, they
continue to the @tt{control-study}, otherwise they continue to the
@tt{treatment-study}.

@codeblock|{
#lang conscript

(require conscript/survey-tools)

(provide
  randomized-study)

(define (randomize-treatments)
  (assigning-treatments
    (list 'control 'treatment)))

(defstep (control-page)
  @md{
    # Control Page

    You are in the control treatment.

    @button{Continue}})

(defstudy control-study
  [control-page --> ,(lambda () done)])

(defstep (treatment-page)
  @md{
    # Treatment Page

    You are in the treatment treatment. (I know.)

    @button{Continue}
})

(defstudy treatment-study
  [treatment-page --> ,(lambda () done)])

(defstep (welcome)
  @md{
    # Welcome

    Thank you for participating in this study.

    @button[randomize-treatments]{Continue}})

(define (final-page)
  (define participant-treatment
    (~a (get* 'role)))

  @md{
    # Final Page

    Thank you for having participated in the *@|participant-treatment|* treatment.})

(defstudy randomized-study
  [welcome --> ,(lambda ()
                   (case (get* 'role)
                     [(control)   'control-study]
                     [(treatment) 'treatment-study]))]

  [control-study --> final-page]
  [treatment-study --> final-page]
  [final-page --> final-page])
}|

When a participant hits the @tt{Continue} button, the function
@racket[randomized-treatments] is called, which randomizes the order of the
treatments and balance them across participants as they arrive across the
treatments that you list in the call to @racket[assigning-treatments]. That
means that when there are two treatments (as in the example), every set of 2
participants is assigned to these 2 roles to ensure that we always have 1 person
in control and 1 in treatment. The order in which they are assigned these roles
is randomized.

This works by storing the treatment of the participant as a participant variable in @racket['role] @;and the set of treatments for the current group of participants as an instance variable in @racket['treatments].
Importantly, this variable is stored at the global level @;(@tt{(*root*)})
, so it can be retrieved in any substudy via @racket[get*]. @;, @racket[put*] for the @racket['role] and with @racket[get/instance/global] and @racket[put/instance/global] for the @racket['treatments] (you should not mess with the latter though).
@;You can overrule these with @racket[#:treatments-key] and @racket[#:role-key]. So if you prefer storing the next set of treatments in @racket['my-treatments] and the treatment of the participant in @racket['treatment], then you need to change the action as follows:

Once @racket[randomized-treatments] is called, we then check in the transition
whether the @racket['role] of the participant is @tt{control} or @tt{treatment}
and send them to the appropriate branch of the study.

On the final page, we display the role in the text, by first assigning it to the
variable @racket[participant-treatment]. In order to display this correctly, we
have to turn @racket['control] into the string @racket["control"], which is what
@racket[(~a (get 'role))] does. Then inside the markdown, we want to
@emph{emphasize} the name of the treatment, so we surround it by asterisks.
However, this would lead to an error, since it would attempt to look up the
variable @racket[participant-treatment*]. Therefore we surround
@racket[participant-treatment] by bars (@tt{|}), which highlights the start and
end of the variable we are looking up.

@subsection{Random Assignment to Additional Groups}

Now suppose that both in control and treatment, we want to further randomize
participants into of three subgroups: ``buyer'', ``seller'', and ``observer'',
with each role making different choices. To illustrate that we don't have to
have the same number in each group, let's suppose that for every group of 10
people, we want 2 to be buyers, 3 to be sellers, and 5 to be observers.

While we could use @racket[assigning-treatments] to do so, this would overwrite
the role of ``treatment'' or ``control''. We therefore have to call
@racket[assigning-treatments] with two additional keyword arguments:

@codeblock|{
#lang conscript

; Define same steps as above
(define (randomize-market-roles)
  (assigning-treatments
   (list 'buyer 'buyer 'seller 'seller 'seller
    'observer 'observer 'observer 'observer 'observer)
   #:treatments-key 'market-roles
   #:role-key       'market-role))

(defstep (introduce-market-role)
  @md{
    # Market Roles

    We will now assign you one of the following roles:

    1. Buyer
    2. Seller
    3. Observer

    @button[randomize-market-roles]{Assign Role}})

(defstep (show-market-role)
  (define mr
    (get 'market-role))

  @md{
    # Show Market Role

    Your role is: @|mr|.

    @button{Next}})

; Update control and treatment studies as follows
(defstudy treatment-study
  [treatment-page --> introduce-market-roles
                  --> show-market-role
                  --> ,(lambda () done)])

(defstudy control-study
  [control-page --> introduce-market-roles
                --> show-market-role
                --> ,(lambda () done)])

; The rest as before
; ...
}|

With this change, we can have every participant both assigned to either
``treatment'' or ``control'', as well as to one of ``buyer'', ``seller'', and
``observer''.

Note that we get the role of treatment with @racket[(get 'role)] --- the
default key for roles --- while we get the ones for the market role with
@racket[(get 'market-role)], since that's the value for the @racket[#:role-key]
keyword argument in the call to @racket[assigning-treatments].

@section{How to add Images}

In order to add images --- or any other files --- to use in a page, we need to
upload a zip folder containing our study and all the resources that we need. The
zip file must contain a file named @tt{study.rkt} which provides the study we
want to run. We then can declare static resources such as images relative to
this @tt{study.rkt} file: that means that if the zip folder contains the
@tt{study.rkt} file and a directory called @tt{img/} where we store an image
called @tt{code-screenshot.png}, say, then we declare the static resource for
this image with @code|{(define-static-resource my-screenshot
"img/code-screenshot.png")}|.

To create a complete example, do the following:

@itemlist[
  #:style 'ordered
  @item{Create a new folder, which you can call @tt{zipped-study-with-images.}}
  @item{Inside this folder, create a file called @tt{study.rkt}. @bold{The file
  must be called @tt{study.rkt}, nothing else!}}
  @item{Inside this folder --- and next to @tt{study.rkt} --- create another
  folder called @tt{img}. You can call this folder anything, as long as you
  change the code below accordingly.}
  @item{Inside of the @tt{img} folder, put one or more images you want to
  display inside your study, and call one of them @tt{test.png}.}
]

If you followed the above steps, then you are ready to write the following code
inside of @tt{study.rkt}:

@codeblock|{
#lang conscript

(provide
  image-study)

; Create the resource that points at the image.
(define-static-resource screenshot "img/test.png")

(defstep (show-image)
  @html{
    @h1{A Page with an Image}

    ; The image tag takes two keywords:
    ; - #:alt holds a textual replacement for the image.
    ; - #:src contains the path to the image
    ; See https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img.
    @img[
      #:alt "Screenshot"
      ; `resource-uri` creates the path for static resources, here an image
      #:src (resource-uri screenshot)
    ]})

(defstudy image-study
  [show-image --> show-image])
}|

You can remove the comments, which explain how this works. In more detail:

@itemlist[
  #:style 'ordered
  @item{@code|{(define-static-resource screenshot "img/test.png")}|: declares
  that we want to use the static resource that can be found at that path.}
  @item{@code|{@img[#:alt "Screenshot" #:src (resource-uri screenshot)]}|:
  creates the HTML element for an image, with the alternative text of
  "Screenshot" and with the correct path to the image generated via
  @racket[resource-uri] based on the static resource @racket[screenshot].}
]

Finally, once you have saved this file, you should compress the whole folder
@tt{zipped-study-with-images} into a zip file, and then upload this entire
folder instead of just the study file on the server.

If everything goes well, you now have a study displaying exactly one page with
one image.

@section{Basic Arithmetic}

You can use basic arithmetic in @tech{Racket}, which is infix based:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
; Add two or more numbers
(+ 2 3)     ; 2 + 3
(+ 1 2 3 4) ; 1 + 2 + 3 + 4

; Subtract two numbers
(- 3 4) ; 3 - 4

; Multiply two or more numbers
(* 2 3)   ; 2 * 3
(* 2 3 4) ; 2 * 3 * 4
(* 2 -3)  ; 2 * (-3)

; Divide
(/ 9 3)   ; 9 / 3 => 3
(/ 7 2)   ; 7 / 2 => the fraction 7/2!
(/ 7 2.0) ; 7 / 2.0 => floating point 3.5

; compute mean of 4 numbers
(/ (+ 1 2 3 4) 4.0)

; Get the remainder or quotient
(remainder 7 2) ; => 1
(quotient 7 2)  ; => 3
}|

If you want to include it in html markup, don't forget to transform the number to a string by using either @racket[number->string] or @racket[~a] (which turns every input into a string, so also works for symbols (@tt{'age}) or lists or other items):

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (numbers-to-strings)
  @md{# Twice your age is @(number->string (* 2 (get 'age)))

      Your remaining life expectancy is @(~a (- 80 (get 'age))).

      @button{Next}})
}|

@section{Studies with Logic}

We often want to respond to participants, whether it is to display different
messages as we progress, or based on their answers. We will now create a few
studies using some of the programming features that conscript provides.

First, let us count down from 10 to 0 and display the pages to the
user. We could of course define a separate step for each number,
calling them @racket[step10] down to @racket[step0] and then string
them together as @tt{step10 --> ... --> step0}, but that is
tedious. Instead, for every user, let us store the value of
@racket[counter] and every time the user progresses, we decrease the
value of @racket[counter] and display it on the screen. To store a
value for a user, we use @code[#:lang "conscript"]|{(put id value)}|. @margin-note{@racket[put] is what the forms use to store the answers of users in the database, but this happens without you having to do anything. Thus, by default, an input @code|{@input-number[#:the-number]}| will call @code|{(put 'the-number value-provided-by-the-user)}| behind the scenes, which is why you can get the value via @code|{(get 'the-number)}|.}


To implement the countdown, we need to do two things. First, we need an action that initializes the user's counter to 10; second, we need an action that decreases the counter by 1 whenever we advance:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(define (initialize-counter)
  (put 'counter 10))

(define (decrement-counter)
  ; Get the old value --- (get 'counter) --- subtract 1 from it, then store the
  ; new value in 'counter.
  (put 'counter (sub1 (get 'counter))))
}|

This uses @racket[sub1], which subtracts 1 from its argument.

Here we will implement the logic by calling the action whenever the next button is clicked. We can pass an argument to the @racket[button] via @code[#:lang "conscript"]|{@button[my-action]{Next}}|, which will evaluate @racket[my-action] and then advance to the next step.

@codeblock|{
#lang conscript

@; Code defining the actions above
@; ...

(defstep (description)
  @md{# The countdown is about to begin

      @button[initialize-counter]{Start Countdown}})

(defstep (show-counter)
  @md{@(number->string (get 'counter))

      @button[decrement-counter]{Count down!}})

(defstudy countdown
  [description --> show-counter]
  [show-counter --> show-counter])
}|

While this works, it has a fatal flaw. We keep counting down forever and ever. (Try it.) Instead, we would like to stop once we hit 0, and display the launch page.

@subsection{Conditions}

In order to stop once the counter hits 0, we need to change the transitions. Specifically, we want to transition to the @tt{launch} step when the counter is 0, and otherwise keep displaying the @tt{show-counter} step. To do so, we use @racket[if] inside a transition, which has to be wrapped in something mysterious called a @racket[lambda]:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (launch)
  @md{# Study launched!})

(defstudy countdown
  [description --> show-counter
               --> ,(lambda ()
                      (if (= (get 'counter) 0)
                         'launch
                         'show-counter))]
  [launch --> launch])
}|

For now, ignore @racket[lambda] line, simply type it and run with it. As for the @racket[if], it is straightforward: it first checks a condition, here @code[#:lang "conscript"]|{(= (get 'counter) 0)}|, which checks whether the value of @tt{'counter} is 0 or not. If the condition holds, then the transition continues to the value of the next expression (called the @tt{if} clause), here @tt{'launch}. If the condition fails, then the transition continues to the value of the final expression (called the @tt{else} clause), here @tt{'show-counter}.@margin-note{Note that the name of the step has a quote (') in front of it, so that @tt{'show-counter} denotes the symbol @tt{show-counter}. This is because internally the names of steps are stored as symbols.}


@;{
@section{Studies involving multiple Participants}

Many studies involve participants who interact or affect each other: the dictator game, the public goods game, any study with markets, auctions, or negotiations. This requires also that we somehow share values across participants. While @racket{get} and @racket{put} provide a way to store values for a given participant, these values are private. This makes sense by default. Consider what would happen in the countdown study if two participants took the same study instance, and one participant has already progressed to the point where the counter is 3. Then the other participant would see the counter at 3 right from the start, which is clearly not the behavior we want.

We will start with a specific, but particularly useful feature that we can implement with multiple participants: an admin page for your study instance that only you (the owner of the study instance) can see. You can display information about the study there, whether about progress by participants or some summaries on their answers. For now, let us have a study where participants provide their name and age, and on the admin page we show how many participants have completed the study so far.

For this, we use the function @racket[current-participant-owner?], which returns true (@racket[#t]) when the current user is the owner of the instance, and false (@racket[#f]) otherwise.

@codeblock[#:keep-lang-line? #f]|{
#lang scribble/manual

@; Same code as in tutorial 2 above for the steps of normal participants

@step[admin]{
  @h1{You reached the admin page!}
}

@study[
  survey-with-admin
  #:transitions
  [start --> @(ev (lambda ()
                    (cond [(current-participant-owner?) 'admin]
                          [else 'description])))]
  [admin --> admin]
  [description --> age-name-survey --> thank-you --> thank-you]]
}|

With this code, the owner of the study instance will see the start page, and after that the admin page and remain there. Study participants on the other hand will be guided to the normal study instead. Right now, the admin page doesn't provide any useful data. In order to share data from the participants with the owner, we need to use @racket[get/instance] and @racket[put/instance] instead of @racket[get] and @racket[put]. @racket[put/instance] stores the data in a way that we can access it with @racket[get/instance] from each participant. This also means that if two participants store something on an instance, they overwrite each others value. Therefore we have to make sure first get the old value, and then update it.

For now, all we want is to count how many participants completed the study, so every time a participant gets to the @tt{thank-you} page, we increment an @emph{instance-wide} counter by 1.

@codeblock[#:keep-lang-line? #f]|{
#lang scribble/manual

@action[initialize-count-if-needed]{
  @(ev
    (begin
      (unless (get/instance 'participant-count #f)
        (put/instance 'participant-count 0))
      ))
}

@step[start]{
  @h1{Start page}

  @button[#:action initialize-count-if-needed]{Next}
}

@action[increment-participant-count]{
  @(ev
    (begin
      (define participant-count
        (get/instance 'participant-count))
      (put/instance 'participant-count (add1 participant-count))
    ))
}

@step[thank-you #:pre increment-participant-count]{
  @; old code
  @; ...
}

@step[admin]{
  @h1{Admin}

  Number of participants who completed the study: @(ev (get/instance 'participant-count))
}

}|

Next, let us implement a simple version of the dictator game to highlight a more complicated case of multi-person experiment. In our version, the person chooses between ($10, $0) and ($5, $5): i.e. taking $10 for themselves and nothing for the other, or giving $5 dollars to both.

@codeblock[#:keep-lang-line? #f]|{
#lang scribble/manual

@action[assign-roles]{
  @(ev
    (begin
      (define next-pid
        (get/instance 'pid 0))
      (put 'pid next-pid)
      (cond [(even? next-pid) (put 'role "Dictator")]
            [else (put 'role "(Non-)Receiver")])
      (put/instance 'pid (add1 next-pid))
    ))
}

@step[start #:pre assign-roles]{
  @h1{You are in the role of @(ev (get 'role))}

  @button{Next}
}

@step[choice]{

  @form{
    @radios[
      payment-string
      '(
        ("10" . "$10 for yourself, $0 for the other perons")
        ("5"  . "$5 for yourself, $5 for the other person")
       )
    ]{Please choose which of these options you prefer:}
    @submit-button}
}

@action[check-answer]{
  @(ev
    (begin
      (define payments
        (get/instance 'payments (hash)))
      (define receiver-payment
        (hash-ref payments (get 'pid) #f))
      (cond [receiver-payment (put 'payment receiver-payment)]
            [else (put 'payment #f)])))
}

@step[wait]{

  @h1{Refresh this screen regularly}

  Check back later to see if your partner has made their choice yet.

  @button[#:action check-answer]{Check the answer}
}

@action[update-receiver-payment]{
  @(ev
     (begin
       @; We pair participant with id 1 with participant
       @; with id 0; participant with id 3 with participant with id 2;
       @; and so on
       (define payment
         (string->number (get 'payment-string)))
       (put 'payment payment)
       (define receiver-id
         (add1 (get 'pid)))
       (define receiver-payment
         (- 10 payment))
       (define current-payments
         (get/instance 'payments (hash)))
       (define new-payments
         (hash-set current-payments receiver-id receiver-payment))
       (put/instance 'payments new-payments)))
}

@step[display-dictator #:pre update-receiver-payment]{
  @h1{You will receive $@(ev (number->string (get 'payment)))}
}

@step[display-receiver]{
  @h1{You will receive $@(ev (number->string (get 'payment)))}
}

@study[
  baby-dictator
  #:transitions
  @; everyone
  [start --> @(ev (lambda () (cond [(even? (get 'pid)) 'choice]
                        [else 'wait])))]
  @; dictator
  [choice --> display-dictator]
  [display-dictator --> display-dictator]
  @; receiver
  [wait --> @(ev (lambda () (cond [(get 'payment #f) 'display-receiver]
                       [else 'wait])))]
  [display-receiver --> display-receiver]
]
}|

}
