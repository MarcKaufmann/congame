#lang scribble/manual

@(require (for-label conscript racket/base))

@(define exercise-counter 0)

@(define (exercise . description)
   (set! exercise-counter (add1 exercise-counter))
   (para
    (bold (format "Exercise ~a: " exercise-counter)) description))

@; TODO
@; Be consistent in the use of 'I', 'We', 'you'

@title{Conscript Tutorial}

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
  @item{As @emph{Study ID}, take the ID provided by your source code, which for the code above is @emph{tutorial}}
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

To illustrate this, let us add a display of the person's age to the previous study. It may seem straightforward, and you might try to do change the code of the final @racket[thank-you] step as follows:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(defstep (thank-you)
  @html{
    @h1{Thank you, @get['first-name]}

    Thank you for participating in our survey, @get['first-name] - and for
telling us that you are @get['age] years old!}) }|

You might expect this to display the age on the page. Instead, you are likely to find that the final page does not display the age at all, and you see only "You are the most awesome &-year old!" (or some other strange character in place of the @tt{&}) instead. What is going on?

What is going on is that when we are storing a number, we are storing a number and not a string! So when we write @code|{@get['age]}| to display the age, we are providing the age as a number and not as a string, and when the number is sent, as bytes, to your browser, your browser interprets it as some other special symbol. This leads to the strange display you get.

To fix this, all we need to do is to convert numbers to strings before displaying them. Fortunately, @racket[~a] is provided by default, which turns its argument into a string (and more). To use it, call it like @racket[get]:

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

Let us illustrate this with age. First, everyone's age is positive, so let us put a minimum for age at 0. Morevoer, some people would rather not reveal their age, so let's make it optional. Then our input for age becomes:

@margin-note{
While the order of the other keyword arguments does not matter, the first keyword argument @emph{must} always be the identifier of the input, here @racket[#:age].
}

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
@input-number[#:age #:required? #f #:min 0]{What is your age (in years)?}
}|

Sometimes when asking for a range, we may want to offer a slider, especially when we don't expect people to have a precise number in mind. That's when we can use @racket[input-range]. Like @racket[input-number], it takes optional keyword arguments for @racket[#:min] and @racket[#:max]. When none are provided, the browser will default to a range from 0 to 100. You can try out as follows:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
@input-number[#:agree]{How strongly do you agree with using `input-range` for
this question rather than `input-number`? (0: not at all, 100 completely)}
}|

@exercise{Build a study using all the inputs above except @racket[radio], @racket[select], and @racket[input-file].}

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

Since it is easier to write, I suggest that you write most pages with lots of text in markdown.

@section{Reusing steps and whole studies}

In many studies, we repeatedly measure the same thing: willingness to pay, mood, fatigue, and many others. When doing so, we need need to take care in naming the values that we measure so that we don't overwrite old values by the new ones.

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
    @input-number[#:fatigue #:min 1 #:max 5]{On a scale from 1 (very tired) to 5 (very awake), how tired are you?}
    @submit-button[]
  }})

(defstep (done)
  @md{
    # You are done!

    Thank you for participating.})

(defstudy three-fatigues
  @; This will not work
  [description --> how-tired-are-you
               --> how-tired-are-you
               --> how-tired-are-you
               --> done]
  [done --> done])
}|

@margin-note{If you wonder why we did not decide to let @code|{a --> a --> b}| stand for ``first step @racket[a], then step @racket[a], then step @racket[b]'', ask yourself the following: how could you distinguish, in words to a person or in code, between the first and second instance of step @racket[a]? To avoid solving this problem implicitly, we prefer to require the study writer to give a unique and explicitly specified name to every step.}

The above does not work, because @code|{a --> b}| stands for ``the step @racket[a] is always followed by the step @racket[b]''. So @code|{a --> a --> a --> b}| is both redundant and inconsistent. It is redundant because the first @code|{a --> a}| means the same as the second. It is ambiguous because @code|{a --> b}| contradicts @code|{a --> a}|, since it would mean that the step @racket[a] is always followed by the step @racket[a] and also always followed by the step @racket[b].

Moreover, even if this study did run, the problem is that the answer to the latest question would always overwrite the previous answers, so we can never store more than one answer.

One way to solve the problem, albeit a bit cumbersome, is to define three separate steps that do the same thing, but store it in different values:

@codeblock|{
#lang conscript

(provide
  three-fatigues)

(defstep (how-tired-are-you1)
  @md{
  # Fatigue question

  @form{
    @input-number[fatigue1 #:min 1 #:max 5]{On a scale from 1 (very tired) to 5 (very awake), how tired are you?}
    @submit-button[]
  }})

(defstep (how-tired-are-you2)
  @md{
  # Fatigue question

  @form{
    @input-number[fatigue1 #:min 1 #:max 5]{On a scale from 1 (very tired) to 5 (very awake), how tired are you?}
    @submit-button[]
  }})

(defstep (how-tired-are-you3)
  @md{
  # Fatigue question

  @form{
    @input-number[fatigue3 #:min 1 #:max 5]{On a scale from 1 (very tired) to 5 (very awake), how tired are you?}
    @submit-button[]
  }})

(defstudy three-fatigues
  [description --> how-tired-are-you1
               --> how-tired-are-you2
               --> how-tired-are-you3
               --> done]
  [done --> done])
}|

This has several problems. First, because I copy-pasted the code of the first step and forgot to change the name of the input from @racket[fatigue1] to @racket[fatigue2], the answer to the second step would overwrite the answer to the first step. But even if we fixed this, one of the more famous mottos in software engineering is @emph{Don't Repeat Yourself}, or @emph{DRY} for short. While one can overdo it with DRY, here we repeated the same operation three times, manually changing names of steps and of identifiers. This is error-prone and brittle: imagine doing this for 10 repetitions, only to realize that you want to change the wording, so now you have to find and change it in 10 different places.

This brings us to one of the nicer features of conscript: we can reuse whole studies as substudies of a larger study. Moreover, by default data is stored on substudies, so that different substudies do not overwrite each other's data. So by defining a study that saves the answer under the id @tt{'fatigue}, we can reuse it multiple times in a way that saves all the answers.

@codeblock[#:keep-lang-line? #f]|{
#lang conscript

@; take the `description` and `done` steps from above
@; ...

@; Add the following
(defstep (how-tired-are-you)
  @md{
  # Fatigue question

  @form{
    @input-number[fatigue #:min 1 #:max 5]{On a scale from 1 (very tired) to 5 (very awake), how tired are you?}
    @submit-button[]
  }})

(defstudy fatigue
  [how-tired-are-you --> ,(lambda () done)])

(defstudy three-fatigues
  [description --> [fatigue1 fatigue]
               --> [fatigue2 fatigue]
               --> [fatigue3 fatigue]
               --> done]
  [done --> done])
}|

This is the first time that we define two studies and reuse the first study, named @tt{fatigue} three times in the second one. To do soe, we define transtions as follows: @tt{--> [<name-the-step> <study-or-step-to-run>]}, where the @emph{<name-of-the-step>} is the unique id of this step as part of @tt{three-fatigues}, while @tt{<study-or-step-to-run>} is the study (or step) that should be run in this step. Hence we provide three different names, yet reuse the same study each time.

It is necessary that the substudy ends in a transition @code|{... --> ,(lambda () done)}|, which indicates that the substudy should exit and continue in the parent study. I won't explain this code, just include it as a magic incantation.

@section{Other inputs: @racket[radio], @racket[select]}

Let us now implement radio buttons and selects, both of which allow the user to select a single option from a list of predetermined options. The difference is that with @racket[radio] buttons we display all the options with a radio button next to each option, while with a @racket[select] the user chooses the option from a long dropdown.

We will create a dropdown selector first for the question "What is your occupation?", taken from the research team ACH91 of the ``Many Designs'' study: @;FIXME put a proper link to the study here

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(define (socio-demographics)
  @html{
    @h1{Socio-demographics}

    Please answer the following questions:

    @form{
      @select[
        #:occupation
        "What is your occupation?"
        '(
          ("1"  . "Management, professional, and related")
          ("2"  . "Service")
          ("3"  . "Sales and office")
          ("4"  . "Farming, fishing, and forestry")
          ("5"  . "Constuction, extraction, and maintenance")
          ("6"  . "Production, transportation, and material moving")
          ("7"  . "Government")
          ("8"  . "Retired")
          ("9"  . "Unemployed")
          ("10" . "Student")
          ("11" . "Other"))]
      @radios[
        #:gender
        "What is your gender?"
        '(
          ("1" . "Male")
          ("2" . "Female")
          ("3" . "Other")
          )]
      @submit-button}})
}|

If a person chooses ``Government'' and ``Other'', then this will store the value of @tt{"7"} for @racket[occupation] and @tt{"3"} for @racket[gender]. Notice that these are both strings, not numbers! Now suppose that you you come across these values in the database without context, then it is hard to figure out what they mean. While the descriptions of the occupations are a bit long, those for genders are short enough, so we can replace it by the following:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
@; the page and form omitted
@radios[
  #:gender
  '(
    ("male"   . "Male")
    ("female" . "Female")
    ("other"  . "Other"))
]
}|

In this case, the string @racket{"other"} would have been saved in the database, which along with the fact that the identifier is @racket['gender] is pretty self-documenting.

@;{
@; Continue here next time
@; Add exercises, that's the main thing needed.

@section{Exercises}

@exercise[1]{Create a study where the user enters a number between 0 and 5 and then display double the number on the next page.}

@exercise[2]{Implement a step that elicits a multiple price list. How painful would it be to figure out at which bullet point people are switching?}

@exercise[3]{Add an admin page to the study in part 1 that shows you how many participants have already completed the study and what numbers they have chosen.}

@section{Studies with Logic}

We often want to respond to participants, whether it is to display different messages as we progress, or based on their answers. We will now create a few studies using some of the programming features that conscript provides.

First, let us count down from 10 to 0 and display the pages to the
user. We could of course define a separate step for each number,
calling them @racket[step10] down to @racket[step0] and then string
them together as @tt{step10 --> ... --> step0}, but that is
tedious. Instead, for every user, let us store the value of
@racket[counter] and every time the user progresses, we decrease the
value of @racket[counter] and display it on the screen. To store a
value for a user, we use @code[#:lang "scribble/manual"]|{@put[id value]}|.

The most important building block for this is the @code[#:lang
"scribble/manual"]|{@action}| operator. Whenever you want to change or update
some value or variable in conscript, you have to define a named @tt{action}
which will be evaluated when called. In the case of the countdown, we need to do
two things. First, we need an action that initializes the user's counter to 10;
second, we need an action that decreases the counter by 1 when called:


@codeblock[#:keep-lang-line? #f]|{
#lang scribble/manual

@action[initialize-counter]{
  @(ev (put 'counter 10))
}

@action[decrement-counter]{
  @(ev (put 'counter (sub1 (get 'counter))))
}
}|

Here we use @racket[sub1], which subtracts 1 from its argument. Later we'll see another way to do basic arithmetic.

Next, we need to call the @tt{action}s in the right places. There are two places where you can use actions: before a step is displayed by using @code|{@step[step-name #:pre action-id]}|; or after a button click (and before the next step is executed) using @code|{@button[#:action-id action-id]{Next}}|. We will the button approach here:

@codeblock[#:keep-lang-line? #f]|{
#lang scribble/manual

@; Here goes the above code defining the actions
@; ...

@step[description]{
  @h1{The countdown is about to begin}

  @button[#:action initialize-counter]{Start Countdown}
}

@step[show-counter]{
  @h1{@(ev (number->string (get 'counter)))}

  @button[#:action decrement-counter]{Count down!}
}

@study[
  countdown
  #:transitions
  [description --> show-counter]
  [show-counter --> show-counter]
]
}|

While this works, it has a fatal flaw. We keep counting down forever and ever. Instead, we would like to stop once we hit 0, and display the launch page.

@section{Conditions}

In order to stop once the counter hits 0, we need to change the transitions. Specifically, we want to transition to the @tt{launch} step when the counter is 0, and otherwise keep displaying the @tt{show-counter} step. To do so, we use @racket[cond] inside a transition, which has to be wrapped in something mysterious called a @racket[lambda]:

@codeblock[#:keep-lang-line? #f]|{
#lang scribble/manual

@step[launch]{
  @h1{Study launched!}
}

@study[
  countdown
  #:transitions
  [description --> show-counter
               --> @(ev (lambda ()
                          (cond
                            [(= (get 'counter) 0)
                             'launch]
                            [else
                             'show-counter])))]
  [launch --> launch]
]
}|

For now, ignore what the @racket[lambda] part does, simply type it and run with it. As for the @racket[cond], it is relatively straightforward: it consists of two or more clauses that are wrapped in square brackets ('[]'). Each clause starts with a condition, such as @tt{(= (get 'counter) 0)}, which checks whether the value of @tt{'counter} is 0 or not. If the condition holds, then the transition continues to the step at the end of the clause, here @tt{'launch}, which has a quote (') in front of it. (Note: this will soon change, as we will write it @tt{(goto launch)}, with no quote (') in front of launch.)

The final clause must always start with the keyword @racket[else], which is a catchall for the case where none of the conditions in previous clauses were met.

@section{CSS with #:style}

On the launch page, let us highlight the font of "Study launched!" in red, which requires that we change the following CSS rule (CSS stands for @emph{Cascading Style Sheet}) with "h1 { color: red; }".


In @tech{conscript}, we can add CSS styles to @tt{div} tags by using the @tt{#:style} keyword argument. To change the @tt{h1} tag, we need to wrap it in a @tt{div} tag and style it accordingly:

@codeblock|{
@step[launch]{
  @div[#:style "color: red;"]{
    @h1{Study launched!}
  }
}
}|

To add multiple style properties, we separate them by a semicolon:

@codeblock|{
@step[launch]{
  @div[#:style "color: red; font-size: 2rem;"]{
    @h1{Study launched!}
  }
}
}|

@section{Intermezzo: Some exercises}

@bold{Exercise 1} The function @racket[rand] can be used to generate random numbers: @tt{(rand 10)} returns a random integer between 0 and 9. Use this to create the following study. Draw a random number between 0 and 9 for a person and store it. Now repeat the following steps three times:

@itemlist[
  @item{Elicit the person's belief that their number is greater than or equal to 5}
  @item{Pick a random number. Tell the person whether their number is larger than (or equal to) or smaller than this new random number, which you show them.}
]

For example, you might pick the random number 3 for them. Then you pick the numbers 6 ("Your number is smaller than 6"), 9 ("Your number is smaller than 9"), and 2 ("Your number is larger than (or equl to) 2").

@bold{Exercise 2} Pick an experiment from a paper of your choice. Stub out the experiment: this means that you create a page for every page of the experiment, but for pages that might require some interface (e.g. some game or an involved elicitation method), you simply write what would happen there and what functionality you need to be able to implement it.


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
    @submit-button[]}
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

@section{Example Studies on Github}

You can find example studies on GitHub at @url{https://github.com/MarcKaufmann/congame/tree/master/congame-doc/conscript-examples} that illustrate a variety of features:

@itemlist[
  @item{how to include tables (tables.scrbl)}
  @item{the form input types available (all-inputs.scrbl)}
  @item{how to randomize participants into multiple treatments (assign-treatments.scrbl)}
  @item{how to use @code|{@refresh-every}| to wait for input from another player (wait-for-partner.scrbl)}
  @item{how to use html tags inside of @tt{(ev ...)} (tags-in-ev.scrbl)}
  @item{how to use images, and apply CSS styles to them with @code|{@style}|, @tt{#:style}, and @tt{#:class} (images.scrbl)}
]

@section{Randomizing participants into treatments}

While the example study on github provides an example for randomizing participants into treatments from scratch, you can use the function @racket[assigning-treatments] that you can use inside @tt{(ev ...)}. Thus the action that assigns treatments simply becomes:

@codeblock[#:keep-lang-line? #f]|{
#lang scribble/manual

@action[assigning-treatments]{
  @(ev
    (begin
      (define treatments
        (list
          'buyer 'buyer 'seller 'seller 'seller
          'observer 'observer 'observer 'observer 'observer))
      (assigning-treatments treatments)))
}
}|

This will randomize the order of the treatments and balance them across participants as they arrive. That means that in the case of 10 treatments (as in the example), every set of 10 participants is assigned to these 10 roles to ensure that we always have 2 buyers, 3 sellers, and 5 observers. The order in which they are assigned these roles is randomized.

This works by storing the treatment of the participant as a participant variable in @racket['role] and the set of treatments for the current group of participants as an instance variable in @racket['treatments]. Importantly, both of these variables are stored at the top level (@tt{(*root*)}), so they can be set and retrieved with @racket[get/global], @racket[put/global] for the @racket['role] and with @racket[get/instance/global] and @racket[put/instance/global] for the @racket['treatments] (you should not mess with the latter though). You can overrule these with @racket[#:treatments-key] and @racket[#:role-key]. So if you prefer storing the next set of treatments in @racket['my-treatments] and the treatment of the participant in @racket['treatment], then you need to change the action as follows:

@codeblock[#:keep-lang-line? #f]|{
#lang scribble/manual

@action[assigning-treatments]{
  @(ev
    (being
      (define treatments
        (list
          'buyer 'buyer 'seller 'seller 'seller
          'observer 'observer 'observer 'observer 'observer))
      (assigning-treatments treatments
                            #:treatments-key 'my-treatments
                            #:role-key       'treatment)))
}

@step[show-treatment]{
  @h1{Your treatment is @(ev (~a (get/global 'role)))}
}
}|

Notice the use of @racket[get/global] instead of @racket[get] to retrieve the role. This gets the value of @racket['role] stored at the top level study (@tt{(*root*)}), so that it is available from all substudies. Do not overuse the @tt{/global} versions of @racket[get] and @racket[put]. These set global variables, which is bad practice in general, as it leads to buggier code.

@section{Using buttons to jump to different steps}

You can use the @racket[#:to-step] keyword of buttons to override the default transition and instead jump to the step mentioned in the button as follows:

@codeblock[#:keep-lang-line? #f]|{
#lang scribble/manual

step[a]{
  @h1{Step b}

  @button[#:to-step c]{To step c}
  @button[#:to-step d]{To step d}
  @button{Default transition (to step b)}
}

step[b]{
  @h1{Step b}
  @button{Next}
}

step[c]{
  @h1{Step c}
  @button{Next}
}

step[d]{
  @h1{Step d}
  @button{Next}
}

step[final]{
  @h1{Final}
}

@study[
  skippy
  #:transitions
  [a --> b --> c --> d --> final]
  [final --> final]
]
}|

This study would usually go from step a through d to the final page. But if we click on the appropriate button on the first page, we will instead directly jump to step c or d, overriding the defaults.

@section{Basic Arithmetic}

You can use basic arithmetic in @tt{ev} based on @tech{Racket}'s arithmetic, which means that it is infix based:

@codeblock[#:keep-lang-line? #f]|{
#lang scribble/manual

@; Add two or more numbers
@(ev (+ 2 3))     @; 2 + 3
@(ev (+ 1 2 3 4)) @; 1 + 2 + 3 + 4

@; Subtract two numbers
@(ev (- 3 4)) @; 3 - 4

@; Multiply two or more numbers
@(ev (* 2 3))   @; 2 * 3
@(ev (* 2 -3))  @; 2 * (-3)

@; Divide
@(ev (/ 9 3))   @; 9 / 3 => 3
@(ev (/ 7 2))   @; 7 / 2 => the fraction 7/2!
@(ev (/ 7 2.0)) @; 7 / 2.0 => floating point 3.5

@; compute mean of 4 numbers
@(ev (/ (+ 1 2 3 4) 4.0))

@; Get the remainder or quotient
@(ev (remainder 7 2)) @; => 1
@(ev (quotient 7 2))  @; => 3
}|

If you want to include it in html markup, don't forget to transform the number to a string by using either @racket[number->string] or @racket[~a] (which turns every input into a string, so also works for symbols (@tt{'age}) or lists or other items):

@codeblock[#:keep-lang-line? #f]|{
#lang scribble/manual

@h1{Twice your age is @(ev (number->string (* 2 (get 'age))))}

Your remaining life expectancy is @(ev (~a (- 80 (get 'age)))).
}|

}
