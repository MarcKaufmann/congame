#lang scribble/manual

@title{DSL (Conscript)}

@section{Grammar}

@racketgrammar*[
  #:literals (
    @; stmt
    action define import step study template template-ungrouped

    @; expr
    a br button div em escape form get h1 h2 h3 img li p put quote span strong ol ul yield

    @; form-expr
    input-date input-text input-number textarea submit-button

    @; transition
    --> cond
  )

  [stmt (action id action-expr ...+)
        (define id expr)
        (import module-id binding-id ...+)
        (step id expr ...+)
        (step id #:pre pre-action-id expr ...+)
        (study id #:transitions transition ...+)
        (template id stmt ...+)
        (template-ungrouped id stmt ...+)]

  [expr (a text expr)
        (br)
        (button maybe-action text ...+)
        call-expr
        (div maybe-option ... expr ...+)
        (em expr ...+)
        (escape s-expr)
        (form maybe-action form-expr ...+)
        get-expr
        (h1 expr ...+)
        (h2 expr ...+)
        (h3 expr ...+)
        (img string)
        (span maybe-class expr ...+)
        (strong expr ...+)
        (ol (li expr ...+) ...+)
        (ul (li expr ...+) ...+)
        (template id expr ...+)
        (yield)

        imported-id
        number
        string
        keyword
        (quote datum)]

  [call-expr (imported-id expr ...)]

  [get-expr (get expr)
            (get expr expr)
            (get #:instance expr)
            (get #:instance expr expr)]

  [put-expr (put id-expr expr)
            (put #:instance id-expr expr)]

  [form-expr (input-date id expr ...+)
             (input-text id expr ...+)
             (input-number id expr ...+)
             (input-number id #:min number expr ...+)
             (input-number id #:max number expr ...+)
             (input-number id #:min number #:max number expr ...+)
             (submit-button)
             (textarea id expr ...+)
             expr]

  [action-expr call-expr
               get-expr
               put-expr
               (escape s-expr)]

  [maybe-action (code:line)
                (code:line #:action action-id)]
  [maybe-option (code:line)
                (code:line #:class string)
                (code:line #:style string)]

  [transition [transition-entry ...+]]
  [transition-entry -->
                    target-id
                    cond-expr
                    [lambda action-expr ...+ end]
                    [lambda action-expr ...+ target-id]
                    [lambda action-expr ...+ cond-expr]]

  [target-id step-id
             study-id
             imported-id]
]

@section{Goals and Philosophy}

@deftech{conscript} is aimed at creating simple studies fast and without fuss. It provides a more user-friendly syntax than @tech{congame}, which requires taking a deep dive and using full @tech{Racket}. @tech{conscript} makes it especially convenient to mix the text seen by users with small amounts of logic. It is not well-suited to complicated logic, or studies that manipulate complicated data objects.

One nice feature of @tech{conscript} is the ability to compile it to full @tech{congame} studies, which can then be reused to build larger @tech{congame} studies --- although this feature should not be abused, since the code generated is not as readable as handcrafted code. Nothing beats artisanal code from your local code bakery.

For this reason, @tech{conscript} will never be feature-complete and expose the same power as @tech{congame}: doing so would necessarily lead to the same complexity. When @tech{conscript} proves too limited for your needs, it is time to take the red pill and take the deep dive into @tech{congame}.

@section{Tutorial}

You will create a simple study that gets some information from people (their age, their height), elicits a probability about some belief from them, and finally asks them to choose their favorite color between orange and blue, which will determine how the final page looks.

@subsection{Create researcher account}

To follow along with this tutorial, you need to create an account with the 'researcher' or 'admin' roles on a @tech{congame} server. To do so, create an account on your @tech{congame} server, then in your database, set the researcher role in the database. In postgres:

@codeblock|{
  UPDATE users SET roles = '{researcher}' WHERE ID = <id-of-your-user>;
}|

If you have a researcher or admin role, you will see navigation for 'Admin' and 'Jobs'.

@subsection{The first study}

To start, note that @tech{conscript} is based on @tech{scribble} syntax: this means that all operators start with an @"@", followed by the name of the operator, followed either by square brackets ("[]") or curly brackets ("{}") that contain additional content. To get started, let us create a @tech{conscript} study that displays a single page with some text. To do so, store the following text in @filepath{tutorial.scrbl}:

@codeblock|{
@step[start]{
    @h1{The Beginning is the End}}

@study[
  tutorial1
  #:transitions
  [start --> start]]
}|

This code defines a @tech{step} named 'start', and a @tech{study} named 'tutorial', which starts with a single step and ends with a single step. You can upload the code to your congame server as follows, where you have to provide the name of the study that should be run as the @emph{Study ID}. To do so, follow these steps:

@itemlist[
  @item{Log in to your researcher account}
  @item{Go the @emph{Admin} page}
  @item{Click on @emph{New Study}}
  @item{Provide a @emph{Name} such as "Tutorial <your name>"}
  @item{As @emph{Type}, choose @emph{DSL}}
  @item{As @emph{Study ID}, take the ID of the study from your source code, @emph{tutorial1} if you used the code above}
  @item{As @emph{DSL source}, browse for your @filepath{tutorial.scrbl} file}
  @item{Click the @emph{Create} button}
]

If everything went well, you will see a page with instances of your tutorial study, which should be none. Create a @emph{New Instance}. You can give it whatever name you want, and don't need to add any other field. Simply click @emph{create}.

Now when you go back to the @emph{Dashboard}, you should see your study with the name you gave it as an instance. You can now enroll in that study yourself (for testing) and should see the first page. Congratulations!

@subsection{Multi-Step Studies}

Having a study that consists of a single page isn't very interesting. Let us add two more steps, one intermediate one, where we will ask for the name and age, and a final one to thank the person by name.

There are several new parts in this multi-step study:

@itemize[
  @item{How to put multiple steps in sequence}
  @item{How to write a form}
  @item{How to get and use data that is stored}
]

Suppose that we have three steps, creatively named @racket[step1], @racket[step2], and @racket[step3]. To create a study with these steps in order, with @racket[step3] the final one, we write:

@codeblock|{
  @study[
    three-steps
    #:transitions
    [step1 --> step2 --> step3 --> step3]]
}|

The first argument of @racket[study] is the ID of the study. It must be followed by the keyword @racket[#:transitions], followed by one or more transition entries enclosed in square brackets. The simplest type of transition entry is a sequence of step IDs connected by @racket[-->]'s, such as @racket[step1 --> step2]. The arrow indicates that after completing @racket[step1], we transition to @racket[step2].

Note that every step has to explicitly define a transition, even if it is meant to be the final step. Thus to make @racket[step3] the final step, we have to write that it transitions to itself: @racket[step3 --> step3].

The primary goal of studies is to collect data from participants, and @racket[form]s are the main way of getting input from participants. The simplest forms will contain one or more @racket[input] fields, and a @racket[submit-button]. The input field for free-form text answers (e.g. when asking for a name) is @racket[input-text]. In order to be able to store the answer provided by the user when the form is submitted, we need to provide an ID for the data:

@codeblock|{
  @input-text[first-name]{What is your name?}
}|

This input field ensures that the answer the user provided is a string and stores it as such with the ID @racket[first-name]. A form to get the first name and the age of a person will thus look as follows:

@codeblock|{
  @form{
    @input-text[first-name]{What is your first name?}
    @input-number[age]{What is your age (in years)?}
    @submit-button[]}
}|

It is important not to confuse square ("[]") and curly ("{}") brackets. The main difference is that curly brackets interpret their content as a string by default (although they correctly expand other @"@" forms, such as @code|{@get}| that we'll see later). Therefore much of what users see will be in curly brackets. Square brackets on the other hand interpret their content as data: therefore identifiers of studies and steps, numbers, or keys to extract data should be enclosed in square brackets. Square brackets are optional, but when used have to come before curly brackets (which are also optional).

Once a study stores data, we can get it by using @code|{@get}|. Suppose the user provided their first name, then we can get the value with @code|{@get['first-name]}| -- note the single quote (') in front of first-name, which identifies it as a @emph{symbol} rather than as the object named @racket[first-name].

Putting all of this together, we can create our first multi-step study by updating @filepath{tutorial.scrbl} as follows:

@codeblock|{
@step[description]{
  @h1{The study}

  Welcome to our study. In this study, we will ask for

  @ul{
    @li{your first name}
    @li{your age}}

  @button{Start Survey}
}

@step[age-height-survey]{
  @h1{Survey}

  @form{
    @input-text[first-name]{What is your first name?}
    @input-number[age]{What is your age (in years)?}
    @submit-button[]}
}

@step[thank-you]{
  @h1{Thank you @get['first-name]}

  Thank you for participating in our survey @get['first-name]!
}

@study[
  tutorial2
  #:transitions
  [description --> age-height-survey --> thank-you --> thank-you]]
}|

We have to update the code on the congame server to reflect these changes. Go to the admin page, and follow these steps to update the study code and the study run for tutorial:

@itemize[
  @item{Click on your existing study instance}
  @item{Click on @emph{Edit DSL}}
  @item{Change the DSL ID to @emph{tutorial2}, since we call the new study @emph{tutorial2}}
  @item{Pick the updated version of @filepath{tutorial.scrbl}}
  @item{Click @emph{Update}}]

Try to resume the study. If you did the the @emph{tutorial1} study, you should now see an error. This is because when you did @emph{tutorial1}, you progressed to the step with the ID @emph{start}. Since such a step does not exist in @emph{tutorial2}, you get an error.

To fix this, you have to clear the progress of your user for this study instance. Go to the admin page of the tutorial instance (@emph{Admin}, click on the name of your tutorial instance). Towards the bottom, you will see a list of instances under @bold{Instance Name}. Click on your instance. At the bottom of the next page is the list of participants who have enrolled in this study. Click on your ID (which you can identify by the email if you enrolled from your congame server). Then click on @emph{Clear participant progress}. (Note: it may look like there was no progress, if the table is empty. That's because the progress shows only additional data that you store explicitly, not implicit progress such as the current step you are on.)

Now you can bo back to the dashboard and go through the study. Congratulations, this is your first survey in @tech{conscript}!

@subsection{Using the standard library}

Basic conscript is purposefully underpowered and comes with a small number of built-in features. To provide more functionality without resorting to @tech{Racket}, we are collecting useful functions in the standard library that makes it relatively easy to use them with conscript.

To illustrate this, let us add a display of the person's age to the previous study. It may seem straightforward, and you might try to do change the code of the final @racket[thank-you] step as follows:

@codeblock|{
@step[thank-you]{
  @h1{Thank you, @get['first-name]}

  Thank you for participating in our survey, @get['first-name]! You are the most awesome @get['age]-year old!
}
}|

You might expect this to display the age on the page. Instead, you are likely to find that the final page does not display the age at all, and you see only "You are the most awesome &-year old!" instead. What is going on?

What is going on is that when we are storing a number, we are storing a number and not a string! So when we use @code|{@get['age]}| to display the age, we are providing the age as a number and not as a string, and since numbers are encoded differently, this leads to the strange display you get. To fix this, all we need to do is to convert numbers to strings before displaying them. Fortunately, @racket[number->string] is provided in @racket[stdlib]. To use a function from @racket[stdlib], you have to

@itemlist[
  @item{import it with @code|{@import[stdlib name-of-the-function]}|}
  @item{use it with @code|{@call[name-of-the-function function-arguments]}|}
  #:style 'ordered
]

In our case, this would look as follows:

@codeblock|{
@; import functions you need, ideally at the top of the file
@import[stdlib number->string]

@; Now you can use `number->string` in a @call
@step[thank-you]{
  @h1{Thank you, @get['first-name]}

  Thank you for participating in our survey, @get['first-name]! You are the most awesome @call[number->string @get['age]]-year old!
}
}|

@; UNCOMMENT IF STUDENTS HIT THIS PROBLEM
@; Notice that calling a function such as @racket[number->string] via the @code|{@call}| operator is different from using, say, the @code|{@h1}| operator. Specifically, we list the function name followed by a space followed by a list of arguments needed by the function, here the number that we want to convert to a string. So you cannot write @code|{@number->string[@get['age]]}|. The reason is that the former is in fact a function (or macro or operator) defined in some @tech{Racket} package, while @code|{@h1}| is a core operator in Conscript.

While cumbersome to get used to at first, it is good to get in the habit of realizing what type of object you are dealing with when programming, even in concscript.

@subsection{Studies with Logic}

We often want to respond to participants, whether it is to display different messages as we progress, or based on their answers. We will now create a few studies using some of the programming features that conscript provides.

First, let us count down from 10 to 0 and display the pages to the
user. We could of course define a separate step for each number,
calling them @racket[step10] down to @racket[step0] and then string
them together as @tt{step10 --> ... --> step0}, but that is
tedious. Instead, for every user, let us store the value of
@racket[counter] and every time the user progresses, we decrease the
value of @racket[counter] and display it on the screen. To store a
value for a user, we use @code[#:lang "scribble/manual"]|{@put[id value]}|.

The most important building block for this is the @code[#:lang "scribble/manual"]|{@action}| operator. Whenever you want to change or update some value or variable in conscript, you have to define a named @tt{action} which will be evaluated when called. In the case of the countdown, we need to do two things. First, we need an action that initializes the user's counter to 10; second, we need an action that decreases the counter by 1 when called:

@codeblock[#:keep-lang-line? #f]|{
#lang scribble/manual
@import[stdlib sub1]

@action[initialize-counter]{
  @put['counter 10]
}

@action[decrement-counter]{
  @put['counter @call[sub1 @get['counter]]]
}
}|

Here we also import @racket[sub1], which subtracts 1 from its argument. Later we'll see another way to do basic arithmetic.

Next, we need to call the @tt{action}s in the right places. There are two places where you can use actions: before a step is displayed by using @code|{@step[step-name #:pre action-id]}|; or after a button click (and before the next step is executed) using @code|{@button[#:action-id action-id]{Next}}|. We will the button approach here:

@codeblock[#:keep-lang-line? #f]|{
#lang scribble/manual

@import[stdlib number->string]

@; Here goes the above code defining the actions
@; ...

@step[description]{
  @h1{The countdown is about to begin}

  @button[#:action initialize-counter]{Start Countdown}
}

@step[show-counter]{
  @h1{@call[number->string @get['counter]]}

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

@subsection{Conditions}

In order to stop once the counter hits 0, we need to change the transitions. Specifically, we want to transition to the @tt{launch} step when the counter is 0, and otherwise keep displaying the @tt{show-counter} step. To do so, we use @code[#:lang "scribble/manual"]|{@cond}|:

@codeblock[#:keep-lang-line? #f]|{
#lang scribble/manual

@step[launch]{
  @h1{Study launched!}
}

@study[
  countdown
  #:transitions
  [description --> show-counter
               --> @cond[[@=[@get['counter] 0]
                          launch]
                         [@else
                          show-counter]]]
  [launch --> launch]
]
}|

@subsection{Intermezzo: Some exercises}

Use the keyword arguments @tt{#:style} @tt{#:class} with the @code|{@div}| to add some CSS styling to your page. (You can only use these with @tt{div} operators, not for others.)

@subsection{Studies involving multiple Participants}

Coming soon...

@subsection{Basic Arithmetic (features in progress)}

One important thing to note about conscript is that it currently does not yet have many features that you would expect of a programming language, including basic arithmetic and string operations. The way to do basic arithmetic is to use @code|{@escape[...]}|, for example:

@codeblock|{
  @; Adding two numbers
  @escape[(+ 1 2)]
  @; Multiplying two numbers
  @escape[(* 3 4)]
  @; Subtracting two numbers
  @escape[(* 3 4)]
  @; Dividing two numbers
  @escape[(/ 4.5 2)]
}|

In addition, since Racket uses prefix notation for arithmetic --- we write @racket{(+ 1 2)} instead of @racket{1 + 2}, and the parentheses are necessary --- the same is true for conscript. Let us know how painful this is and we can see if it is worth spending time fixing this (but first you'll have to try to use it, then once you can do it, we'll listen).
