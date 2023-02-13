#lang scribble/manual

@; TODO
@; Be consistent in the use of 'I', 'We', 'you'

@title{Conscript Tutorial}

You will create a simple study that gets some information from people (their age, their height), elicits a probability about some belief from them, and finally asks them to choose their favorite color between orange and blue, which will determine how the final page looks.

@section{Create researcher account}

To follow along with this tutorial, you need to create an account with the 'researcher' or 'admin' roles on a @tech{congame} server. To do so, create an account on your @tech{congame} server, then in your database, set the researcher role in the database. In postgres:

@codeblock|{
  UPDATE users SET roles = '{researcher}' WHERE ID = <id-of-your-user>;
}|

If you have a researcher or admin role, you will see navigation for 'Admin' and 'Jobs'.

@section{The first study}

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

@section{Multi-Step Studies}

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
  @h1{Thank you @(ev (get 'first-name))}

  Thank you for participating in our survey @(ev (get 'first-name))!
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

@section{Using standard functions}

Basic conscript is purposefully underpowered and comes with a small number of built-in features. Many @tech{Racket} functions are provided by default, and we will add more as they become useful.

To illustrate this, let us add a display of the person's age to the previous study. It may seem straightforward, and you might try to do change the code of the final @racket[thank-you] step as follows:

@codeblock|{
@step[thank-you]{
  @h1{Thank you, @(ev (get 'first-name))}

  Thank you for participating in our survey, @(ev (get 'first-name))! You are the most awesome @(ev (get 'age))-year old!
}
}|

You might expect this to display the age on the page. Instead, you are likely to find that the final page does not display the age at all, and you see only "You are the most awesome &-year old!" (or some other strange character in place of the &) instead. What is going on?

What is going on is that when we are storing a number, we are storing a number and not a string! So when we use @code|{@(ev (get 'age))}| to display the age, we are providing the age as a number and not as a string, and since numbers are encoded differently, this leads to the strange display you get. To fix this, all we need to do is to convert numbers to strings before displaying them. Fortunately, @racket[number->string] is provided by default. To use it, just call it inside an @code|{@(ev ...)}| call:

@codeblock|{
@step[thank-you]{
  @h1{Thank you, @(ev (get 'first-name))}

  Thank you for participating in our survey, @(ev (get 'first-name))! You are the most awesome @(ev (number->string (get 'age)))-year old!
}
}|

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
}|

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



@section{Studies involving multiple Participants}

Coming soon...

@section{Basic Arithmetic (features in progress)}

Coming soon...
