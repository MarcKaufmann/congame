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
        (div maybe-class expr ...+)
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
  [maybe-class (code:line)
               (code:line #:class string)]

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

This code defines a @tech{step} named 'start', and a @tech{study} named 'tutorial', which starts with a single step and ends with a single step. You can upload the code to your congame server as follows, where you have to provide the name of the study that should be run as the @emph{DSL ID}. To do so, follow these steps:

@itemlist[
  @item{Log in to your researcher account}
  @item{Go the @emph{Admin} page}
  @item{Click on @emph{New Study}}
  @item{Provide a name such as "Tutorial <your name>"}
  @item{Pick the type @emph{DSL}}
  @item{As @emph{DSL ID}, write the name of the study, @emph{tutorial1}}
  @item{As @emph{DSL source}, browse for your @filepath{tutorial.scrbl} file}
  @item{Click the @emph{Create} button}
]

If everything went well, you will see a page with instances of your tutorial study, which should be none. Create a @emph{New Instance}. You can give it whatever name you want, and don't need to add any other field. Simply click @emph{create}.

Now when you go back to the @emph{Dashboard}, you should see your study with the name you gave it as an instance. You can now enroll in that study yourself (for testing) and should see the first page. Congratulations!

@subsection{Multi-Step Studies}

Having a study that consists of a single page isn't very interesting. Let us add two more steps, one intermediate one, where we will ask for the name and age, and a final one to thank the person by name.

Update @filepath{tutorial.scrbl} to contain the following:

@codeblock|{
@step[description]{
  @h1{The study}

  Welcome to our study. In this study, we will ask you about

  @ul{
    @li{your age}
    @li{your height}}

  @button{Start Survey}
}

@step[age-height-survey]{
  @h1{Survey}

  @form{
    @input-text[name]{What is your name?}
    @input-number[age #:min 0]{What is your age (in years)?}
    @;@input-number[height #:min 0.0]{How tall are you (in meters)?}
    @submit-button[]}
}

@step[thank-you]{
  @h1{Thank you @get['name]}

  Thank you for participating in our survey!
}

@study[
  tutorial2
  #:transitions
  [description --> age-height-survey --> thank-you]
  [thank-you --> thank-you]
]
}|

Then, on your congame server, go to the admin page, and follow these steps to update the study code and the study run for tutorial:

@itemize{
  @item{Click on your existing study instance}
  @item{Click on @emph{Edit DSL}}
  @item{Change the DSL ID to @emph{tutorial2}, since we call the new study @emph{tutorial2}}
  @item{Pick the updated version of @filepath{tutorial.scrbl}}
  @item{Click @emph{Update}}}

Try to resume the study. If you did the the @emph{tutorial1} study, you should now see an error. This is because when you did @emph{tutorial1}, you progressed to the step with the ID @emph{start}. Since such a step does not exist in @emph{tutorial2}, you get an error.

To fix this, you have to clear the progress of your user for this study instance. Go to the admin page of the tutorial instance (@emph{Admin}, click on the name of your tutorial instance). Towards the bottom, you will see a list of instances under @strong{Instance Name}. Click on your instance. At the bottom of the next page is the list of participants who have enrolled in this study. Click on your ID (which you can identify by the email if you enrolled from your congame server). Then click on @emph{Clear participant progress}. (Note: it may look like there was no progress, if the table is empty. That's because the progress shows only additional data that you store explicitly, not implicit progress such as the current step you are on.)

Now you can bo back to the dashboard and go through the study. Congratulations, this is your first survey in @tech{conscript}!
