#lang scribble/manual

@(require (for-label conscript/base
                     conscript/form0
                     conscript/survey-tools
                     racket/unit
                     racket/contract
                     racket/list)
          racket/runtime-path
          "doc-util.rkt")

@(define-runtime-path fig-plugs "units-fig-plugs.svg")
@(define-runtime-path fig-problem "units-fig-problem.svg")
@(define-runtime-path fig-flow "units-fig-flow.svg")

@title[#:style 'quiet #:tag "units-tutorial"]{Tutorial: Advanced Study Composition with Units}

In @secref["parent-child-tutorial"] you learned how to embed one study inside another, and how to
share a variable between them. That approach works well when the parent and child are written
together and agree on the names of the variables they share.

This tutorial goes one step further. It shows how to write studies as interchangeable parts, so that
a parent study can plug in any one of them without knowing what’s inside, hand a value in, and get
a value back out. You’ll learn how to:

@itemlist[

@item{Describe the “plugs” a study needs and provides, using a signature}

@item{Write a study as a unit that fits those plugs}

@item{Compose several units into one study, in a fixed order}

@item{Compose units so that each participant gets one of them, chosen at random}

@item{Write the surrounding study once, as a function that composes any number of units}

]

This tutorial assumes you’re familiar with the basic concepts covered in @secref["overview"] and
@secref["intro"], and that you’ve read @secref["parent-child-tutorial"].

@;===============================================

@section[#:tag "unitstut-problem"]{The problem}

Suppose your lab has written studies for two small tasks. In the counting task, participants solve
addition problems and report how many they solved, earning ten cents per problem. In the lottery
task, participants choose between a sure payment and a coin flip. Each task’s study is complete on
its own, and each one pays the participant something.

Now you want a single study that shows a consent page, has the participant do a task, and then shows
them what they’ll be paid. Inside that larger study, the parent, each task’s study runs as a
@emph{substudy}: a study that runs as one step of the parent. There are two versions you’d like
to be able to build. In the first, every participant does both tasks, one after the other. In the
second, each participant is randomly given just one of the tasks.

@centered{@image[fig-problem]}

And once both work, you’d like the parent’s own pages, the consent page, the fee, and the payment
page, to be written once and used again with whatever substudies the lab writes next, without
editing them each time.

Two pieces of information have to cross the line between the parent and the substudy inside it. The
participation fee is decided by the parent, but the substudy needs to know it, because its
instructions mention it. And the payment is worked out by the substudy, but the parent needs it,
because the parent owns the payment page.

You could handle this with the @racket[defvar*] technique from the previous tutorial: agree on a
variable name for the fee and another for the payment, and have both sides use them. That is fine
for one parent and one child. It gets awkward when you want @emph{any} substudy to fit into the same
place in the parent, when a substudy needs to hand back a calculation rather than a number, or when
nothing checks that a substudy actually provides what the parent expects until a participant hits
the missing piece. Racket’s units solve exactly this problem.

@inline-note{In this tutorial, a @emph{task} is always something a participant does, such as solving
the counting problems. The Conscript study that contains a task, and that the parent plugs in, is
a substudy.}

@;===============================================

@section[#:tag "unitstut-what-is-a-unit"]{What a unit is}

Think of a signature as the shape of a plug, and a unit as a study packed in a box with plugs on
it. One plug is for what goes in, and the others are for what comes out.

@centered{@image[fig-plugs]}

A signature is just a list of names. This tutorial uses three of them. The
@racketidfont{fee^} signature names one function, @racketidfont{get-fee}, which returns the
participation fee. The @racketidfont{study^} signature names @racketidfont{study}, the study
itself. The @racketidfont{payment^} signature names @racketidfont{compute-payment}, a function
that returns what the substudy pays the participant. By convention, signature names end in a caret
(@litchar{^}) and unit names end in an at-sign (@litchar{@"@"}).

A unit is a block of code that @emph{imports} some signatures and @emph{exports} others. Importing
@racketidfont{fee^} means “somebody outside will give me a function called @racketidfont{get-fee}”.
Exporting @racketidfont{study^} and @racketidfont{payment^} means “I promise to define
@racketidfont{study} and @racketidfont{compute-payment}, and the outside can have them”.

A unit is not a study. It is a recipe for making a study, and the recipe can’t be followed until
someone supplies the imports. You do that with @racket[define-values/invoke-unit], which takes the
unit and the imports, runs the unit’s code, and defines its exports for you. Keep this last point
in mind: invoking a unit @emph{defines} names in whatever scope you’re standing in when you do it.
Nearly every mistake people make with units comes from forgetting that.

@margin-note{Units are a general Racket feature, not something specific to Congame. For the full
story, see @secref["units" #:doc '(lib "scribblings/guide/guide.scrbl")] in the Racket Guide.}

@;===============================================

@section[#:tag "unitstut-signatures"]{The signatures}

The signatures go in a file of their own, apart from any study. Create @filepath{signatures.rkt}:

@filebox["signatures.rkt"]{
@codeblock|{
#lang conscript

(provide fee^
         study^
         payment^)

(define-signature fee^
  [get-fee])

(define-signature study^
  [study])

(define-signature payment^
  [compute-payment])
}|}

There are two reasons to keep the signatures apart from the studies. The first is the same reason you’re composing studies
at all: you are factoring out the parts that every piece has in common, and the signatures are the
most common part of all. Every substudy and every parent study needs them.

The second reason is a rule of Racket’s. Two signatures are only “the same” if they come from the
same definition. If @filepath{counting-study.rkt} defined its own @racketidfont{fee^} and the parent
study defined another one, they would be two different signatures that happen to share a name, and
the parent could never plug the substudy in. Defining each signature once, outside the files that
define the studies, and @racket[require]-ing that definition everywhere is what makes the plugs fit.

@margin-note{This tutorial keeps its three signatures together in one file. That part is a matter of
taste. Some authors prefer one file for each signature, and Racket has a small language for writing
a file that way: see @racketmodname[racket/signature].}

@;===============================================

@section[#:tag "unitstut-writing-a-substudy"]{Writing a substudy as a unit}

Here is the substudy for the counting task, written as a unit. Save it as @filepath{counting-study.rkt}:

@filebox["counting-study.rkt"]{
@codeblock|{
#lang conscript/with-require

(require conscript/form0
         conscript/survey-tools
         "signatures.rkt")

(provide counting-study@)

(define counting-study@
  (unit
    (import fee^)
    (export study^ payment^)

    (with-namespace my-lab.counting-study
      (defvar* problems-solved))

    (define (compute-payment)
      (* 0.10 problems-solved))

    (defstep (instructions)
      @md{# Counting task

          On the next page you will see ten addition problems. Solve as many as you can
          in your head, then tell us how many you solved.

          You will earn $0.10 for each problem you solve, in addition to your
          @(~$ (get-fee)) participation fee.

          @button{Start}})

    (defstep (problems)
      (define-values (report-form on-submit)
        (form+submit
         [problems-solved (ensure binding/number (required) (range/inclusive 0 10))]))
      (define (render rw)
        @md*{@rw["problems-solved" @input-number{How many did you solve?}]
             @|submit-button|})
      @md{# Problems

          17 + 26, 38 + 45, 52 + 19, 64 + 27, 71 + 39,
          83 + 48, 29 + 57, 46 + 35, 58 + 67, 92 + 13

          @form[report-form on-submit render]})

    (defstudy study
      [instructions --> problems --> ,(lambda () done)])))
}|}

If you have written a Conscript study before, most of this will look familiar. The steps and the
@racket[defstudy] that strings them together are exactly what you would write in a standalone
study. What’s new is the wrapper around them, the name of the study, and a few details inside.

The file uses @code{#lang conscript/with-require} rather than plain @code{#lang conscript}, because
it needs to @racket[require] the sibling file @filepath{signatures.rkt}. (See
@secref["conscript-with-require"] for what this implies about who can upload the study.)

The whole body of the substudy sits inside @racket[(unit (import fee^) (export study^ payment^) ...)],
and the file @racket[provide]s the unit, @racketidfont{counting-study@"@"}, rather than a study.
Everything you would normally write at the top level of the file goes inside the unit instead, and
@racket[defvar], @racket[defstep], and @racket[defstudy] all work there just as they do outside.

The substudy’s flow, @racketidfont{instructions} then @racketidfont{problems}, is a study named
@racketidfont{study}. That name is fixed: it is the name the @racketidfont{study^} signature
promises, and the parent will refer to the substudy by it. Every substudy exports its study under
this same name. Telling the substudies apart is the parent’s job, not the substudy’s, and
@secref["unitstut-own-name"] shows how the parent does it.

The fee is never written into this file. Wherever the text needs it, the code calls
@racket[(get-fee)], and it does so inside a step, at the moment the page is shown. The function
@racketidfont{get-fee} is the import: this file doesn’t define it, and it doesn’t exist until the
parent supplies it.

The study’s last transition is @racket[,(lambda () done)]. When the participant leaves the
@racketidfont{problems} page, the study is finished and hands control back to the parent, just as
with any child study. The substudy does not have a thank-you page of its own,
because the parent will provide one.

Finally, look at @racketidfont{problems-solved}. It is defined with @racket[defvar*] inside a
@racket[with-namespace] block, not with plain @racket[defvar], and the reason deserves a careful
explanation.

@subsection[#:tag "unitstut-defvar-star"]{Why @racket[compute-payment] needs a @racket[defvar*]}

Recall from @secref["pctut-sharing-data"] that a @racket[defvar] variable is stored under the
study that sets it. All of the counting substudy’s own pages run inside the same study, so among
themselves they could share a plain @racket[defvar] without trouble.

But @racketidfont{compute-payment} is exported. The parent study calls it later, from a page of its
own, which runs @emph{outside} the substudy. If @racketidfont{problems-solved} were a plain
@racket[defvar], the parent’s call would look for it in the parent’s own storage, find nothing,
and get @racket[undefined]. Making it a @racket[defvar*] stores it in one shared place that both
the substudy’s pages and the parent can reach. The @racket[with-namespace] block gives that shared
place a name that no other substudy will accidentally use; naming the namespace after the substudy, as
here, is a good habit.

@inline-note[#:type 'tip]{The rule is short: inside a unit, any @tech{study variable} that an
exported function reads must be a @racket[defvar*] inside @racket[with-namespace]. Variables that 
only the unit’s own pages use can stay plain @racket[defvar].}

The lottery substudy shows both kinds side by side. Save it as @filepath{lottery-study.rkt}:

@filebox["lottery-study.rkt"]{
@codeblock|{
#lang conscript/with-require

(require conscript/form0
         conscript/survey-tools
         "signatures.rkt")

(provide lottery-study@)

(define lottery-study@
  (unit
    (import fee^)
    (export study^ payment^)

    (with-namespace my-lab.lottery-study
      (defvar* payoff))

    (defvar choice)

    (define (compute-payment)
      payoff)

    (defstep (choose)
      (define-values (choice-form on-submit)
        (form+submit
         [choice (ensure binding/text (required))]))
      (define (render rw)
        @md*{@rw["choice"
                 (radios '(("safe"  . "Take $1.00 for sure")
                           ("risky" . "Flip a coin: heads pays $3.00, tails pays nothing"))
                         "Which do you prefer?")]
             @|submit-button|})
      @md{# Lottery choice

          Your @(~$ (get-fee)) participation fee is yours to keep whatever you choose here.

          @form[choice-form on-submit render]})

    (defstep (flip)
      (set! payoff
            (if (equal? choice "safe")
                1.00
                (if (zero? (random 2)) 3.00 0.00)))
      (skip))

    (defstep (result)
      @md{# Result

          You chose the @(if (equal? choice "safe") "safe" "risky") option and earned
          @(~$ payoff).

          @button{Continue}})

    (defstudy study
      [choose --> flip --> result --> ,(lambda () done)])))
}|}

Here @racketidfont{choice} is a plain @racket[defvar], because only the substudy’s own pages read it.
The coin is flipped in a step of its own, @racketidfont{flip}, which stores the result and then
calls @racket[skip] so that the participant never sees a page for it. (Flipping the coin inside the
@racketidfont{result} page would flip it again every time that page was reloaded.) The result goes
into @racketidfont{payoff}, which is a @racket[defvar*] because @racketidfont{compute-payment}
reads it.

@subsection[#:tag "unitstut-converting"]{Converting a study you’ve already written}

If you have an existing standalone study and want to turn it into a unit, the changes are the ones
you have just seen. The steps below are written for any set of signatures, not just the three in
this tutorial, and it helps to make them in order.

@itemlist[#:style 'ordered

@item{Change the first line to @code{#lang conscript/with-require} and @racket[require] the file
that defines the signatures you are going to use. In this tutorial that file is
@filepath{signatures.rkt}.}

@item{Wrap everything below the @racket[require] in @racket[(define my-study@ (unit (import ...)
(export ...) ...))]. Under @racket[import], list the signatures for the things the study needs
from outside; under @racket[export], list the signatures for the things it provides. Then change
the @racket[provide] so that the file provides the unit instead of the study. The substudies in this
tutorial import @racketidfont{fee^} and export @racketidfont{study^} and
@racketidfont{payment^}.}

@item{For each name in an imported signature, find the places where the study used to supply that
value itself, and replace them with the imported name. Call imported names from inside a step, at
the moment the page is shown, never at the top level of the unit. In the counting substudy, the fee
that used to be typed into the text became a call to @racket[(get-fee)].}

@item{For each name in an exported signature, make sure the unit defines something with exactly
that name. If one of the exports is the study itself, rename its @racket[defstudy] to the name the
signature promises, give its last page a button, make its last transition
@racket[,(lambda () done)], and remove any thank-you page that the parent will provide instead.
Here the study is renamed @racketidfont{study}, because that is the name @racketidfont{study^}
promises.}

@item{Any study variable that an exported function reads must become a @racket[defvar*] inside a
@racket[with-namespace] block named after the study. Here @racketidfont{compute-payment} reads
@racketidfont{problems-solved}, so @racketidfont{problems-solved} is a @racket[defvar*].}

]

@;===============================================

@section[#:tag "unitstut-shared-pages"]{The shared pages}

The consent page and the closing pages are the same no matter which task a participant does, so
they live in a file of their own. Save this as @filepath{shared-pages.rkt}:

@filebox["shared-pages.rkt"]{
@codeblock|{
#lang conscript

(provide consent
         no-consent
         thank-you
         consent-given?
         participation-fee)

(require conscript/form0
         conscript/survey-tools)

(with-namespace my-lab.shared-pages
  (defvar* participation-fee))

(defvar consent-given?)

(defstep (consent)
  (define-values (consent-form on-submit)
    (form+submit
     [consent-given? (ensure binding/text (required))]))
  (define (render rw)
    @md*{@rw["consent-given?"
             (radios '(("yes" . "Yes, I agree to take part")
                       ("no"  . "No, I do not want to take part"))
                     "Do you agree to take part in this study?")]
         @|submit-button|})
  @md{# Welcome

      You will receive @(~$ participation-fee) for taking part, plus whatever you earn
      in the task.

      @form[consent-form on-submit render]})

(defstep (no-consent)
  @md{# Thank you for your time

      You have chosen not to take part. You may close this window.})

(defstep (thank-you)
  @md{# Thank you!

      Your responses have been recorded. You may close this window.})
}|}

This file owns the participation fee. It is a @racket[defvar*] for the same reason as before, seen
from the other side: the parent study sets it, and the substudies read it (through
@racketidfont{get-fee}) from inside their own studies. A plain @racket[defvar] set by the parent
would be invisible to them.

@;===============================================

@section[#:tag "unitstut-fixed"]{Part 1: Composing in a fixed order}

Now for the first parent study, where every participant does the counting task and then the
lottery task. Save it as @filepath{fixed-study.rkt}:

@filebox["fixed-study.rkt"]{
@codeblock|{
#lang conscript/with-require

(require conscript/survey-tools
         "counting-study.rkt"
         "lottery-study.rkt"
         "shared-pages.rkt"
         "signatures.rkt")

(provide fixed-study)

(define-values (counting-study compute-counting-payment)
  (let ()
    (define (get-fee) participation-fee)
    (define-values/invoke-unit counting-study@
      (import fee^)
      (export study^ payment^))
    (values study compute-payment)))

(define-values (lottery-study compute-lottery-payment)
  (let ()
    (define (get-fee) participation-fee)
    (define-values/invoke-unit lottery-study@
      (import fee^)
      (export study^ payment^))
    (values study compute-payment)))

(defstep (set-fee)
  (set! participation-fee 2.00)
  (skip))

(defstep (payment)
  @md{# Your payment

      Participation fee: @(~$ participation-fee)

      Counting task: @(~$ (compute-counting-payment))

      Lottery: @(~$ (compute-lottery-payment))

      Total: @(~$ (+ participation-fee
                     (compute-counting-payment)
                     (compute-lottery-payment)))

      @button{Continue}})

(defstudy fixed-study
  [set-fee --> consent --> ,(lambda ()
                              (if (equal? consent-given? "yes")
                                  'counting-study
                                  'no-consent))]
  [counting-study --> lottery-study --> payment --> thank-you --> ,(lambda () done)]
  [no-consent --> ,(lambda () done)])
}|}

The interesting part is the two @racket[define-values] blocks near the top. Each one unpacks a
unit. Read the first one slowly. Inside a @racket[(let () ...)] block, it defines
@racketidfont{get-fee}, which is the import the unit asked for. Then
@racket[define-values/invoke-unit] runs the unit’s code, supplying that import and defining the
two exports, @racketidfont{study} and @racketidfont{compute-payment}, right there inside the
@racket[let]. Finally, @racket[values] hands those two things out of the @racket[let], where
@racket[define-values] gives them the names @racketidfont{counting-study} and
@racketidfont{compute-counting-payment}.

Why the @racket[let]? Because invoking a unit @emph{defines} its exports in the surrounding scope.
If you invoked both units at the top level of the file, the second would try to define
@racketidfont{study} a second time, and Racket would refuse with
@dr-message{module: identifier already defined in: study}. The @racket[let] gives each invocation a
small private scope. The exports are defined in there, you pick the ones you want to keep, and you
give them names that won’t clash.

The @racketidfont{get-fee} you define inside the @racket[let] is not passed to the unit as an
argument. The unit finds it by name, in the scope where it is invoked. That is why it has to be
defined right there, and it has to be called exactly @racketidfont{get-fee}. It returns
@racketidfont{participation-fee}, which is the @racket[defvar*] from the shared pages.

After that, the rest of the file is an ordinary Conscript study. @racketidfont{counting-study} and
@racketidfont{lottery-study} are studies, so they go into the transition graph like any other child
study. Each substudy is a step of its own there, under its own name, so each one also stores its data
under its own name; @secref["unitstut-own-name"] explains why that matters. The @racketidfont{set-fee} step stores the fee and skips ahead. The payment page calls the
two exported functions and adds up the total. The consent transition sends participants who
decline to the @racketidfont{no-consent} page and everyone else into the counting substudy.

Upload @filepath{fixed-study.rkt} to your Congame server and try it. The uploader will bundle the
other four files for you, as long as they are in the same folder. If you report solving seven
problems and take the sure payment, the payment page will show a $2.00 fee, $0.70 for counting,
$1.00 for the lottery, and a total of $3.70.

@;===============================================

@section[#:tag "unitstut-random"]{Part 2: Choosing a substudy at random}

In the second version, each participant does one task, chosen at random when they reach that
point in the study. The parent study can’t unpack the substudy’s unit at the top of the file any
more, because it doesn’t know which one it will need until the participant arrives.

@centered{@image[fig-flow]}

Save this as @filepath{randomized-study.rkt}:

@filebox["randomized-study.rkt"]{
@codeblock|{
#lang conscript/with-require

(require conscript/survey-tools
         "counting-study.rkt"
         "lottery-study.rkt"
         "shared-pages.rkt"
         "signatures.rkt")

(provide randomized-study)

(with-namespace my-lab.randomized-study
  (defvar* selected-substudy)
  (defvar* substudy-payment))

(define substudies
  (list (cons 'counting counting-study@)
        (cons 'lottery lottery-study@)))

(defstep (set-fee)
  (set! participation-fee 2.00)
  (skip))

(defstep (pick-substudy)
  (set! selected-substudy (car (list-ref substudies (random (length substudies)))))
  (skip))

(defstep/study run-substudy
  #:study (lambda ()
            (define substudy@ (cdr (assq selected-substudy substudies)))
            (define (get-fee) participation-fee)
            (define-values/invoke-unit substudy@
              (import fee^)
              (export study^ payment^))
            (define (record-payment)
              (set! substudy-payment (compute-payment))
              (skip))
            (defstudy substudy-then-record
              [{,selected-substudy study} --> record-payment --> ,(lambda () done)])
            substudy-then-record))

(defstep (payment)
  @md{# Your payment

      Participation fee: @(~$ participation-fee)

      Task (@(symbol->string selected-substudy)): @(~$ substudy-payment)

      Total: @(~$ (+ participation-fee substudy-payment))

      @button{Continue}})

(defstudy randomized-study
  [set-fee --> consent --> ,(lambda ()
                              (if (equal? consent-given? "yes")
                                  'pick-substudy
                                  'no-consent))]
  [pick-substudy --> run-substudy --> payment --> thank-you --> ,(lambda () done)]
  [no-consent --> ,(lambda () done)])
}|}

The list @racketidfont{substudies} pairs a name with each unit. Adding a third substudy to this study means
adding one more pair to this list, and nothing else. In @secref["unitstut-compose-fn"], the list
itself becomes an argument.

The @racketidfont{pick-substudy} step picks a name at random, stores it in
@racketidfont{selected-substudy}, and skips ahead. The choice is made here, in a step of its own, and
written down before anything uses it. The next section explains why that matters.

The @racketidfont{run-substudy} step is where the unit gets unpacked. Recall from
@secref["pctut-dynamic-generation"] that @racket[defstep/study] accepts a procedure that builds the
child study when the step is reached. That procedure is the @racket[lambda] here, and its body does
the same unpacking as the @racket[let] blocks in Part 1: it looks up the chosen unit, defines
@racketidfont{get-fee}, and invokes the unit, which defines @racketidfont{study} and
@racketidfont{compute-payment} inside the @racket[lambda]. Then it builds a two-step study that
runs the chosen substudy and, once it is done, copies the substudy’s payment into
@racketidfont{substudy-payment}. That little study is what the @racket[lambda] returns, and it is what
the participant runs.

The first step of that little study is written @racket[{,selected-substudy study}]. It runs the
unit’s @racketidfont{study}, but as a step named after the chosen substudy, @racketidfont{counting} or
@racketidfont{lottery}, rather than as a step named @racketidfont{study}. This is the
@racket[{,_step-id-expr _step}] form of @racket[defstudy], which computes a step’s name when the
study is built. @secref["unitstut-own-name"] explains why the name matters.

The @racket[lambda] is a private scope, just as the @racket[let] was. Nothing it defines leaks out
into the rest of the file, so it doesn’t matter that every unit exports the same two names.

There is one thing this version gives up. In Part 1 the units were invoked when the file was
loaded, so a unit that didn’t fit its plugs was rejected the moment you uploaded the study. Here
the unit is invoked inside the @racket[lambda], which runs only when a participant reaches
@racketidfont{run-substudy}, so a unit that doesn’t fit is discovered only then.
@secref["unitstut-compose-fn"] gets the early check back.

Two more variables are @racket[defvar*] here, and neither has anything to do with the signatures.
@racketidfont{substudy-payment} is set inside @racketidfont{run-substudy} and read on the payment page,
which is outside it. @racketidfont{selected-substudy} is set in @racketidfont{pick-substudy} and read on the
payment page too. Both cross a study boundary, so both need to be shared. This is the general form
of the rule from earlier: whenever a value is written inside one study and read from another, it
must be a @racket[defvar*].

Upload @filepath{randomized-study.rkt} and run through it a few times in different private browser
windows. Some participants will get the counting task and others the lottery, and the payment page
names whichever task they did.

@subsection[#:tag "unitstut-pick-first"]{Why the choice is made in its own step}

You might wonder why @racketidfont{pick-substudy} exists at all. Couldn’t the @racket[lambda] in
@racketidfont{run-substudy} just call @racket[random] itself?

The reason is that the @racket[lambda] runs every time a participant @emph{enters} the
@racketidfont{run-substudy} step, and that can happen more than once. If a participant closes the
browser halfway through the task and later returns to the study, Congame walks them back to where
they were, and entering @racketidfont{run-substudy} again runs the @racket[lambda] again. If the
@racket[lambda] picked at random, it might pick a different substudy the second time. Congame would
then try to resume the participant at a step of a study they were never in, which either fails
with an error or, if the two substudies happen to have a step with the same name, silently drops them
into the wrong one.

So the @racket[lambda] must make the same decision every time it runs. The way to guarantee that
is to make the decision once, in a step, store it in a @racket[defvar*], and have the
@racket[lambda] only look it up. The @racket[lambda] should not roll dice, count anything, or
change any variable. It reads stored values and builds a study, nothing more.

The stored choice does a second job in the @racket[lambda]: it names the step the substudy runs in.
That name must also be the same every time, because Congame finds a returning participant’s place
by the names of the steps they are in. Because @racketidfont{selected-substudy} is stored, the name is
the same every time.

@subsection[#:tag "unitstut-own-name"]{Why each substudy runs under a name of its own}

In @filepath{randomized-study.rkt}, the @racket[lambda] runs the substudy as
@racket[{,selected-substudy study}] rather than simply as @racketidfont{study}. The reason has to do
with how Congame files away what a substudy stores.

Recall from @secref["pctut-sharing-data"] that a @racket[defvar] is stored under the study that
sets it. More precisely, every stored value is filed under a path: the names of the steps the
participant passed through to reach the page that stored it, from the outermost study inward, where
each name is the one used in the enclosing study’s transition graph. The name a study gives itself
in @racket[defstudy] plays no part.

In @filepath{fixed-study.rkt}, each substudy is a separate step of the parent, under its own name, so
the counting substudy’s pages store their values under @tt{counting-study} and the lottery’s under
@tt{lottery-study}. The paths are already different, and there is nothing more to do.

@filepath{randomized-study.rkt} is different. Whichever substudy a participant is given, it runs inside
the same @racketidfont{run-substudy} step. If the @racket[lambda]’s little study were written
@racket[[study --> record-payment --> ...]], the substudy would always be the step called
@racketidfont{study}, because that is the name every unit exports. So the path to every substudy’s
pages would be the same, @tt{run-substudy / study}, and both substudies would store their variables at the
same path. For a value that only one participant ever touches, that is merely untidy: you would
need @racketidfont{selected-substudy} to tell which substudy a row of data came from. For a value shared by
all participants in the instance, it is an error. Suppose each substudy kept a @racket[defvar/instance]
named @racketidfont{treatments}, the list of conditions still to be handed out. One list would serve
both substudies, and each would hand out the other’s conditions. The state that @racket[make-matchmaker]
keeps for a study is stored at the path too, so two substudies that each match participants into pairs
would draw from one shared pool, and a participant in the counting task could be paired with one in
the lottery.

The @racket[{,selected-substudy study}] form prevents this. It runs the same @racketidfont{study}, but
as a step named by the value of @racketidfont{selected-substudy}. So the counting substudy’s pages store
their values under @tt{run-substudy / counting}, and the lottery’s under @tt{run-substudy / lottery}. Each
substudy has a path of its own, everything it stores by position stays apart from the other one’s, and
the data itself records which task the participant did.

This is the parent’s job, not the substudy’s. A unit exports its study under the name its signature
promises, and it cannot know how a parent will use it. Only the parent knows whether one of its
steps can run more than one unit, and so only the parent needs to give each unit a name of its own.

@inline-note[#:type 'warning]{The name must be the same every time the @racket[lambda] runs,
including when a participant comes back later and after the server restarts. Congame finds a
returning participant’s place, and the data the substudy stored, by that name. Compute it from the
stored decision, as here, and never with @racket[random] or @racket[gensym]. See @racket[defstudy]
for the other rules of the @racket[{,_step-id-expr _step}] form.}

@inline-note{This is a different job from the one @racket[with-namespace] does. A
@racket[defvar*] is stored under its namespace alone, with no path at all, which is exactly what
makes it reachable from outside the substudy. That is right for @racketidfont{problems-solved}, which
the parent must read, and wrong for something like @racketidfont{treatments}, which should belong
to one substudy and no other. Nor can a namespace of yours reach the state that a library such as
@racket[make-matchmaker] keeps on your behalf. Use the step name to keep a substudy’s own state to
itself, and @racket[defvar*] for values that must cross the boundary.}

One helper works differently: @racket[assigning-treatments] from
@racketmodname[conscript/survey-tools] stores its list of remaining treatments at the top level of
the study, outside any path, so two substudies that both call it must be told apart with its
@racket[#:treatments-key] argument.

@subsection[#:tag "unitstut-standalone"]{Keeping a substudy runnable on its own}

A unit can’t be uploaded and run by itself, because it is only a recipe for a study. If you also
want to run its study on its own, for testing or because it is a study in its own right, you can
make a standalone version by invoking the unit once at the top of its own file and supplying fixed
values for its imports. For the counting substudy the only import is the fee. Add this to the bottom of
@filepath{counting-study.rkt}:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript/with-require
(define counting-study-alone
  (let ()
    (define (get-fee) 2.00)
    (define-values/invoke-unit counting-study@
      (import fee^)
      (export study^ payment^))
    study))
}|

and change the @racket[provide] at the top of the file to @racket[(provide counting-study@
counting-study-alone)]. The unit and the standalone study live in the same file, and the standalone
study is built from the unit, so there is only one copy of the substudy to maintain.

@;===============================================

@section[#:tag "unitstut-compose-fn"]{Part 3: A function that composes any substudies}

@filepath{randomized-study.rkt} does its job, but it is still a study about the counting and lottery
substudies. If another experiment in your lab wanted the same consent page, the same fee, the same
random choice, and the same payment page around three other substudies, you would copy the file and
edit the list. Nothing in the file except that list depends on which substudies it runs, and that
suggests the last step: turn the parent into a function whose arguments are the substudies.

The function goes in a file of its own. Save it as @filepath{compose-substudies.rkt}:

@filebox["compose-substudies.rkt"]{
@codeblock|{
#lang conscript/with-require

(require conscript/survey-tools
         "shared-pages.rkt"
         "signatures.rkt")

(provide compose-substudies)

(with-namespace my-lab.compose-substudies
  (defvar* selected-index)
  (defvar* substudy-payment))

(define (compose-substudies #:fee [fee 2.00] . substudy@s)
  (when (null? substudy@s)
    (raise-argument-error 'compose-substudies "at least one substudy unit" substudy@s))

  (define (check-fit substudy@)
    (define (get-fee) fee)
    (define-values/invoke-unit substudy@
      (import fee^)
      (export study^ payment^))
    (void))
  (for-each check-fit substudy@s)

  (defstep (set-fee)
    (set! participation-fee fee)
    (skip))

  (defstep (pick-substudy)
    (set! selected-index (random (length substudy@s)))
    (skip))

  (defstep/study run-substudy
    #:study (lambda ()
              (define substudy@ (list-ref substudy@s selected-index))
              (define substudy-id
                (string->symbol (format "substudy-~a" selected-index)))
              (define (get-fee) participation-fee)
              (define-values/invoke-unit substudy@
                (import fee^)
                (export study^ payment^))
              (define (record-payment)
                (set! substudy-payment (compute-payment))
                (skip))
              (defstudy substudy-then-record
                [{,substudy-id study} --> record-payment --> ,(lambda () done)])
              substudy-then-record))

  (defstep (payment)
    @md{# Your payment

        Participation fee: @(~$ participation-fee)

        Task: @(~$ substudy-payment)

        Total: @(~$ (+ participation-fee substudy-payment))

        @button{Continue}})

  (defstudy composed
    [set-fee --> consent --> ,(lambda ()
                                (if (equal? consent-given? "yes")
                                    'pick-substudy
                                    'no-consent))]
    [pick-substudy --> run-substudy --> payment --> thank-you --> ,(lambda () done)]
    [no-consent --> ,(lambda () done)])

  composed)
}|}

The study you upload is now a separate, very short file. Save it as @filepath{composed-study.rkt}:

@filebox["composed-study.rkt"]{
@codeblock|{
#lang conscript/with-require

(require "compose-substudies.rkt"
         "counting-study.rkt"
         "lottery-study.rkt")

(provide composed-study)

(define composed-study
  (compose-substudies counting-study@ lottery-study@))
}|}

Most of @filepath{compose-substudies.rkt} is @filepath{randomized-study.rkt} moved inside a function.
@racket[defstep], @racket[defstep/study], and @racket[defstudy] are definitions, and definitions
can live inside a function body just as they live at the top of a file. Each call to
@racketidfont{compose-substudies} runs those definitions afresh and builds a new study around the units
it was given; the steps refer to @racketidfont{substudy@"@"s}, the list of arguments, the way the steps
in Part 2 referred to the list @racketidfont{substudies}. The last expression in the function is the
study, which is what the function returns and what @filepath{composed-study.rkt} provides.

Two declarations stayed outside the function: the @racket[defvar*] variables at the top of the
file. They name places in the database, and those places are the same whichever substudies are passed
in, so there is nothing to gain from redeclaring them on every call. Everything that depends on the
arguments went inside.

The body of the function runs when the file is loaded, before any participant exists, just like the
top level of a unit. That is why it only defines steps and checks its arguments, and reads nothing
from a study variable. All the reading happens inside the steps, while a participant is on the
page.

In Part 2 the parent knew its substudies by name, because it wrote the list itself. A function that
accepts any substudies receives only the units, and a unit has no name. What the function does know is
the order of its arguments, so @racketidfont{pick-substudy} now stores a position, drawn with
@racket[random] from the length of the list, and the @racket[lambda] fetches the chosen unit with
@racket[list-ref]. The rule from @secref["unitstut-pick-first"] is unchanged: the decision is made
in a step, stored, and only looked up by the @racket[lambda]. What is stored has to be something the
database can hold, and a number is; a unit is not.

Without names, the @racket[lambda] also makes the substudy’s step name from the position:
@racketidfont{substudy-id} is @racketidfont{substudy-0} for the first argument, @racketidfont{substudy-1} for
the second, and so on. As @secref["unitstut-own-name"] showed, everything a substudy stores sits under a
path that ends in that name, so the payment page no longer names the task, but the data still
records which argument ran.

@inline-note[#:type 'warning]{Both the stored choice and the step name refer to positions in the
argument list. Once participants have started the study, don’t change the order of the arguments in
@filepath{composed-study.rkt}. A participant who is partway through a task would come back to a
different task, or to an error.}

The fee became a keyword argument, @racket[#:fee], with a default of $2.00. In Part 2 the parent
decided the fee on its own; now the caller decides it, or accepts the default. Anything else the
parent used to decide for itself can move out the same way.

The one genuinely new piece is @racketidfont{check-fit}. A unit’s plugs are checked when the unit
is invoked. In Part 1 that happened as the file was loaded, so a unit that didn’t fit was rejected
at upload. In Part 2 the unit is invoked inside the @racket[lambda], so a unit that doesn’t fit is
discovered only when a participant reaches @racketidfont{run-substudy}. @racketidfont{check-fit}
invokes each unit once, right away, and discards the result. Its only purpose is the check: a unit
that doesn’t export @racketidfont{study^} and @racketidfont{payment^} stops the file from loading,
with a message naming the missing signature, and you see it the moment you upload. The guard above
it does the same for the mistake of passing no substudies at all, which would otherwise surface as an
error from @racket[random] on the first participant’s screen.

Upload @filepath{composed-study.rkt}. The uploader bundles @filepath{compose-substudies.rkt} and the
substudy files along with it, and the study behaves exactly as @filepath{randomized-study.rkt} did.
The difference is in what it costs to change. A third substudy is one more @racket[require] and one
more argument in @filepath{composed-study.rkt}; a second experiment with different substudies is another
file as short as this one; and anything you add inside @racketidfont{compose-substudies}, a waiting
room before the substudy, a balanced assignment in place of @racket[random], an extra page afterwards,
reaches every study built from it without a change to any of them.

@;===============================================

@section[#:tag "unitstut-guidelines"]{Guidelines}

Almost everything that can go wrong with units follows from the same fact: invoking a unit defines
its exports in the scope where you invoke it. The habits below keep you out of trouble whatever
signatures your units use. Each comes with the error you’ll see if you break it, and with what it
means for the substudies in this tutorial.

@bold{Unpack a unit inside a @racket[let] or a @racket[lambda], never at the top of a file.} Two
units invoked at the top level that export the same name produce
@dr-message{module: identifier already defined}. Even a single unit at the top level is a bad
habit, because the next one you add will collide with it. Here, every unit exports
@racketidfont{study}, so unpacking two of them at the top of @filepath{fixed-study.rkt} would
fail.

@bold{Define every import in the same scope, just before you invoke the unit, using exactly the
names the signature uses.} The unit finds its imports by name, not as arguments. If a name is
missing you get @dr-message{unbound identifier} for that name, and a definition under a different
name, however sensible, doesn’t help. The only import in this tutorial is @racketidfont{get-fee},
so each @racket[let] and the @racket[lambda] define @racketidfont{get-fee} first.

@bold{Treat @racket[define-values/invoke-unit] as a definition, not an expression.} It can appear
where a @racket[define] can appear and nowhere else. Writing it inside @racket[(define s (begin
...))] or in the argument of a function call gives
@dr-message{define-values: not allowed in an expression position}.

@bold{Call imported names from inside steps, not at the top level of the unit.} The top level of a
unit runs when the unit is invoked, which may be before any participant has arrived, or before the
parent has set the values an import depends on. Here, @racket[(get-fee)] reads
@racketidfont{participation-fee}, which the parent sets in its @racketidfont{set-fee} step, so the
substudies call it only while showing a page.

@bold{Use @racket[defvar*] for anything that crosses between a unit’s study and the study around
it.} Inside a unit, that means every study variable an exported function reads. In the parent, it
means anything written inside the step that runs the unit’s study and read outside it, or the other
way around. Plain @racket[defvar] is fine for values that stay within one study. Breaking this rule
doesn’t produce an error message; it produces a page that shows @racket[undefined] where a value
should be. In this tutorial the shared variables are @racketidfont{participation-fee},
@racketidfont{problems-solved}, @racketidfont{payoff}, @racketidfont{selected-substudy}, and
@racketidfont{substudy-payment}.

@bold{When one step can run more than one unit, give each unit’s study a step name of its own.} In
the parent, write the unit’s study as @racket[{,_step-id-expr study}], not as @racketidfont{study}.
Otherwise every unit runs under the same step name and keeps its instance variables, and the state
behind @racket[make-matchmaker], in one shared place. There is no error message for getting this
wrong, only a study that quietly mixes two substudies’ data. Here the names are @racketidfont{counting}
and @racketidfont{lottery} in Part 2, and @racketidfont{substudy-0}, @racketidfont{substudy-1}, and so on in
Part 3. A parent that gives each unit a step of its own, as Part 1 does, has nothing to do. The
units themselves always export @racketidfont{study}.

@bold{Decide which unit to run in its own step, store the decision, and have the @racket[lambda]
only look it up.} A @racket[lambda] that makes its own random choice can choose differently when a
participant resumes, and drop them into a different study partway through. Here the decision is
made in @racketidfont{pick-substudy} and stored in @racketidfont{selected-substudy}, or, in Part 3, as a
position in @racketidfont{selected-index}. Store the decision as something the database can hold,
a name or a number, never the unit itself. Make the unit’s step name from the same stored decision.

@bold{When the parent is a function, keep the @racket[defvar*] declarations outside it and
everything else inside.} The steps and studies belong inside, so that each call builds a fresh
study around the units it was given; the variables name places in the database, which are the same
whichever units are passed. The body of the function runs when the file is loaded, before any
participant exists, so it may define steps and check its arguments, and it must not read a study
variable. Here @filepath{compose-substudies.rkt} declares @racketidfont{selected-index} and
@racketidfont{substudy-payment} at the top of the file and defines everything else inside
@racketidfont{compose-substudies}.

@bold{Define each signature once, in a file apart from the studies.} A signature defined twice is
two different signatures, even with the same name, and a unit written against one cannot be plugged
into a parent written against the other. Whether the signatures share one file or each have a file
of their own is up to you. Here all three are in @filepath{signatures.rkt}.

@bold{If a unit exports a study, end that study with @racket[,(lambda () done)] and give its last
page a button.} Without the @racket[done], control never returns to the parent. Without the button,
the participant has no way to reach it.

@bold{Don’t keep participant data in a plain @racket[define] inside a unit.} The unit’s code runs
again every time it is invoked, so anything stored that way is lost. Study variables made with
@racket[defvar] and @racket[defvar*] are stored in the database and survive.

@;===============================================

@section[#:tag "unitstut-when"]{When to use units}

Units are more machinery than most studies need. Any study can already be dropped into another
study as a child, it can return control with @racket[done], and it can share values through
@racket[defvar*] variables that both sides agree on. If you have one parent and one or two children
that were written together, that is the right tool, and @secref["parent-child-tutorial"] covers it.

Reach for units when the pieces are interchangeable. If one place in a study has to accept any of
several studies, chosen at run time, the signature is what makes them all fit, and the compiler
checks the fit before any participant does. Reach for them when a piece must not know who is using
it, because it will be used by several parents or with several settings, and you don’t want every
parent to know the piece’s variable names. And reach for them when the parent needs a calculation
from the piece rather than a number, since a signature can export a function and a
@racket[defvar*] can’t. And reach for them when the study around the pieces is worth writing once,
as a function, and reusing with a different set of pieces each time, as in
@secref["unitstut-compose-fn"].

If none of those apply, use a plain child study.

@;===============================================

@section[#:tag "unitstut-exercise"]{Exercise}

Write a third substudy, a short survey task that asks one question and pays a flat $0.50, as a unit
in its own file. Add it as a third argument to @racketidfont{compose-substudies} in
@filepath{composed-study.rkt}, upload, and run through the study until you’ve seen all three tasks
come up.

Then try leaving out the @racket[(export payment^)] clause in your new substudy and uploading again.
The error tells you, before any participant arrives, that the unit doesn’t fit the slot. That check
is the reason for all of this.

@;===============================================

@section[#:tag "unitstut-recap"]{Key concepts recap}

@itemlist[

@item{A signature is a list of names. A unit imports the signatures for what it needs and exports
the signatures for what it provides. Define each signature once, apart from the studies, where
every unit and every parent can require it. This tutorial uses three: @racketidfont{fee^}, @racketidfont{study^}, and
@racketidfont{payment^}.}

@item{Inside a unit, define every exported name exactly as its signature spells it, and call
imported names only from inside steps. If one of the exports is a study, end it with
@racket[,(lambda () done)].}

@item{Invoking a unit with @racket[define-values/invoke-unit] defines its exports in the current
scope. Always do it inside a @racket[let] or a @racket[lambda], with every import defined right
there.}

@item{For a fixed composition, unpack each unit in its own @racket[let] at the top of the parent.
For a random one, store the choice in a step, then unpack the chosen unit inside the
@racket[lambda] given to @racket[defstep/study].}

@item{Any value that crosses between studies, in either direction, must be a @racket[defvar*].}

@item{A unit exports its study under the name the signature promises. When one step of the parent
can run more than one unit, the parent gives each unit’s study a step name of its own with
@racket[{,_step-id-expr study}], computed from the stored decision. That name gives everything the
substudy stores a path of its own.}

@item{The parent can be a function. @racket[defstep], @racket[defstep/study], and @racket[defstudy]
work inside a function body, the @racket[defvar*] declarations stay outside it, the stored decision
is a position in the argument list, and invoking each unit once as the study is built rejects a
unit that doesn’t fit at upload time.}

]

@;===============================================

@section[#:tag "unitstut-next-steps"]{Next steps}

@itemlist[

@item{Look at @github-link{congame-example-study/many-designs-composition/} for another worked
example: three small studies, two of them units, composed in a fixed order.}

@item{Read @secref["units" #:doc '(lib "scribblings/guide/guide.scrbl")] in the Racket Guide for
what else units can do, such as linking several units together.}

@item{Review the @secref["Conscript_Reference"] for @racket[defstep/study], @racket[defvar*], and
@racket[with-namespace].}

]
