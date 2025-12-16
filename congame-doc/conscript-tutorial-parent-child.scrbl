#lang scribble/manual

@(require (for-label conscript/base
                     conscript/form0
                     racket/contract
                     racket/list)
          "doc-util.rkt")

@title[#:style 'quiet #:tag "parent-child-tutorial"]{Tutorial: Composing Studies}

This tutorial explains how to build studies from smaller, reusable parts. You'll learn how to:

@itemlist[

@item{Embed one study inside another as a "child" study}

@item{Reuse the same child study multiple times}

@item{Share data between parent and child studies}

@item{Generate study steps dynamically}

]

This tutorial assumes you're familiar with the basic concepts covered in @secref["overview"] and
@secref["intro"].

@;===============================================

@section[#:tag "pctut-why-compose"]{Why Compose Studies?}

As your studies grow more complex, you'll often find yourself wanting to reuse the same sequence of
steps in multiple places. For example:

@itemlist[

@item{A consent form that appears at the start of several different studies}

@item{A demographics questionnaire used across multiple experiments}

@item{A task that participants repeat several times with slight variations}

]

Rather than copying and pasting the same steps, you can define them once as a child study and
then embed that study wherever you need it. This approach has several benefits:

@itemlist[

@item{@bold{Reusability:} Define a component once, use it many times}

@item{@bold{Maintainability:} Fix a bug or update wording in one place}

@item{@bold{Clarity:} Break complex studies into understandable pieces}

]

@;===============================================

@section[#:tag "pctut-simple-example"]{A Simple Example}

Let's start with a concrete example. Suppose you want to ask participants the same question at
different points in your study: "How tired are you right now?" Rather than writing the same step
three times, you can define a small "fatigue" study and embed it multiple times.

Create a new file and save it as @filepath{fatigue-study.rkt}:

@filebox["fatigue-study.rkt"]{
@codeblock|{
#lang conscript

(require conscript/form0)

(provide main-study)

(defvar tiredness)

;; ---------------------------------------------
;; Child study: a single question about fatigue
;; ---------------------------------------------

(define-values (tiredness-form tiredness-onsubmit)
  (form+submit
   [tiredness (ensure binding/number (required))]))

(define (render-tiredness rw)
  @md*{On a scale from 1 (very tired) to 5 (wide awake),
       how tired are you right now?

       @rw["tiredness" @input-number[#:attributes '([min "1"] [max "5"])]]

       @|submit-button|})

(defstep (how-tired)
  @md{# How are you feeling?

      @form[tiredness-form tiredness-onsubmit render-tiredness]})

(defstudy fatigue-check
  [how-tired --> ,(lambda () done)])

;; ---------------------------------------------
;; Parent study that uses the child study twice
;; ---------------------------------------------

(defstep (welcome)
  @md{# Welcome

      This study will ask you about your energy levels at different points.

      @button{Begin}})

(defstep (task)
  @md{# Task

      Imagine you are completing an important task here...

      @button{Done}})

(defstep (thank-you)
  @md{# Thank You

      You have completed the study.})

(defstudy main-study
  [welcome --> [fatigue1 fatigue-check]
           --> task
           --> [fatigue2 fatigue-check]
           --> thank-you]
  [thank-you --> thank-you])
}|}

There are two important patterns to understand in this code.

@bold{Ending a child study.} Notice the transition at the end of @racket[fatigue-check]:
@racket[,(lambda () done)]. This is how a child study signals that it's finished. When the child
study reaches @racket[done], control returns to the parent study, which then continues to its next
step.

@bold{Embedding a child study.} In the @racket[main-study] definition, the syntax @racket[[fatigue1
fatigue-check]] means "run the @racket[fatigue-check] study and call this instance
@racket[fatigue1]". Each embedding needs a unique name (@racket[fatigue1], @racket[fatigue2]) so
that their data is stored separately. Without distinct names, both embeddings would share the same
@racket[tiredness] value and overwrite each other.

When a participant goes through this study, they will:

@itemlist[#:style 'ordered

@item{See the welcome page}

@item{Complete the first fatigue check (stored under @racket[fatigue1])}

@item{Complete the task}

@item{Complete the second fatigue check (stored under @racket[fatigue2])}

@item{See the thank you page}

]

Upload this study to your Congame server (using the @onscreen{Upload Study} button in DrRacket) and
try it out to see how the child study is embedded twice.

@;===============================================

@section[#:tag "pctut-sharing-data"]{Sharing Data Between Parent and Child}

Often you'll want the parent study to pass data to a child study, or you'll want the
child study to use values set by the parent.

When you define a variable with @racket[defvar], the variable's @emph{value} is stored in the
database under the current study's namespace. When a child study runs, it operates in its own
namespace --- so even though the variable identifier is lexically in scope (it's defined in the same
file), a @racket[defvar] variable in the parent and child would read and write to @emph{different
storage locations}. The child wouldn't see the value the parent set.

To share data between parent and child studies, use @racket[defvar*] together with
@racket[with-namespace]:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(with-namespace my-study.variables
  (defvar* participant-name))
}|

The @racket[with-namespace] form creates a globally unique storage key by combining the namespace
with the variable name. Both parent and child studies then read and write to the @emph{same}
underlying storage location. Always use @racket[with-namespace] when using @racket[defvar*].

Here's a complete example where the parent collects a participant's name and passes it to a child
study that displays a personalized greeting:

@filebox["greeting-study.rkt"]{
@codeblock|{
#lang conscript

(require conscript/form0)

(provide main-study)

;; Shared variable: visible to both parent and child studies
(with-namespace my-greeting-app
  (defvar* participant-name))

;; ---------------------------------------------
;; Child study: displays a personalized greeting
;; ---------------------------------------------

(defstep (greet)
  @md{# Hello, @|participant-name|!

      Welcome to this part of the study.

      @button{Continue}})

(defstudy personalized-greeting
  [greet --> ,(lambda () done)])

;; ---------------------------------------------
;; Parent study: collects name, then uses child
;; ---------------------------------------------

(define-values (name-form name-onsubmit)
  (form+submit
   [participant-name (ensure binding/text (required))]))

(define (render-name rw)
  @md*{@rw["participant-name" @input-text{What is your name?}]

       @|submit-button|})

(defstep (get-name)
  @md{# Tell us about yourself

      @form[name-form name-onsubmit render-name]})

(defstep (done-page)
  @md{# All Done

      Thanks for participating, @|participant-name|!})

(defstudy main-study
  [get-name --> [greeting personalized-greeting] --> done-page]
  [done-page --> done-page])
}|}

The @racket[participant-name] variable is defined once at the top of the file with @racket[defvar*].
Because it uses @racket[defvar*] (not plain @racket[defvar]), it is visible to both the parent
study @racket[main-study] and the child study @racket[personalized-greeting].

If you need a shared variable that is @emph{also} shared across all participants in the study
instance (like a counter or configuration value), use @racket[defvar*/instance] instead of
@racket[defvar*]. Like @racket[defvar*], it must be used inside a @racket[with-namespace] block.
This combines the visibility of @racket[defvar*] with the instance-scoped behavior of
@racket[defvar/instance].

@;===============================================

@section[#:tag "pctut-dynamic-generation"]{Dynamic Study Generation}

In the previous examples, child studies were defined with @racket[defstudy] and embedded directly
in the parent's transition graph using the @racket[[step-id child-study]] syntax. This works
because the child study's structure is fixed at the time your code is loaded.

But sometimes you don't know in advance how many steps you need --- the structure depends on a
value the participant provides at runtime. In these cases, you need to pass a @emph{procedure} that
creates the study when the step is reached, rather than a pre-built study value. The
@racket[make-step/study] function handles both cases: it accepts either a study directly or a
procedure that returns one.

This example lets a participant choose how many questions they want to answer. The number of
question pages shown will match what they enter:

@filebox["dynamic-study.rkt"]{
@codeblock|{
#lang conscript

(require conscript/form0)

(provide dynamic-example)

;; Use defvar* so the generated substudy can access it
(with-namespace my-study.dynamic
  (defvar* n)
  (defvar* last-response))

(define-values (start-form start-onsubmit)
  (form+submit
   [n (ensure binding/number (required) (range/inclusive 1 5))]))

(define (render-start rw)
  @md*{How many questions would you like to answer (1-5)?

       @rw["n" @input-number[#:attributes '([min "1"] [max "5"])]]

       @|submit-button|})

(defstep (start)
  @md{# Setup

      @form[start-form start-onsubmit render-start]})

(define-values (question-form question-onsubmit)
  (form+submit
   [last-response (ensure binding/text (required))]))

(define (render-question rw)
  @md*{@rw["last-response" @input-text{Enter any text:}]

       @|submit-button|})

(defstep (question i)
  @md{# Question @number->string[(add1 i)]

      This is question number @number->string[(add1 i)] of @number->string[n].

      @form[question-form question-onsubmit render-question]})

(define (make-questions)
  (for/study ([i (in-range n)])
    (question i)))

(defstep (end)
  @md{# Complete

      You answered @number->string[n] questions.})

(defstudy dynamic-example
  [start --> [questions (make-step/study 'questions make-questions)] --> end]
  [end --> end])
}|}

Let's break down what's happening:

@itemlist[#:style 'ordered

@item{@racket[n] is defined with @racket[defvar*] so the dynamically generated study can access it.}

@item{The @racket[start] step collects how many questions the participant wants using a form built
with @racket[form+submit]. The @racket[range/inclusive] validator ensures the value is between 1 and
5.}

@item{@racket[(question i)] is a step function that takes an argument. Each call to
@racket[(question i)] creates a different step showing "Question 1", "Question 2", etc.}

@item{@racket[make-questions] uses @racket[for/study] to generate a study with @racket[n] steps.
Each iteration creates one step by calling @racket[(question i)] where @racket[i] goes from
@racket[0] to @racket[(sub1 n)].}

@item{In the @racket[defstudy] transition graph, the syntax @racket[[questions (make-step/study
'questions make-questions)]] embeds the dynamically generated study. The @racket[make-step/study]
call takes a step identifier and a procedure that returns a study. Because @racket[make-questions]
is a procedure (not a study value), it gets called when the step is reached --- after @racket[n] has
been set. This timing is essential: if you passed the study directly, it would be created before the
participant had a chance to set @racket[n].}

]

@;===============================================

@section[#:tag "pctut-recap"]{Key Concepts Recap}

@itemlist[

@item{@bold{Child studies} are embedded using @racket[[step-id child-study]] syntax in transitions.
Each embedding needs a unique step name.}

@item{@bold{Ending child studies:} Use @racket[,(lambda () done)] as the final transition. This
returns control to the parent study.}

@item{@bold{Shared variables:} Use @racket[defvar*] wrapped in @racket[with-namespace] to create
variables visible to child studies.}

@item{@bold{Dynamic generation:} Use @racket[for/study] to generate studies with a variable number
of steps, and @racket[make-step/study] to embed them in your transition graph.}

]

@;===============================================

@section[#:tag "pctut-next-steps"]{Next Steps}

Now that you understand how to compose studies, you might want to:

@itemlist[

@item{Look at @github-link{congame-example-study/conscript-for-study.rkt} for another working
example of dynamic study generation}

@item{Consult the @secref["Conscript_Cookbook"] for more recipes and patterns}

@item{Review the @secref["Conscript_Reference"] for detailed documentation of @racket[make-step/study],
@racket[defvar*], and related forms}

]
