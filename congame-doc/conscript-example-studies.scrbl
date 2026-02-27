#lang scribble/manual

@(require "doc-util.rkt")

@title{Example Studies}

The @filepath{congame-example-study} directory contains a collection of example studies that
demonstrate the features of Congame and Conscript. Each link below points to the source code on
GitHub.

@section{Getting Started}

@itemlist[

@item{@github-link{congame-example-study/conscript.rkt} --- A basic Conscript study with simple
steps and a consent page.}

@item{@github-link{congame-example-study/conscript-how-tos.rkt} --- Common tasks: creating links,
displaying monetary amounts, and navigating between steps with buttons.}

]

@section{Forms & Input}

@itemlist[

@item{@github-link{congame-example-study/conscript-easy-forms/study.rkt} --- Comprehensive forms
tutorial covering checkboxes, tables, LaTeX/MathJax, timers, sliders, dice rolls, image selection,
and dropdowns.}

@item{@github-link{congame-example-study/conscript-forms-lib.rkt} --- Using the forms library with
checkboxes, radios, and selects.}

@item{@github-link{congame-example-study/conscript-radio-table.rkt} --- Radio buttons arranged in a
custom table layout.}

@item{@github-link{congame-example-study/conscript-radio-shuffle-and-style.rkt} --- Shuffling and
styling radio button options.}

@item{@github-link{congame-example-study/conscript-range-formlet.rkt} --- Range and scale form fields
with static and dynamic definitions.}

@item{@github-link{congame-example-study/conscript-multi-input-validation.rkt} --- Multi-field form
validation with constraints such as requiring inputs to sum to 100.}

]

@section{Randomization & Dynamic Studies}

@itemlist[

@item{@github-link{congame-example-study/conscript-step-randomizer.rkt} --- Randomly assigning
participants to treatment or control conditions.}

@item{@github-link{congame-example-study/conscript-shuffle.rkt} --- Dynamically shuffling the order
of steps.}

@item{@github-link{congame-example-study/conscript-for-study.rkt} --- Generating studies dynamically
with @racket[for/study].}

@item{@github-link{congame-example-study/conscript-defstudy-dynamic-study.rkt} --- Dynamic study
creation with @racket[for/study] and @racket[make-step/study].}

@item{@github-link{congame-example-study/conscript-choice-that-counts.rkt} --- Randomly selecting
one task for payment, with slider inputs and scoring.}

]

@section{Multi-Participant Studies}

@itemlist[

@item{@github-link{congame-example-study/prisoners-dilemma.rkt} --- A full prisoner's dilemma with
matchmaking, bots, admin pages, and repeated rounds.}

@item{@github-link{congame-example-study/prisoners-dilemma-redux.rkt} --- A simplified prisoner's
dilemma.}

@item{@github-link{congame-example-study/dictator-game.rkt} --- A dictator game with matchmaking
and role assignment.}

@item{@github-link{congame-example-study/conscript-multi-example.rkt} --- A multi-participant study
with balanced randomization and competitive payments.}

@item{@github-link{congame-example-study/conscript-three-plus-participants.rkt} --- Groups of three
or more participants with role assignment.}

@item{@github-link{congame-example-study/proposal-choice.rkt} --- A four-person game with three
proposers and one responder.}

]

@section{Data Sharing & Variables}

@itemlist[

@item{@github-link{congame-example-study/conscript-defvar.rkt} --- Sharing global variables across
modules using @racket[defvar*].}

@item{@github-link{congame-example-study/conscript-defvar-consent.rkt} --- Reusing global variables
in a separate consent module (companion to @tt{conscript-defvar.rkt}).}

@item{@github-link{congame-example-study/conscript-communication.rkt} --- Parent-child study
communication with namespaced variables.}

@item{@github-link{congame-example-study/conscript-data-sharing.rkt} --- Passing data between
parent and nested studies.}

]

@section{Styling & Media}

@itemlist[

@item{@github-link{congame-example-study/conscript-css.rkt} --- Adding inline CSS to a study.}

@item{@github-link{congame-example-study/conscript-css-resource.rkt} --- Loading external CSS files
as resources.}

@item{@github-link{congame-example-study/conscript-images.rkt} --- Loading and displaying images
in a study.}

@item{@github-link{congame-example-study/zip-study-example/study.rkt} --- Loading images using
resource URIs.}

]

@section{Admin, Bots & Other Features}

@itemlist[

@item{@github-link{congame-example-study/conscript-bot.rkt} --- Bot autofill with @racket[form0]
and bot model definitions.}

@item{@github-link{congame-example-study/conscript-with-admin.rkt} --- Creating studies with admin
pages.}

@item{@github-link{congame-example-study/conscript-push-notification.rkt} --- Sending push
notifications to participants.}

@item{@github-link{congame-example-study/conscript-archetype1.rkt} --- Treatment randomization with
comprehension checks and multiple-choice questions.}

@item{@github-link{congame-example-study/phd-seminar.rkt} --- A longitudinal survey with multiple
phases and file uploads.}

@item{@github-link{congame-example-study/many-designs/ach91.rkt} --- Design notes for a behavioral
economics experiment on self-reported performance and competition (comments only, no runnable code).}

]

@section{Legacy Examples}

These examples use @tt{#lang racket/base} with the older Congame API rather than Conscript.

@itemlist[

@item{@github-link{congame-example-study/example.rkt} --- Core study features: forms, nested
studies, and wrappers.}

@item{@github-link{congame-example-study/looping.rkt} --- Looping and repeated substudies with
round counters.}

@item{@github-link{congame-example-study/deferred.rkt} --- Deferred study creation and study
wrapping.}

@item{@github-link{congame-example-study/matchmaking.rkt} --- Manual matchmaking with group
assignment and lobbies.}

@item{@github-link{congame-example-study/quizzes.rkt} --- A quiz system with admin grading.}

@item{@github-link{congame-example-study/instructor-review.rkt} --- Submission and grading with
file uploads and PDF annotations.}

@item{@github-link{congame-example-study/multi-review.rkt} --- Peer review with submission and
review phases.}

@item{@github-link{congame-example-study/edpb-survey.rkt} --- A behavioral economics survey with
subjective beliefs and phased answer release.}

@item{@github-link{congame-example-study/calibration.rkt} --- An interactive calibration form for
indifference-point elicitation.}

@item{@github-link{congame-example-study/inline.rkt} --- Inline form handling with matrix tables
and dynamic field rendering.}

@item{@github-link{congame-example-study/prolific-redirect.rkt} --- Redirecting participants to
Prolific on study completion.}

]
