#lang scribble/manual

@(require [for-label racket/base])

@title{Overview}

What follows is a high-level overview of Congame's systems and how they all work together. If you've
just finished the @seclink["intro"]{introductory tour} then this will give you some more context for
what you saw.

@section{Basic Concepts}

A @deftech{study} is a series of @tech{steps} and a transition graph that controls how
study participants proceed through those steps.

A @deftech{step} is a point within a @tech{study} at which we provide information to
a participant, and/or collect a response from them. Steps in Congame correspond to
individual web pages.

A @deftech{study instance} is a discrete running of a @tech{study} on a particular server,
and between specific start and end dates/times.

A @deftech{replication} is a duplicate of a @tech{study instance}, create specifically to test
whether a study instance's results can be replicated.

@section{Building Studies}

Two ways to author studies, depending on whether you're running your own Congame server.

@section{Programming in Racket}

@section{Scribble Syntax}

Conscript allows use of Scribble syntax. Quick explainer

@section{Web Pages}

Study steps eventually become web pages, so you may need to know a little about HTML and Markdown.