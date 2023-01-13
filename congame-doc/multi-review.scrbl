#lang scribble/manual

@(require (for-label congame-example-study/multi-review))

@title{Multiple-Person Review Study}
@defmodule[congame-example-study/review]

A multi-person review study allows a submitter to submit an assignment that is reviewed by multiple people for scoring. The study provided is @racket{review-study}.

Example of using multi-review study: Suppose students have to submit 5 research ideas, and other students are supposed to evaluate them. First, we have to create a study that elicits 5 research ideas from students. Second, we have to create a study for evaluating the 5 research ideas. Third, we use both of these to create the final corresponding review study.

Future features:

@itemlist[@item{Allow the submitter to review their own submission}
          @item{Add dates (or other conditions) that switch from the submission to the review stage}]
