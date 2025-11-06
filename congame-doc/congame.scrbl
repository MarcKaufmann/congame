#lang scribble/manual

@(require (for-label racket/base
                     racket/contract)
          "doc-util.rkt")

@title[#:style 'toc]{Congame}
@author[(author+email "Marc Kaufmann" "marc@trichotomy.xyz")]

Congame is a platform for designing and deploying web-based research studies, and is @emph{the best
experimental economics software written in Racket}. 

Once deployed, a Congame @tech{study} becomes a web application. People can use their browsers to
enroll in the study and participate by walking through each of the study’s steps, providing
responses to questions and even interacting in controlled ways with other participants. Congame
collects the responses and provides the data to study authors for further analysis.

Congame includes:

@itemlist[ 

@item{@secref["Conscript"], a scripting environment for writing studies.}

@item{Libraries and a server application for hosting studies (@secref["The_Congame_Server"])}

]

@inline-note{@bold{Installation}: See @secref["install-congame"].

@bold{Getting help}: See @secref["Help_and_support"].}

@inline-note{The canonical copy of this documentation lives at
@url{https://docs.totalinsightmanagement.com}.}

@table-of-contents[]

@include-section["introduction.scrbl"]
@include-section["install.scrbl"]
@include-section["concepts.scrbl"]
@include-section["conscript-section.scrbl"]
@include-section["congame-section.scrbl"]
@include-section["reference.scrbl"]
@include-section["_orig-congame.scrbl"]
