#lang scribble/manual

@(require (for-label racket/base
                     racket/contract))

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

@bold{Installation}: See @secref["install-congame"].

@bold{Getting help}: If you have questions about Congame or about this documentation, create a post
in @link["https://github.com/MarcKaufmann/congame/discussions"]{the Discussions area of Congame’s
GitHub repository}, and the authors will respond.

@table-of-contents[]

@include-section["introduction.scrbl"]
@include-section["install.scrbl"]
@include-section["concepts.scrbl"]
@include-section["conscript-section.scrbl"]
@include-section["congame-section.scrbl"]
@include-section["_orig-congame.scrbl"]
