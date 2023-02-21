@; level2 2
@step[day1-main-session]{
  @h1{Day1 Main Session}

  What will happen here is that the person answers:

  @ul{
    @li{How willing they are to work between...}}

  This is the challengin part, since I have to implement the game or the lottery...

  @button{Next}
}

@step[landing-screen]{
  @h1{The landing screen}

  @button{Next}
}

@; level 1 studies
@;{
Bla bla
bla
}
@study[
  day1
  #:transitions
  [landing-screen --> description
                  --> tutorial
                  --> comprehension-test
                  --> consent
                  --> day1-main-session
                  --> wait-page-until-session2]
]

@study[
  day2
  #:transitions
  [day2-main-session --> debrief-survey --> demographic-survey]
]

@; level 0 (top level)
@study[
  edpb
  #:transitions
  [day1 --> day2 --> thank-you]
]

@; Exercises
@study[exercises1 ...]
@study[exercises2 ...]

@study[exercises
  #:transitions
  [choice-page --> @(ev (lambda () (cond [...])))]
  [exercises1 --> choice-page]
]
