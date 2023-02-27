@; Study with admin page that shows the number of people who completed the study

@step[number]{
  @(ev (cond [(even? 1) (h1 "Even")]
                 [else (h1 "Odd")]))

  @(ev (p "test " (strong "this") " bla "))
}

@study[
  admin-study
  #:transitions
  [number --> number]
]
