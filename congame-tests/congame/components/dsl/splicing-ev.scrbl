@step[list]{
  Test
  @ul{
    @splicing-ev[(list "a" "b" "c")]
  }
}

@step[done]{You're done.}

@study[
  hello-study
  #:transitions
  [list --> done]
  [done --> done]
]
