@define[x 42]

@step[list]{
  x = @(ev (~a x))
}

@step[done]{You're done.}

@study[
  hello-study
  #:transitions
  [list --> done]
  [done --> done]
]
