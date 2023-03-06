@import[somewhere make-tasks]
@study[tasks-study #:dynamic (make-tasks 5)]
@study[
  hello-study
  #:transitions
  [tasks-study --> done]
  [done --> done]
]
