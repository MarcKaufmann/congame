@import[studies/real-effort/tasks simple-task-study]
@study[two-tasks #:dynamic (simple-task-study 2 "Some title")]

@step[start]{
  @h1{The start}

  @button{Next}
}

@step[the-end]{
  @h1{The end}
}

@study[
  tasks
  #:transitions
  [start --> two-tasks
         --> the-end]
  [the-end --> the-end]
]
