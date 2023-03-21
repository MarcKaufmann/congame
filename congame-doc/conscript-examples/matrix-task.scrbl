@import[studies/real-effort/tasks simple-task-study variable-task-study]
@study[two-tasks #:dynamic (simple-task-study 2 "Some title")]

@step[start]{
  @h1{The start}

  @button{Next}
}

@step[get-variable-tasks]{
  @h1{How many tasks do you want to do?}

  @form{
    @input-number[allocation #:min 1 #:max 3]{How many tasks do you want to do (between 1 and 3)?}
    @submit-button[]}
}

@step[variable-tasks #:dynamic (variable-task-study
                  'variable-tasks
                  '([n allocation]
                    [title (const "Allocated tasks")]
                    [max-wrong-tasks allocation]
                    [hide-description? (const #t)])
                  '([success? success?]))]

@step[the-end]{
  @h1{The end}
}

@study[
  tasks
  #:transitions
  [start --> two-tasks
         --> get-variable-tasks
         --> variable-tasks
         --> the-end]
  [the-end --> the-end]
]
