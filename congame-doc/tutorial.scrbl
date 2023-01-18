@step[start]{
    @h1{The Beginning is the End}
}

@study[
  tutorial1
  #:transitions
  [start --> start]
]

@step[description]{
  @h1{The study}

  Welcome to our study. In this study, we will ask you about

  @ul{
    @li{your age}
    @li{your height}}

  @button{Start Survey}
}

@step[age-height-survey]{
  @h1{Survey}

  @form{
    @input-text[first-name]{What is your first name?}
    @input-number[age]{What is your age (in years)?}
    @submit-button[]}
}

@step[thank-you]{
  @h1{Thank you @get['first-name]}

  Thank you for participating in our survey @get['first-name]!
}

@study[
  tutorial2
  #:transitions
  [description --> age-height-survey --> thank-you --> thank-you]
]
