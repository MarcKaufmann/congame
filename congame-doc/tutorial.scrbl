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
    @input-text[name]{What is your name?}
    @input-number[age #:min 0]{What is your age (in years)?}
    @;@input-number[height #:min 0.0]{How tall are you (in meters)?}
    @submit-button[]}
}

@step[thank-you]{
  @h1{Thank you @get['name]}

  Thank you for participating in our survey @get['name]!
}

@study[
  tutorial2
  #:transitions
  [description --> age-height-survey --> thank-you]
  [thank-you --> thank-you]
]
