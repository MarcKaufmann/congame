@import[stdlib number->string]

@step[description]{
  @h1{Description}

  Welcome to our study. In this study, we will ask for your:

  @ul{
    @li{your age}
    @li{your name}}

  @button{Start Survey}
}

@step[survey]{
  @h1{Survey}

  @form{
    @input-text[first-name]{What is your first name?}
    @input-number[age]{What is your age?}
    @submit-button[]}
}

@step[thank-you]{
  @h1{Thank you, @get['first-name]}

  Thank you for participating in our survey, @get['first-name]! You are the most awesome @call[number->string @get['age]]-year old!
}

@study[
  tutorial2
  #:transitions
  [description --> survey --> thank-you]
  [thank-you --> thank-you]
]
