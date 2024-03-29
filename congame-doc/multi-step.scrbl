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
    @input-number[age #:required? #f]{What is your age?}
    @submit-button[]}
}

@step[thank-you]{
  @h1{Thank you, @(ev (get 'first-name))}

  @; TODO: The next part currently breaks if age is #f.
  Thank you for participating in our survey, @(ev (get 'first-name))! You are the most awesome @(ev (number->string (get 'age)))-year old!
}

@study[
  tutorial2
  #:transitions
  [description --> survey --> thank-you]
  [thank-you --> thank-you]
]
