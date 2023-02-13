@step[form-step]{
  @form{
    @h1{Section 1}
    @input-text[name]{What is your name?}
    @h1{Section 2}
    @input-number[age #:max 100 #:min 1]{How old are you?}
    @submit-button[]
  }
}
