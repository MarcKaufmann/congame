@step[survey-required]{
  @h1{Survey (all fields required)}

  @form{
    @checkbox[checkbox]{Can you check this checkbox? (@"@checkbox")}
    @input-date[data]{What date is today? (@"@input-date")}
    @input-file[uploaded-file]{Upload a file. (@"@input-file")}
    @input-number[n]{What is your favourite number? (@"@input-number")}
    @input-text[plain-text]{What is your name? (@"@input-text")}
    @input-time[time]{What is the local time? (@"@input-time")}
    @textarea[textarea]{Tell me about yourself. Uh-mh. Interesting. Mmhm. (@"@textarea")}
    @submit-button[]{Submit glorious information}}
}

@step[survey-not-required]{
  @h1{Survey (no fields required)}

  @form{
    @checkbox[checkbox-not-r #:required? #f]{Can you check this checkbox? (@"@checkbox")}
    @input-date[data-not-r #:required? #f]{What date is today? (@"@input-date")}
    @input-file[uploaded-file-not-r #:required? #f]{Upload a file. (@"@input-file")}
    @input-number[n-not-r #:required? #f]{What is your favourite number? (@"@input-number")}
    @input-text[plain-text-not-r #:required? #f]{What is your name? (@"@input-text")}
    @input-time[time-not-r #:required? #f]{What is the local time? (@"@input-time")}
    @textarea[textarea-not-r #:required? #f]{Tell me about yourself. Uh-mh. Interesting. Mmhm. (@"@textarea")}
    @submit-button[]{Submit glorious information}}
}

@step[display-answers]{
  @h1{Display all answers}

  Work in progress...
}

@study[
  all-inputs
  #:transitions
  [survey-required --> survey-not-required --> display-answers]
  [display-answers --> display-answers]
]
