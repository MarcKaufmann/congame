@step[survey]{
  @h1{Survey}

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

@step[display-answers]{
  @h1{Display all answers}

  Work in progress...
}

@study[
  all-inputs
  #:transitions
  [survey --> display-answers]
  [display-answers --> display-answers]
]
