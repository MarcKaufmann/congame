@step[survey-required]{
  @h1{Survey (all fields required)}

  @form{
    @checkbox[checkbox]{Can you check this checkbox? (@"@checkbox")}
    @input-date[data]{What date is today? (@"@input-date")}
    @input-file[uploaded-file]{Upload a file. (@"@input-file")}
    @input-number[n]{What is your favourite number? (@"@input-number")}
    @input-range[slider]{Slide around to find a number between 0 and 100 (the defaults). (@"@input-range")}
    @input-text[plain-text]{What is your name? (@"@input-text")}
    @input-time[time]{What is the local time? (@"@input-time")}
    @radios[radios '(("yes" . "Yes") ("no" . "No") ("maybe" . "Maybe"))]{What is your answer? (@"@radios")}
    @select[select '(("blue" . "Blue") ("orange". "Orange") ("red" . "Red"))]{Which colors do you like? (@"select")}
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
    @input-range[slider-not-r #:required? #f]{Slide around to find a number between 0 and 100 (the defaults). (@"@input-range")}
    @input-text[plain-text-not-r #:required? #f]{What is your name? (@"@input-text")}
    @input-time[time-not-r #:required? #f]{What is the local time? (@"@input-time")}
    @radios[radios-not-r '(("yes" . "Yes") ("no" . "No") ("maybe" . "Maybe")) #:required? #f]{What is your answer? (@"@radios")}
    @select[select-not-r '(("blue" . "Blue") ("orange" . "Orange") ("red" . "Red")) #:required? #f]{Which color do you like? (@"select")}
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
