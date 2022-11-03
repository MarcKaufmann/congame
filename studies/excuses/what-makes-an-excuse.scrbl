@step[introduction]{
    @h1{The Study}

    Thank you for participating in this research study exploring the role of excuses in people's decisions. The survey should take ~10 minutes and you will receive $3 if you complete it.

    The research is conducted by Luca Flóra Drucker and Marc Kaufmann, and financed by the Central European University. It has been approved by the Ethics Review Committee of the Central European University (CEU). Your participation is voluntary and you may withdraw at any time. Participation in the study is not associated with any forseeable risks and or benefits beyond the monetary compensation.

    Please contact the principal investigator, Marc Kaufmann, at kaufmannm@"@"ceu.edu or by direct message via the platform if you have any concerns or questions. Report any technical problems you encounter to admin@"@"totalinsightmanagement.com.

    @button{Start Survey}
}

@template[excuse-list]{
  @ol{
    @li{Who was making an excuse and to whom?}
    @li{What action or behavior was the excuse for?}
    @li{Why do you think the person felt the need for an excuse?}
    @li{What was the excuse?}
    @li{Did you find the excuse convincing or not?}}
}


@step[general-excuse]{

  @h1{Excuses}

  Think of a situation that you witnessed where someone made an excuse for some behavior. Describe the situation, making sure to mention the following aspects:

  @template[excuse-list]{for some behavior}

  @form{
    @textarea[general-excuse]{Describe the situation and excuse:}
    @submit-button[]}
}

@step[procrastination-excuse]{

  @h1{Excuses for procrastinating}

  Think of a situation that you witnessed where someone made an excuse for procrastinating. Describe the situation, making sure to mention the following aspects:

  @template[excuse-list]{for procrastinating}

  @form{
    @textarea[procrastination-excuse]{Describe the situation and excuse:}
    @submit-button[]}
}

@step[work-excuse]{

  @h1{Excuses for working too little}

  Think of a situation that you witnessed where someone made an excuse for procrastinating. Describe the situation, making sure to mention the following aspects:

  @template[excuse-list]{for working too little}

  @form{
    @textarea[working-little-excuse]{Describe the situation and excuse:}
    @submit-button[]}
}

@step[done]{
  @h1{Thank You!}

  Thank you for your answers. You are done.

  @h2{Completion Code: DISCODE}

  Provide the above code on prolific to complete the study.
}

@study[
  excuses
  #:transitions
  [introduction --> general-excuse --> procrastination-excuse --> work-excuse --> done]
  [done --> done]
]
