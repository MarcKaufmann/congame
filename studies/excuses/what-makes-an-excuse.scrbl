@step[introduction]{
    @h1{The Study}

    Thank you for participating in this research study exploring the
    role of excuses in people's decisions. The survey should take ~15
    minutes and you will receive £2.50 if you complete it. You will be
    asked to describe situations in which people make excuses for 3
    different behaviors, followed by 2 general questions about
    excuses.

    The research is conducted by Marc Kaufmann, and financed by the
    Central European University. Your participation is voluntary and
    you may withdraw at any time. Participation in the study is not
    associated with any forseeable risks or benefits beyond the
    monetary compensation.

    Please contact the principal investigator, Marc Kaufmann, at
    kaufmannm@"@"ceu.edu or by direct message via the platform if you
    have any concerns or questions. Report any technical problems you
    encounter to admin@"@"totalinsightmanagement.com.

    @button{Start Survey}
}

@template-ungrouped[excuse-list]{
  @h1{Excuses @yield[]}

  Think of a situation that you witnessed where someone made an excuse
  @yield[]. Describe the situation, making sure to mention the
  following aspects:

  @ol{
    @li{Who was making an excuse and to whom?}
    @li{What action or behavior was the excuse for?}
    @li{What was the excuse?}
    @li{Did the excuse achieve anything?}}
}

@step[general-excuse]{
  @template[excuse-list]{for some behavior}

  @form{
    @textarea[general-excuse]{Describe the situation and excuse:}
    @submit-button[]}
}

@step[procrastination-excuse]{
  @template[excuse-list]{for procrastinating}

  @form{
    @textarea[procrastination-excuse]{Describe the situation and excuse:}
    @submit-button[]}
}

@step[work-excuse]{
  @template[excuse-list]{for working too little}

  @form{
    @textarea[working-little-excuse]{Describe the situation and excuse:}
    @submit-button[]}
}

@step[missing-goal-excuse]{
  @template[excuse-list]{for missing a goal}

  @form{
    @textarea[missing-goal-excuse]{Describe the situation and excuse:}
    @submit-button[]}
}

@step[not-helping-excuse]{
  @template[excuse-list]{for not helping someone else}

  @form{
    @textarea[not-helping-excuse]{Describe the situation and excuse:}
    @submit-button[]}
}

@step[what-is-an-excuse]{
  @h1{Final Questions}

  @form{
    @textarea[why-make-excuses]{Why do you usually make an excuse?}
    @textarea[do-excuses-change-response]{
      Do you think that excuses change how people are treated, even when
      their are doubts whether they are true?
    }
    @submit-button[]
  }
}

@step[done]{
  @h1{Thank You!}

  Thank you for your answers. You are done.

  @h2{Completion Code: CES9S6UR}

  Provide the above code on prolific to complete the study.
}

@;TODO: add @substudy syntax to #:transitions (pre-pass to extract @substudy and bind it).
@define[substudy (make-randomized-study (hasheq 'missing-goal-excuse missing-goal-excuse
                                                'not-helping-excuse not-helping-excuse
                                                'procrastination-excuse procrastination-excuse))]

@study[
  excuses
  #:transitions
  [introduction --> substudy --> what-is-an-excuse --> done]
  [done --> done]
]
