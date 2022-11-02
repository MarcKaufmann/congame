@step[general-excuse]{
  @h1{Excuse}

  Think of a situation that you witnessed where a person made an excuse for some action or event. Make sure to describe each of the following points (not necessarily in that order):

  @ol{
    @li{What was the situation?}
    @li{Who was making an excuse? (Do not tell us the name, but the role of the person to understand the situation: colleague, employee, brother, parent, boss, ...)}
    @li{To whom did they make an excuse? (Same, no need to tell us the name, just tell us the role of the person to us.)}
    @li{What action or behavior did the person excuse?}
    @li{What excuse did they use -- what did they say (or do)?}
    @li{Why do you think they did excuse their behavior?}
    @li{Do you consider that an excuse was necessary in that situation?}
    @li{Were you convinced by the excuse -- do you consider it a good or a poor excuse?}}

  @form{
    @textarea[general-excuse]{Describe the situation and excuse:}
    @submit-button[]}
}

@step[pb-excuse]{
  @h1{Excuses for Being Late}

  Now think of @strong{a different situation} where a person made an excuse for @strong{not completing something on time}. As a reminder, make sure to describe each of the following points (not necessarily in that order):

  @ol{
    @li{What was the situation?}
    @li{Who was making an excuse? (Do not tell us the name, but the role of the person to understand the situation: colleague, employee, brother, parent, boss, ...)}
    @li{To whom did they make an excuse? (Same, no need to tell us the name, just tell us the role of the person to us.)}
    @li{What action or behavior did the person excuse?}
    @li{What excuse did they use -- what did they say (or do)?}
    @li{Why do you think they did excuse their behavior?}
    @li{Do you consider that an excuse was necessary in that situation?}
    @li{Were you convinced by the excuse -- do you consider it a good or a poor excuse?}}

  @form{
    @textarea[pb-excuse]{Describe the situation and excuse:}

    @submit-button[]}
}

@step[work-excuse]{
  @h1{Excuses for Achieving Little}

  Now think of @strong{a different situation} where a person made an excuse for @strong{achieving little}. As a reminder, make sure to describe each of the following points (not necessarily in that order):

  @ol{
    @li{What was the situation?}
    @li{Who was making an excuse? (Do not tell us the name, but the role of the person to understand the situation: colleague, employee, brother, parent, boss, ...)}
    @li{To whom did they make an excuse? (Same, no need to tell us the name, just tell us the role of the person to us.)}
    @li{What action or behavior did the person excuse?}
    @li{What excuse did they use -- what did they say (or do)?}
    @li{Why do you think they did excuse their behavior?}
    @li{Do you consider that an excuse was necessary in that situation?}
    @li{Were you convinced by the excuse -- do you consider it a good or a poor excuse?}}

  @form{
    @textarea[work-excuse]{Describe the situation and excuse:}
    @submit-button[]}
}

@step[done]{
  @h1{Thank You!}

  Thank you for your answers. You are done.
}

@study[
  excuses
  #:transitions
  [general-excuse --> pb-excuse --> work-excuse --> done]
  [done --> done]
]
