@import[studies/real-effort/tasks task-study]
@import[studies/real-effort/tasks format]

@define[tutorial-tasks (make-step/study
                            'tutorial-tasks
                            task-study
                            #:require-bindings '([n (const 2)]
                                                 [max-wrong-tasks (const 2)]
                                                 [title (const "Tutorial tasks")]
                                                 [hide-description? (const #f)])
                                                #:provide-bindings '([success? success?]))
]

@step[introduction]{
    @h1{The Study}

    Thank you for participating in this study exploring decisions over task allocations.

    The research is conducted by Marc Kaufmann and financed by the Central European University. Your participation is voluntary and you may withdraw at any time. Participation in the study is not associated with any forseeable risks or benefits beyond the monetary compensation.

    If you have concerns or questions, please contact the principal investigator, Marc Kaufmann, at @"kaufmannm@ceu.edu" or by direct message via Prolific. Report technical problems to @"admin@totalinsightmanagement.com".

    @button{Continue}
}

@action[increment-counter]{
    @put[counter]{@call[format "Hello"]}
}

@step[description]{
    @h1{Study Description}

    @button[#:action increment-counter]{Continue}
}

@step[hypothetical-time-choice]{
    @h1{Hypothetical Question}

    @;TODO: How are hypothetical choices framed in experiments and surveys?

    @button{Continue}
}

@study[
    tutorial
    #:transitions
    [introduction --> description
                  @; With the hypothetical choice we can estimate selection into the study based on present bias (or other traits)
                  --> hypothetical-time-choice
                  --> tutorial-tasks
                  --> @cond[[@=[@get[counter] 2] end]
                            [else introduction]]
    ]
]

@step[sign-up-for-main-study]{
    @h1{Participate in Rest of Study}

    @button{Continue}
}

@;@study[
@;    day1
@;    #:transitions
@;    [required-tasks --> work-choices @; Should we ask work choices first?
@;                    --> determine-choice-that-counts
@;                    --> choice-that-counts
@;                    --> do-chosen-work
@;                    --> explain-choice
@;                    --> schedule-day2-reminder-email]
@;]
@;
@;@study[
@;    day2
@;    #:transitions
@;    [required-tasks --> choice-that-counts
@;    --> do-chosen-work]
@;]

@step[final]{
    @h1{Final step}
}

@study[
    excuse-pilot
    #:transitions
    [tutorial --> final]
    [final --> final]
]

@;{
@;
@; OUTLINE
@;
@; - Introduction with usual boilerplate and description of the study
@; - Tutorial with some tasks so they know what they get into
@; - At least two work days
@; - Signup after the tutorial for the full study
@; - If so, make them sign up properly for an account so they receive an email and message
@;
@; DESIGNS
@;
@; - 5 tasks by default every day to make sure they have something to do
@; - could try to piggy-back on other surveys and ask them to do other surveys worth $5 or $10 on any given day, and we pay them an additional 20% of that amount of work. Problem with that is that they might already have decided to do that work, and then we just pay them extras.
@; - Then choose between (5, 5) and (0, 10); between (5, 5) and (10, 0)
@;      - why not (10, 0) vs (0, 10), and also (10, 0) vs (2, 8) or even choices with different amount of inefficiencies?
@; - Treatments: with reasons given and without reasons given
@;      - This part requires the most thinking and how to do
@;      - Reasons: efficiency (do all at once vs spread across days); give to others, i.e. add a dimension where others (or some charity) receives a benefit from the choice
@; - Treatments for social image:
@;      - default; maybe also evaluate, but along another dimension, and not based on the choices made for work
@;      - compare with another person, and the choice of both will be evaluated by another person based on who they believe to be harder working (or so)
@;      - evaluated based on social giving?
@; - Do Krupka, Weber (2013) on these choices, including with various social image concerns
@; - Ask them some questions that get directly at degree of present bias, at their self-identity.
@;}
}
