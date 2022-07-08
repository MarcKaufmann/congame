# Completed Features

- transition graphs that allow adding transitions at the study level rather than the step level
    - When transition graphs are used, it is possible to create a pdf visualizing the transition graph of the study
- data export now allows to export bot data as well as user data
- allow easy deployment of replications (functionality needs testing)
- fixed many bugs
- improved performance
- start testing features automatically, predominantly with bots
- identity server that separates the users who log in from the participants in the study and provides an additional layer of anonymity
    - This was the majority of the work in 2021
    - Studies can send messages to users, and identity server can notify users that they received a message. This way the emails of participants do not have to be shared with the researcher running the study, only the identity server needs to know it.
- researchers can share some information from within their studies with the identity server via `put/identity`. This feature allows a study owner to share information on payments with identity server. We need this whenever participants need to be paid, since we have to match the payments to the identity of a participant.
- upload and store large files to the server rather than the DB
- studies with multiple participants are now possible. congame-example-study contains some studies illustrating this feature, including multi-review.rkt which I used in one of my classes to have students submit assignments and peer review each other. 
- admins can impersonate users, run bots in bot sets (multiple bots) to test studies, or even simulate studies by creating the right model (our term) for bots.
- We now distinguish between admins who can administer the studies they created (or were given explicit access to) and superadmins who can see all studies.

# Features in Progress

The following features are partially implemented and available in the current version, but need fleshing out and improvement:

- Libraries of ready-made studies/steps/widgets/bots
- The current studies implemented in congame highlight some patterns that are common across studies. We are refactoring some of these out into specific libraries to make it easier and faster to build studies that use common components.
- Improve debugging and administration of single studies
    
# Planned Features

- Domain-specific language (DSL) to create studies from existing widgets/studies/study-pieces at a higher level that does not require study designers (whether for research or teaching) to learn Racket
  + including randomizations and treatments
  + this should allow hot-swapping studies: to start them without having to reload the whole server
- Bots:
  + faster simulations by not relying on web browser for running study as bot - direct access to server
- Come up with a way to specify models for bots to run simulations on many studies. 
  + These models should be specified in terms familiar to economists, such as defining the utility function (e.g. Cobb-Douglas with some parameters) and distributions of parameters. The statistical models should probably mirror the syntax of existing software such as R and Stan.
  + This model specification can and probably should work independently from congame, so that it could be used directly, say, for simulations in R. In order to work smoothly with congame, we should add a type system to studies specifying at each step what type of answer it expects. E.g. "Yes/No" answer vs "Utility of work" vs "Utility of money". Then if a bot model specifies all the needed types for a study, it should be capable of automatically running through the study. If on the other hand it is not fully specified -- if the bot does not have a specified "Utility of money" and the study requires this -- then when attempting to simulate the study with this bot, we should receive an error indicating that this is missing. 
  + It may be possible to do such a type analysis statically (before running the bot), but computationally be better to do it via dynamic analysis: a bot runs through a pinned study to generate a template that external programs (R/Stan) need to fill for succesfull runs
    - Issue: since dynamic analysis cannot guarantee that it hits all branches, there has to be a way for study designers to refer (via unique identifiers) to desired/common paths, so this functionality has to integrate with transitions functionality
  + map economic concepts (utility, costs, etc) to the inputs and outputs of steps in order to parameterize bots. Once that is possible, it is enough to parameterize the utility/cost/etc of bots and then derive the answer to a given step from this, rather than specify *exact* answers the bot should give for each specific step.
- Create a library of bots to test properties of the study that we expect to hold. This can be used to test that pages load and behave as expected, that various statistical properties hold (i.e. the study is well-powered for the expected range of answers), and to fully simulate studies. 
- API for integrating with oTree lite: specific steps can be performed by existing (and running) oTree servers that takes certain inputs and provides certain outputs
    - This would allow to reuse existing oTree studies as part of congame studies without having to code them up from scratch
- Add other displays than online web browser studies: e.g. mobile, paper, GUI
  + step handlers have to be written by users, but the transitions and studies should work the same
- Integrate with prolific and MTurk (user messaging, payment, recruitment -- similar to oTree or Gorilla)
- Management of studies
  + Privacy/GDPR compliant by design
    - Thanks to the identity server, we already separate login and study joining/ID from specific studies being run without info about participant
    - We also do not leak any information between studies beyond global data explicitly stated/requested, and it is not possible for one study owner who does not have access to the identity server to access data from the same participant across different studies.
    - We plan on reviewing GDPR requirements and ensuring that congame makes it easy to meet them 
    - Anonymous sign-up? Possible issue is security, i.e. submitting invalid data that looks like it is from one person but isn't
    - A major challenge will be how to share some information across different studies, which might be necessary for long-running panel studies that track the same individual over time. It is also useful to combine data from multiple studies so that researchers don't have to collect the same information again and again.
