# Completed Features

- transition graphs that allow adding transitions at the study level rather than the step level
- allow easy deployment of replications (functionality needs testing)
- fixed many bugs
- improved performance
- start testing features automatically, predominantly with bots
- identity server that separates the users who log in from the participants in the study and provides an additional layer of anonymity
    - This was the majority of the work in 2021
    - Studies can send messages to users, and identity server can notify users that they received a message. This way the emails of participants do not have to be shared with the researcher running the study, only the identity server needs to know it.
- upload and store large files to the server rather than the DB
- admins can impersonate users, run bots in bot sets (multiple bots) to test studies, or even simulate studies by creating the right model (our term) for bots.

# Features in Progress

The following features are partially implemented and available in the current version, but need fleshing out and improvement:

- Libraries of ready-made studies/steps/widgets/bots/bot preferences (see economic concepts)
- The current studies implemented in congame highlight some patterns that are common across studies. We are refactoring some of these out into specific libraries to make it easier and faster to build studies that use common components.
- Improve debugging and administration of single studies:
    
# Planned Features

- Domain-specific language (DSL) to create studies from existing widgets/studies/study-pieces at a higher level that does not require study designers (whether for research or teaching) to learn Racket
  + including randomizations and treatments
  + this should allow hot-swapping studies: to start them without having to reload the whole server
- Bots:
  + faster simulations by not relying on web browser for running study as bot - direct access to server
- Come up with models or simulations that allow running bots to simulate studies. The primary challenge is how to parameterize bots.
  + API to parameterize bots through statistical models via external programs such as R/Stan
    - This requires dynamic analysis: a bot that runs through a pinned study to generate a template that external programs (R/Stan) need to fill for succesfull runs
    - Issue: since dynamic analysis cannot guarantee that it hits all branches, there has to be a way for study designers to refer (via unique identifiers) to desired/common paths, so this functionality has to integrate with transitions functionality
  + map economic concepts (utility, costs, etc) to the inputs and outputs of steps in order to parameterize bots. Once that is possible, it is enough to parameterize the utility/cost/etc of bots and then derive the answer to a given step from this, rather than specify *exact* answers the bot should give for each specific step.
  + library to use bots to test properties of the study that we expect to hold: bots should fail at specific points if they give the wrong answer, see specific inputs in a certain range that depends on their prior answers
- API for integrating with oTree lite: specific steps can be performed by existing (and running) oTree servers that takes certain inputs and provides certain outputs
- Add other displays than online web browser studies: e.g. mobile, paper, GUI
  + step handlers have to be written by users, but the transitions and studies should work the same
- Integrate with prolific and MTurk (user messaging, payment, recruitment -- similar to oTree or Gorilla)
- Management of studies
  + Privacy/GDPR compliant by design
    - Separate login and study joining/ID from specific studies being run without info about participant
    - No leaking of information between studies beyond global data explicitly stated/requested
    - Anonymous sign-up? Possible issue is security, i.e. submitting invalid data that looks like it is from one person but isn't
  + Manage which user can see which study
  + Dependencies between studies (i.e. only see one after having completed part of another one)
  + Individual studies should have the raw data
- Add new user roles that don't have full admin access, or only to studies they themselves created and ran
