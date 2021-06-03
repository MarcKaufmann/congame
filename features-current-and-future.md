# Features in Progress

The following features are partially implemented and available in the current version, but need fleshing out and improvement:

- Simple way to build forms via `formular`: simple surveys require substantial boiler-plate which `formular` cuts down on. 
   + the current version only implements few question types, more need to be added and integration with bots tested
- Move step transitions to the study level:
   + Right now transitions have to be added to individual steps, which requires updating steps when updating transitions. We are moving transitions out to the study level, which then takes care of updating the steps. The benefit is that this puts all the information of transitions at the top level and makes the paths through the study obvious, without intermingling it with other functionality of steps.
   + Once implemented, create a library for common randomizations, treatments, and other transition logic -- including constraints that need to be enforced
- Simplify process of extracting data or create general scripts (currently in R, see `data-tools/`) to import data into external tools.
- Improve payment data and processing - e.g. for use with MTurk/prolific, which work best with specific forms of csv files
- Replication of specific runs: save config at specific point
   - the git sha (which identifies the specific version of the code) is currently stored on each data item, which makes it straightforward to know which version of the code was used when collecting the data
   + when using external libraries, a best practice is to pin a specific version of an external dependency by using specific github snapshots
   + Adding a package as dependency changes git repo, which is tracked as a git SHA
   + Document how to do this and maybe simplify the process through a config file
   + Test and simplify the deployment of congame apps to streamline replications for specific git SHAs
   + Reduce deployment times and bandwidth requirements
- Write documentation for congame, using existing studies
- Streamline and document how to integrate storing and using media files (audio, video, ...)
   + The functionality exists, but is not well-documented and would benefit from some additional helper functions
- Libraries of ready-made studies/steps/widgets/bots/bot preferences (see economic concepts)
   + The current studies implemented in congame highlight some patterns that are common across studies. We are refactoring some of these out into specific libraries to make it easier and faster to build studies that use common components.
- Improve debugging and administration of single studies:
    + Impersonator (exists): Feature allowing administrators to 'impersonate' normal users to help debugging
    + visualize studies
    + Record and replay functionality: allowing admin users to replay how a participant went through a study, seeing the same screen as the participant did at the time. Useful for debugging as well as for data analysis.
- Add mechanism to add meta-data to data: when data is missing, this can occur because an error in the software interrupted the flow, because no data was entered -- due to being optional, due to time-out, or due to dropping out of the study -- or because the data itself is generated from other data, but fails to satisfy some consistency requirements. It can be helpful to add such meta-data to data, so that later data analysts can take these reasons into account.
    + The current mechanism for `fail`ing can be used to record the nature of the failure (such as dropping out of the study due to failing a test)
- Connect with separate server for data crunching of results -- as well as for launching bots to crunch results. Uses R/Julia/... for data analysis and defining distribution of bots.
  + Allows bot testing of statistical tests, i.e. tests that hold for a population of bots (not necessarily individual bots). Maybe functionality should be available to study server as well -- but tests are hard to describe in Racket, so probably not.
    
# Planned Features

- Refactor core implementation and add test coverage for the core implementation
  + including performance tests and improvements
  + blue/green deployment (no down-time upon deployment)
- Domain-specific language (DSL) to create studies from existing widgets/studies/study-pieces at a higher level that does not require study designers to learn Racket
  + including randomizations and treatments
- Bots:
  + speeding up/parallelizing bots that rely on web browser
  + faster simulations by not relying on web browser for running study as bot - direct access to server
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
