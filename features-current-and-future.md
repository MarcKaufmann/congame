# Completed Features

## Status Fall 2023

In the last year, we achieved the following:

- We created the first prototype of conscript, a domain-specific language (DSL) on top of congame that I used in one of my classes to let students code replications of existing studies. While it was usable by students, it lacked a lot of features that the students needed, it was not sufficiently simple for students to use by themselves, and it revealed several bugs that we needed fixing. Nonetheless, most students managed to code parts of existing studies up in conscript, showing the potential for an improved version that we are working on currently.
- `formular` improvements: 
  - elements of a form can now be randomized within a page
  - we added lots of default widgets (input-number etc)
  - we added ways to dynamically create specialized forms, such as tables with multiple columns, that act as inputs.
  - we created default behavior such that normal uses of formular require less code by storing all the values by default under separate names. We added additional functions to switch to other common behaviors.
- Documentation: 
  - We wrote some basic documentation for the central features of congame.
  - We have a separate page for the documentation of congame.
- Admin interface improvements:
  - We display a summary of each data object, which can be expanded to provide the details when needed. This reduces visual clutter when dealing with large objects, such as large hashes.
  - We now catch several types of errors so that the admin page doesn't fail to load when these occur.
- Testing:
  - We created many tests, including snapshot tests for conscript studies, to better catch many errors
- getting and putting data:
  - We created new variants of `get` and `put` that store values at the global level; at the global instance level; or at some other root (such as '*new-root*, instead of the default '*root*) to avoid name collisions and allow features to work well with each other.
- We fixed several big performance bugs, that slowed down the study dramatically:
  - One was identified by a student, literally grinding even minor studies to the ground (see https://github.com/MarcKaufmann/congame/commit/8cb69a0f869e0f49cd36579605326fbe00938361 for the resolution)
  - We catch errors in transition graphs, disallowing one step to have multiple follow-up targets, since each step can only have one well-defined step (which can be a step that transitions to one of several steps depending on which conditions prevails).
  - We found two other performance bugs that leaked memory, so that eventually the server had to be restarted to run. 
- Data extraction
  - We have now several scripts for extracting data, providing convenient templates for future data extractions.
- I wrote several studies in congame:
  - Several surveys that I used in class
  - One survey used for the economics department to test new names for its programme
  - Several experiments for research project
- Different study instances can now be linked so that they can share data between each other
- We allow dynamic (runtime) substudies, which allows simplifying and abstracting a lot of code.
  - We added `for/study`, which creates a new substudy consisting of the looped steps inside the for loop.
- We can use unpoly widgets to provide some element of dynamic behavior on the frontend page (see up-*)

## Status Fall 2022

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
- The server can now be connected to dbg, to monitor its memory and pcu usage, which helps in debugging.

# Plans

The main plan for now is to use congame even more in production; to update conscript based on the lessons learned from last year and all the code improvements since; and to document congame, conscript, and the current list of features.

Additional secondary goals are to improve the data export by creating functionality that simplifies the workflow; and to improve and better integrate defining, modeling, and analysing bots.

# List of Potential Features

This list of features does not represent a roadmap. These are features that came up, but many may not be desirable and create feature bloat.

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
