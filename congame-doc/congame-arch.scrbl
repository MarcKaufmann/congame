#lang scribble/manual

@title[#:style 'quiet]{Congame Configurations}

@section{Basic setup: single congame server}

The simplest setup consists of a single server, the congame server: participants sign up on the
server, the researcher deploys studies to it, and participants can choose to enroll in studies.

Since all the data is stored in the same database, the database owner can map the participant to the
email with which they signed up. Even if this email is an alias and does not allow inferring who the
participant is, it allows the database owner to merge all the information by this same participant.
This may allow to de-anonymize the person, or make inferences that the participant wasn’t aware
would be possible.

For example, a person may state personal information about their sexual orientation in one study
precisely because the study does not ask them about any other information. In a second study, that
same person may reveal their political inclinations. Yet, the participant might not have been
willing to state both their sexual orientation and their political inclination in the same study.
Since the database owner can combine the data, they can however easily infer this, which the
participant might not have wanted.

@section{Privacy-first setup}

For this reason, congame comes with an identity server that allows separation of enrolling in
studies and collecting data. In this setup, there is an additional identity server as well as an
SMTP proxy server with which the identity server receives messages sent by the congame server.

The general workflow is as follows: potential participants sign up with their email on the identity
server. The researcher deploys their study to their congame server. The researcher registers their
congame server with the identity server and creates an API user on their congame server. With this
API user, the identity server can pull any active studies and display them to potential participants
when signing up.

Currently, we then create a unique and anonymous display name, with which the user enrolls in the
given studies.

In the future, the goal is the following: When a participant signs up for a study on the identity
server, the identity server creates a new ID on the congame server for this particular study and
forwards the user to the study. If that same participant enrolls in a second study on this congame
server, then the identity service creates another new identity for them on the congame server. The
owner of the database on the congame server has thus no way of knowing that these two participants
are one and the same person, and thus cannot merge their data. Moreover, the owner of the identity
server does not have access to data on the congame server, and thus can also not merge the data.
They only know that the person participated in both studies.

While the same person may of course run both servers and hence merge the data, in principle this
provides a solution to this particularly aspect of privacy.

Since in many studies it is crucial to send messages to participants, the identities created by the
identity server are emails that get received by the identity server. When it receives a message for
a participant, it sends a notificaiton email to the participant that they received a message, which
they can see by logging in. In order for this to work, one needs to run an SMTP proxy which deals
with receiving emails from congame servers.
