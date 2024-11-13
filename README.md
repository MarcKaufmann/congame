# congame

congame is *the best experimental economics software written in Racket*. [zTree](https://www.ztree.com/) and [oTree](https://www.otree.org/) are two common experimental economics platforms. The reason for building congame is the usual: I used oTree for some of my experiments, but found that it lacked some specific features I needed in my experiments:

1. It is tedious and errorprone to have multi-part studies across several days or weeks
2. It is tedious and errorprone to reuse and compose substudies into larger studies
3. Specification of the experimental study are interweaved with code that deals with web specific concerns

congame addresses the first concern fully, and eases the other two -- at least for the kinds of studies I run. Unsurprisingly, congame lacks many features of oTree (and other behavioral research software):

- There is no easy way for studies requiring multiple participants to interact with each other or respond to each other
- There are almost no templates for many existing tasks and designs

Furthermore, congame has a few features that many will consider bugs:

- It is written in [Racket](https://racket-lang.org/)
- It is build on top of the [koyo web development kit](https://koyo.defn.io/) developed by the indefatiguable (except for lack of coffee) [Bogdan Popa](https://defn.io/)
- There is precisely zero documentation (beyond the code itself)

koyo is much less battle tested than [Django](https://www.djangoproject.com/), the Python framework on which oTree is built, and Racket is a very uncommon choice compared to Python. This means that at this stage, it is fairly unlikely that you will want to use congame.

## Future Vision

The vision is to develop congame further, in particular as follows:

- Provide easy ways to implement strategic interaction between participants, such as prisoner's dilemma etc
- Ensure easy ways to change and test treatments and randomization in the study in a way that is entirely independent from the web aspects of the studies
- Allow for bots, in addition to users, to participate. This will benefit with simulations, debugging, etc - similar to [mTree](https://github.com/gmucsn/mtree)

The features that will get implemented will however be determined by the priorities of the studies that I am actually running -- hence strategic interactions may never be implemented, since my studies don't require that.

## Setup

Congame consists of:

* Server applications and libraries for developing and hosting studies
* Conscript, a scripting language for authoring studies on your local computer

## Setting up Conscript with local dev server

You need [Racket] since this is a Racket application. 

Clone this repository, then from its main folder, run:

    $ raco pkg install congame-core/ congame-web/ congame-cli/ congame-doc/ conscript/

This will install only the core libraries needed to run `#lang conscript` programs locally.

To spin up a local Congame server (needed for testing studies):

1. Install the Docker desktop app for your OS and start it up.

2. In the terminal, go to the root of the repository, and run the command `docker compose up` (make
   sure you are on a fast and stable internet connection before you do this).

3. Wait perhaps 10–15 minutes while lots of things are being downloaded and installed.

4. If all goes well, then you have a server running on <http://localhost:5100>, so go to that url in
   your browser.

5. You can log in to the local dev server with email address `admin@congame.local` and password
   `admin`.

You can read the full documentation by running `raco docs congame`.

## Setting up a server environment

Below are rough instructions for setting up a Congame server outside of Docker.

* You need [Racket] since this is a Racket application.
* You need [node] and [nvm] to build the assets.
* You need access to three local [Postgres] databases: One named
  `congame`, one named `congame_identity`, and the other `congame_tests`. The latter is
  exercised by unit tests.

```sql
$ psql postgres
postgres=# CREATE USER congame WITH PASSWORD 'congame';
postgres=# ALTER ROLE congame SET client_encoding TO 'utf8';
postgres=# ALTER ROLE congame SET default_transaction_isolation TO 'read committed';
postgres=# CREATE USER congame_identity WITH PASSWORD 'congame_identity';
postgres=# ALTER ROLE congame_identity SET client_encoding TO 'utf8';
postgres=# ALTER ROLE congame_identity SET default_transaction_isolation TO 'read committed';
postgres=# CREATE DATABASE congame;
postgres=# CREATE DATABASE congame_tests;
postgres=# CREATE DATABASE congame_identity;
postgres=# GRANT all PRIVILEGES ON DATABASE congame TO congame;
postgres=# GRANT all PRIVILEGES ON DATABASE congame_tests TO congame;
postgres=# GRANT all PRIVILEGES ON DATABASE congame_identity TO congame_identity;
postgres=# \q
```

### First-time Setup

    $ nvm use && npm install && npm run build
    $ raco pkg install chief                 # install and build chief to launch web server

    $ raco pkg install congame-core/         # install and build the core library and its deps
    $ raco pkg install congame-identity/     # install and build the application and its deps
    $ raco pkg install congame-smtp-proxy    # install and build the smtp-proxy dispatching traffic 
                                             # to the right identity server (staging or production)
    $ raco pkg install conscript/            # install and build #lang conscript and dialects
    $ raco pkg install congame-web/          # install and build the application and its deps
    $ raco pkg install studies/              # install studies used in congame-example-study
    $ raco pkg install congame-example-study # install example studies; needed for congame-doc
    $ raco pkg install congame-doc/          # install and build the docs and their deps

    # To install a package or study that tests depend on (e.g. congame-pjb-studies),
    # uncomment the following line and replace by appropriate package.
    # raco pkg install congame-price-lists/  # needed by congame-pjb-studies
    # raco pkg install congame-pjb-studies/

    $ raco pkg install congame-tests/  # install and build the tests and their deps

### Development environment

Copy `.env.default` to `.env`.  [chief] will automatically load the
variables defined in this file into the environment of the
subprocesses defined in the `Procfile` whenever it is run.

The app expects to be run behind an SSL terminated connection (for
example, behind an nginx instance using a self-signed cert), even for
local development. You can disable this requirement by setting
`current-continuation-key-cookie-secure?` parameter to `#f` before the
application is started (do this in `congame-web/components/app.rkt`).

### Running the app locally

    $ nvm use
    $ raco chief start

By default the app will run on `localhost:5100`.

### Adding an admin user

While running the app, browse to `http://localhost:5100/secret-signup`, and provide a username and
password.

In the terminal where you ran the app, look for a line of log output that looks like this:

```
[...] mail-adapter: templated email added to outbox [...] (action_url . "http://127.0.0.1:5100/verify/1/8b781[...]9475" ...
```

Copy and paste the value following `action_url` into your browser: this will “verify” your
username/email, allowing you to log in.

To make this user an admin user (you can do this while the app is running):

    $ psql congame
    congame=# update users set roles = '{user,admin}';

(Be sure to add a `WHERE` clause if you have more than one user.)

Refresh the app in your browser and you should see an "Admin" link in the header.

### Installing study packages

The study package's `info.rkt` needs to `(define congame-studies …)` like so:

``` racket
(define congame-studies
  ;; Path to package, followed by the study provided by the package that you want to use
  '((congame-example-study/example consent-study)
    (congame-example-study/example simple-study)))
```

Be sure to install the study package:

    $ raco pkg install congame-example-study/  # install and build the study package and its deps

To get congame to pick up the new package, run

    rm -r congame-web/studies/compiled/

then

    touch congame-web/studies/all.rkt

This will cause the "all.rkt" module to be re-compiled, and that will
in turn make the new study available in the web app.

### Updating study caches

Run `./bin/bust-study-caches.sh` after adding studies or bots to
`info.rkt` files within study packages.

## Re-building the documentation

    $ raco setup --tidy --check-pkg-deps --unused-pkg-deps congame


[Postgres]: https://www.postgresql.org/
[Racket]: https://racket-lang.org/
[argon2]: https://www.argon2.com/
[chief]: https://github.com/Bogdanp/racket-chief
[node]: https://nodejs.org/en/
[nvm]: https://github.com/nvm-sh/nvm

## Deployment Recommendation

See `ci/README.md`.

## Removing Congame

    raco pkg remove congame-core congame-doc congame-example-study congame-identity congame-pjb-studies congame-price-lists congame-smtp-proxy congame-tests congame-web conscript studies

## Acknowledgement

The creation of congame was supported by Közép-Európai Egyetem and Central European University Private University.

## License

    congame is licensed under the 3-Clause BSD license.

See [LICENSE](congame-doc/LICENSE).
