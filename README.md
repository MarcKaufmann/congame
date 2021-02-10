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

### Requirements

* You need [Racket] since this is a Racket application.
* You need [node] and [nvm] to build the assets.
* You need access to a couple local [Postgres] databases.  One named
  `congame` and the other `congame_tests`.  The latter is
  exercised by unit tests.

### First-time Setup

    $ nvm use && npm install && npm run build
    $ raco pkg install chief
    $ raco pkg install congame-core/   # install and build the core library and its deps
    $ raco pkg install congame/        # install and build the application and its deps
    $ raco pkg install congame-doc/    # install and build the docs and their deps
    $ raco pkg install congame-tests/  # install and build the tests and their deps

### Development environment

Copy `.env.default` to `.env`.  [chief] will automatically load the
variables defined in this file into the environment of the
subprocesses defined in the `Procfile` whenever it is run.

The app expects to be run behind an SSL terminated connection (for
example, behind an nginx instance using a self-signed cert), even for
local development .  You can disable this requirement by setting
`current-continuation-key-cookie-secure?` parameter to `#f` before the
application is started.

## Installing study packages

* The package's `info.rkt` needs to `(define congame-studies)` like so:

``` racket
(define congame-studies
  ;; Path to package, followed by the study provided by the package that you want to use
  '((congame-example-study/example consent-study)
    (congame-example-study/example simple-study)))
```

    $ raco pkg install congame-example-study/  # install and build the study package and its deps

Make sure to `touch` or otherwise modify `congame/studies/all.rkt`
every time you install a new study package to ensure that package is
visible to the web app.

## Updating study caches

Run `./bin/bust-study-caches.sh` after adding studies or bots to
`info.rkt` files within study packages.

## Running the app locally

    $ nvm use
    $ raco chief start

## Re-building the documentation

    $ raco setup --tidy --check-pkg-deps --unused-pkg-deps congame


[Postgres]: https://www.postgresql.org/
[Racket]: https://racket-lang.org/
[argon2]: https://www.argon2.com/
[chief]: https://github.com/Bogdanp/racket-chief
[node]: https://nodejs.org/en/
[nvm]: https://github.com/nvm-sh/nvm

## Acknowledgement

The creation of this software was supported by Central European University (CEU).

## License

    congame is licensed under the 3-Clause BSD license.

See [LICENSE](congame-doc/LICENSE).
