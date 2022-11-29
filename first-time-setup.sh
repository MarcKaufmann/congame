#!/usr/bin/env bash
raco pkg install chief                # install and build chief to launch web server
raco pkg install congame-core/        # install and build the core library and its deps
raco pkg install congame-identity/    # install and build the application and its deps
raco pkg install congame-smtp-proxy/  # install and build the smtp-proxy dispatching traffic 
                                      # to the right identity server (staging or production)
raco pkg install congame-web/         # install and build the application and its deps
raco pkg install studies/             # install and build common studies1:w
raco pkg install congame-doc/         # install and build the docs and their deps
raco pkg install congame-price-lists/ # install and build price lists, needed by pjb-studies, needed by tests
raco pkg install congame-pjb-studies/ # install and build this study, which is required for tests
raco pkg install congame-tests/       # install and build the tests and their deps

