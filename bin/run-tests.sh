#!/usr/bin/env bash

set -euo pipefail

Xvfb :5 -ac &
export DISPLAY=:5
export PLTSTDERR='error debug@marionette'
raco test \
     /src/congame/congame-pjb-studies \
     /src/congame/congame-tests
