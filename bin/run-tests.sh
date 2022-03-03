#!/usr/bin/env bash

set -euo pipefail

Xvfb :5 -ac &
export DISPLAY=:5
export PLTSTDERR='error debug@marionette'
raco test \
     congame-pjb-studies/ \
     congame-tests/
