#!/usr/bin/env bash

set -euo pipefail

Xvfb :5 -ac &
export DISPLAY=:5
raco test /src/congame
