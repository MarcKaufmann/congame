#!/usr/bin/env bash

set -euo pipefail

export PATH="/usr/bin:$PATH"

Xvfb :5 -ac &
export DISPLAY=:5
/opt/congame/bin/congame-web
