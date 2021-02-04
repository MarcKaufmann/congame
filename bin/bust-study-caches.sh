#!/usr/bin/env bash

set -euo pipefail

ROOT="$(dirname "$0")/.."
PKGS=$(while IFS= read -r p; do
    basename "$(dirname "$p")"
done < <(find "$ROOT" -type f -name "info.rkt") | xargs)

rm -r "$ROOT/congame/studies/compiled"
raco setup --pkgs $PKGS
