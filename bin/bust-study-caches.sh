#!/usr/bin/env bash

set -euo pipefail

ROOT="$(dirname "$0")/.."
PKGS=$(while IFS= read -r p; do
    basename "$(dirname "$p")"
done < <(find "$ROOT" -maxdepth 2 -type f -name "info.rkt") | xargs)

rm -fr "$ROOT/congame-web/studies/compiled"
raco setup --pkgs $PKGS
