#!/usr/bin/env bash

set -euo pipefail

echo "Setting up user catalogs..."
raco pkg config \
     --user \
     --set catalogs \
     https://download.racket-lang.org/releases/8.4/catalog/ \
     https://racksnaps.defn.io/snapshots/2022/03/17/catalog/
