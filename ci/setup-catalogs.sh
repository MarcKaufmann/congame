#!/usr/bin/env bash

set -euo pipefail

echo "Setting up user catalogs..."
raco pkg config \
     --user \
     --set catalogs \
     https://download.racket-lang.org/releases/7.9/catalog/ \
     https://racksnaps.defn.io/built-snapshots/2020/11/12/catalog/ \
     https://racksnaps.defn.io/snapshots/2020/11/12/catalog/
