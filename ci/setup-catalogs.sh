#!/usr/bin/env bash

set -euo pipefail

echo "Setting up user catalogs..."
raco pkg config \
     --user \
     --set catalogs \
     https://download.racket-lang.org/releases/8.1/catalog/ \
     https://racksnaps.defn.io/snapshots/2021/11/11/catalog/
