#!/usr/bin/env bash

set -euo pipefail

echo "Setting up user catalogs..."
raco pkg config \
     --user \
     --set catalogs \
     https://download.racket-lang.org/releases/8.11/catalog/ \
     https://racksnaps.defn.io/snapshots/2024/03/02/catalog/
