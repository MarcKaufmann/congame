#!/usr/bin/env bash

set -euo pipefail

echo "Setting up installation catalogs..."
raco pkg config  \
     --installation \
     --set catalogs \
     https://download.racket-lang.org/releases/7.8/catalog/ \
     https://racksnaps.defn.io/built-snapshots/2020/10/01/catalog/ \
     https://racksnaps.defn.io/snapshots/2020/10/01/catalog/

echo "Setting up user catalogs..."
raco pkg config \
     --user \
     --set catalogs \
     https://download.racket-lang.org/releases/7.8/catalog/ \
     https://racksnaps.defn.io/built-snapshots/2020/10/01/catalog/ \
     https://racksnaps.defn.io/snapshots/2020/10/01/catalog/
