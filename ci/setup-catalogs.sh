#!/usr/bin/env bash

set -euo pipefail

raco pkg config --set catalogs \
    https://download.racket-lang.org/releases/7.8/catalog/ \
    https://racksnaps.defn.io/built-snapshots/2020/10/01/catalog/ \
    https://racksnaps.defn.io/snapshots/2020/10/01/catalog/
