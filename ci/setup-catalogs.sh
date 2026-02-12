#!/usr/bin/env bash

set -euo pipefail

echo "Setting up user catalogs..."
raco pkg config \
     --user \
     --set catalogs \
     "$(raco pkg config catalogs | head -1)" \
     https://racksnaps.defn.io/snapshots/2026/02/12/catalog/
raco pkg config catalogs
