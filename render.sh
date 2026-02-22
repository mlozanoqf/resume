#!/usr/bin/env bash
set -euo pipefail

# Clean previous output/assets to avoid stale-cache rendering issues
rm -rf index.html index_files

XDG_CACHE_HOME=/tmp QUARTO_CACHE_DIR=/tmp/quarto quarto render index.qmd
