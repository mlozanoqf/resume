#!/usr/bin/env Rscript

# Keep rendering deterministic across RStudio button and shell script.
# Remove stale outputs/assets that can cause inconsistent TOC/sidebar behavior.
unlink("index.html", force = TRUE)
unlink("index_files", recursive = TRUE, force = TRUE)
unlink(file.path(".quarto", "project-cache"), recursive = TRUE, force = TRUE)
