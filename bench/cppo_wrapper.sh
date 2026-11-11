#!/bin/sh
# Runs cppo, defining GSL_AVAILABLE (for #ifdef) only when dune's own
# %{lib-available:gsl} says true. A separate script, not an inline
# `bash -c "..."` dune action, because dune has to flatten an action into
# a single `-pp <command>` string for merlin/ocaml-lsp, and nested
# double-quotes inside an inline string don't survive that flattening.
set -eu
if [ "$1" = true ]; then
  exec cppo -D GSL_AVAILABLE "$2"
else
  exec cppo "$2"
fi
