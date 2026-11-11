#!/bin/sh
# Structural check on lib/dune itself: every (foreign_stubs ...) stanza
# should thread XOSHIRO_SIMD_EXTRA_CFLAGS through via
# (:include user_extra_cflags.sexp) in its flags list -- a missing or
# misspelled :include in any one of them would silently mean that backend
# never receives the extra flags, and nothing else would catch it (the
# env-var-wiring check above only proves the *file* is generated correctly,
# not that every backend actually reads it).
#
# Counts rather than hardcoding a specific number, so this doesn't need
# updating whenever a backend is added or removed -- it just needs the two
# counts to keep matching.
set -eu

dune_file=$1

stanza_count=$(grep -c '(foreign_stubs' "$dune_file")
include_count=$(grep -c '(:include user_extra_cflags\.sexp)' "$dune_file")

if [ "$stanza_count" -ne "$include_count" ]; then
  echo "error: $dune_file has $stanza_count (foreign_stubs ...) stanza(s) but only $include_count (:include user_extra_cflags.sexp) reference(s) -- some backend is missing the XOSHIRO_SIMD_EXTRA_CFLAGS passthrough." >&2
  exit 1
fi

echo "ok: all $stanza_count foreign_stubs stanza(s) in $dune_file include user_extra_cflags.sexp"
