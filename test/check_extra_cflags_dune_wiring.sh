#!/bin/sh
# Verifies the *dune wiring* around XOSHIRO_SIMD_EXTRA_CFLAGS: whether
# lib/dune's %{env:XOSHIRO_SIMD_EXTRA_CFLAGS=} substitution actually
# resolves the right variable and threads it through to the real,
# dune-generated user_extra_cflags.sexp. Computes the expected value
# independently in shell here, rather than sharing logic with lib/dune's own
# rule, so a bug in that rule (e.g. a typo'd env var name, which would
# silently always resolve to empty) has an independent check to catch it.
set -eu

# sexp_file is expected to already exist, not generated here: the test/dune
# rule that runs this script lists it as a dep, so dune builds it (via
# lib/dune's own rule) before invoking this script. Running this script
# directly, outside of dune, needs a prior `dune build` for that file to exist.
sexp_file=$1
actual=$(cat "$sexp_file")

if [ -n "${XOSHIRO_SIMD_EXTRA_CFLAGS:-}" ]; then
  expected="($XOSHIRO_SIMD_EXTRA_CFLAGS)"
else
  expected="()"
fi

if [ "$actual" != "$expected" ]; then
  echo "error: $sexp_file does not reflect XOSHIRO_SIMD_EXTRA_CFLAGS=${XOSHIRO_SIMD_EXTRA_CFLAGS:-<unset>}" >&2
  echo "  expected: $expected" >&2
  echo "  actual:   $actual" >&2
  echo "check lib/dune's %{env:XOSHIRO_SIMD_EXTRA_CFLAGS=} rule." >&2
  exit 1
fi

echo "ok: $sexp_file correctly reflects the ambient XOSHIRO_SIMD_EXTRA_CFLAGS"
