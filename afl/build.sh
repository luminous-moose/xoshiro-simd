#!/usr/bin/env bash
# Builds afl/harness, statically linking lib/xs256_*.c directly (no OCaml
# involved). Usage:
#
#   ./build.sh                       # plain clang + ASan/UBSan, for local
#                                     # smoke-testing without AFL installed
#   CC=afl-clang-fast ./build.sh     # real AFL instrumentation + ASan
#
# Run afterwards with:
#   afl-fuzz -i corpus -o findings -- ./harness @@
set -euo pipefail
cd "$(dirname "$0")"

LIB=../lib

case "$(uname -m)" in
  x86_64|i386|i686) simd_flags="-DXOSHIRO_SIMD_HAS_SSE2 -DXOSHIRO_SIMD_HAS_AVX2 -DXOSHIRO_SIMD_HAS_AVX512FVL" ;;
  aarch64|arm*)     simd_flags="-DXOSHIRO_SIMD_HAS_NEON" ;;
  *)                simd_flags="" ;;
esac

cc="${CC:-afl-clang-fast}"
if ! command -v "$cc" >/dev/null 2>&1; then
  echo "warning: '$cc' not found; falling back to plain clang." >&2
  echo "         (no AFL instrumentation -- fine for an ASan smoke test," >&2
  echo "         but afl-fuzz itself needs afl-clang-fast/afl-gcc-fast.)" >&2
  cc=clang
fi

# -ffp-contract=off matches lib/dune's common_cflags: with -march=native,
# clang would otherwise fuse Plain's `x * scale + offset` into an FMA on
# hosts that have one, and the fuzzed scale/offset values would then report
# mismatches against the SIMD backends (which multiply and add separately)
# that are not bugs.
set -x
"$cc" -O2 -g -march=native -ffp-contract=off $simd_flags -fsanitize=address,undefined \
  -I"$LIB" \
  harness.c \
  "$LIB"/xs256_common.c "$LIB"/xs256_dispatch.c \
  "$LIB"/xs256_core_plain_c.c \
  "$LIB"/xs256_core_sse2.c \
  "$LIB"/xs256_core_avx2.c \
  "$LIB"/xs256_core_avx512fvl.c \
  "$LIB"/xs256_core_neon.c \
  -o harness
