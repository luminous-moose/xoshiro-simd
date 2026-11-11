#!/usr/bin/env bash
# Proves harness.c's differential check actually fires on a real bug,
# instead of trusting a clean run against unmodified lib/ as sufficient
# evidence. Runs two independent scenarios, each injecting one known-bad
# edit into its own scratch copy of lib/, rebuilding against it, and
# asserting the harness aborts:
#   1. the ++ ("xoshiro256++") scrambler, used only by xs256_block_fn's
#      bitstream output.
#   2. the +  ("xoshiro256+")  scrambler, shared by xs256_double_block_fn
#      (doublestream) and all four xs256_fill_*_ba_fn functions -- they all
#      draw through the same xs256plus_scalar_next helper in
#      xs256_core_plain_c.c, so one mutation there exercises all five of
#      those outputs at once. Because harness.c's checks are one `||` chain
#      evaluated left-to-right, this scenario's abort will be attributed to
#      whichever of those five checks comes first (doublestream, currently),
#      not to all five independently -- see run_mutation_self_test's own
#      comment below for why that's still the right amount of rigor here.
#
#   3. XS256_FLOAT_SCALE_THRESHOLD set too low, so a float32 scale between the
#      real and the mutated threshold takes the prescaled fast path with a
#      subnormal prescaled scale and loses bits. Needs the fuzzed scale/offset
#      fields of the input (a fixed scale of 2.0 never gets near it).
#   4. The same for XS256_DOUBLE_SCALE_THRESHOLD, which only AVX512FVL's
#      float64 range fill consults; skipped if this host cannot build that
#      backend.
# A threshold set too HIGH is not covered and cannot be: the exact path is
# bit-identical to the fast one, so the mistake only costs speed.
#
# Usage: ./self_test.sh
set -euo pipefail
cd "$(dirname "$0")"

cc="${CC:-clang}"

case "$(uname -m)" in
  x86_64|i386|i686) simd_flags="-DXOSHIRO_SIMD_HAS_SSE2 -DXOSHIRO_SIMD_HAS_AVX2 -DXOSHIRO_SIMD_HAS_AVX512FVL" ;;
  aarch64|arm*)     simd_flags="-DXOSHIRO_SIMD_HAS_NEON" ;;
  *)                simd_flags="" ;;
esac

# Each xs256_core_*.c file's body is itself gated on the compiler's own ISA
# macros (__SSE2__/__AVX2__/etc, defined by -march=native below), independent
# of simd_flags above -- see build.sh. If -march=native won't define any of
# them on this host, PLAIN is the only backend that will actually compile in,
# and the differential check below has nothing to compare it against, so
# there's nothing this test can catch: skip cleanly instead of either
# failing to link (simd_flags claiming a backend the compiler won't produce)
# or reporting a false "did not detect the injected bug".
has_any_simd_macro=0
for macro in __SSE2__ __AVX2__ __AVX512F__ __ARM_NEON; do
  if echo | "$cc" -march=native -dM -E - 2>/dev/null | grep -q "define $macro "; then
    has_any_simd_macro=1
    break
  fi
done
if [ "$has_any_simd_macro" -eq 0 ]; then
    echo "self_test: no SIMD backend available on this host ($cc -march=native" >&2
    echo "  defines none of __SSE2__/__AVX2__/__AVX512F__/__ARM_NEON) -- only" >&2
    echo "  PLAIN would compile, and the differential check needs a second" >&2
    echo "  backend to compare it against. Skipping." >&2
    exit 0
fi

declare -a scratch_dirs=()
trap 'rm -rf "${scratch_dirs[@]}"' EXIT

# Builds a scratch copy of lib/ with one exact-text mutation applied, runs
# harness.c against it with a fixed seed+length input, and asserts it aborts
# via the "backend mismatch" path. $1: short label for messages. $2:
# file to mutate, relative to lib/. $3: exact text to replace (must appear
# in $2). $4: its replacement. $5 (optional): the input file's bytes as a
# printf format string, default the 9-byte seed+length input (which leaves
# scale/offset at harness.c's defaults). $6 (optional): compiler macros this
# scenario needs from `$cc -march=native`, space separated; the scenario is
# skipped if any is missing (the backend it targets would not be compiled).
#
# A single mutation-and-assert scenario proves the differential mechanism
# for whatever it perturbs isn't dead code -- not that every individual `||`
# clause in harness.c's comparison fires from its own dedicated bug. Getting
# that level of attribution (one mutation per clause) isn't proportionate
# here: xs256plus_scalar_next alone backs five different outputs
# (doublestream and all four fill_*_ba_fn results), so isolating each would
# need contriving five separate mutations, or reordering harness.c's checks
# per run, for a script whose actual job is just catching "this comparison
# quietly stopped checking anything."
run_mutation_self_test() {
    local label="$1" target="$2" old="$3" new="$4"
    # 8-byte seed + 1-byte length, matching INPUT_LEN in harness.c. 5
    # exercises a nonzero remainder against both the 8-element float32 block
    # and the 4-element double block.
    local input="${5:-\\x01\\x02\\x03\\x04\\x05\\x06\\x07\\x08\\x05}"
    local needs="${6:-}"

    local macro
    for macro in $needs; do
        if ! echo | "$cc" -march=native -dM -E - 2>/dev/null | grep -q "define $macro "; then
            echo "self_test SKIPPED ($label): $cc -march=native does not define $macro on this host"
            return 0
        fi
    done

    local scratch
    scratch="$(mktemp -d)"
    scratch_dirs+=("$scratch")

    cp ../lib/*.c ../lib/*.h "$scratch"/

    sed -i "s/$old/$new/" "$scratch/$target"
    if ! grep -qF "$new" "$scratch/$target"; then
        echo "self_test: failed to apply the $label mutation -- $target layout changed?" >&2
        exit 1
    fi

    "$cc" -O1 -g -march=native -ffp-contract=off \
      $simd_flags \
      -fsanitize=address,undefined \
      -I"$scratch" \
      harness.c \
      "$scratch"/xs256_common.c "$scratch"/xs256_dispatch.c \
      "$scratch"/xs256_core_plain_c.c \
      "$scratch"/xs256_core_sse2.c \
      "$scratch"/xs256_core_avx2.c \
      "$scratch"/xs256_core_avx512fvl.c \
      "$scratch"/xs256_core_neon.c \
      -o "$scratch"/harness_mutated

    printf "$input" > "$scratch"/seed_input

    set +e
    "$scratch"/harness_mutated "$scratch"/seed_input 2>"$scratch"/stderr
    local status=$?
    set -e

    if [ "$status" -eq 0 ]; then
        echo "self_test FAILED ($label): harness did not detect the injected bug" >&2
        exit 1
    fi

    if ! grep -q "backend mismatch" "$scratch"/stderr; then
        echo "self_test FAILED ($label): harness aborted (status $status) but not via" >&2
        echo "  the expected backend-mismatch path -- check stderr:" >&2
        cat "$scratch"/stderr >&2
        exit 1
    fi

    echo "self_test OK ($label): harness correctly caught the injected mismatch (status $status)"
    echo "  $(cat "$scratch"/stderr)"
}

# Corrupt xoshiro256++'s rotate constant in the scalar scrambler
# (lib/xs256_scalar_step.h, shared by xs256_core_plain_c.c and by jump()'s
# state-advance calls in xs256_common.c): xs256_scalar_rotl(s0 + s3, 23) ->
# ...24). jump() discards this function's return value (it only reads back
# the state array afterward), so this only changes PLAIN's actual bitstream
# output, not seed derivation for any backend.
run_mutation_self_test \
  "++ scrambler" \
  xs256_scalar_step.h \
  'xs256_scalar_rotl(s0 + s3, 23)' \
  'xs256_scalar_rotl(s0 + s3, 24)'

# Corrupt xoshiro256+'s scrambler in xs256plus_scalar_next
# (lib/xs256_core_plain_c.c): s0 + s3 -> s1 + s3. Same reasoning as above
# re: state advance being unaffected (this function replicates its own copy
# of the XOR/shift/rotl-45 state transition, independent of the `result`
# line being mutated here). Swapping in a different, effectively-unrelated
# state word is deliberate, not arbitrary: an additive perturbation like
# `+ 1` was tried first and rejected -- every consumer of this function's
# result immediately right-shifts away several low bits before use
# (doublestream's mantissa extraction discards the low 12, float32's 8-bit
# rescale discards 8 more on top of a 32-bit half-select), so a change
# confined to low bits only became visible on the rare carry that
# propagated past those shifts, passing or failing depending on how many
# draws (i.e. how large `length` was) happened to occur. Swapping the
# operand instead produces an effectively-random 64-bit value unrelated to
# the correct one, deterministically visible regardless of length or seed.
run_mutation_self_test \
  "+ scrambler" \
  xs256_core_plain_c.c \
  'const uint64_t result = s0 + s3;' \
  'const uint64_t result = s1 + s3;'

# Threshold mutations. The input is FULL_INPUT_LEN (33) bytes: seed
# 01..08, length 100 (twelve float32 blocks and a tail, a whole number of
# double blocks), then the fuzzed fields. The scale is deliberately not a
# power of two: a power of two prescales exactly even into the subnormal
# range, so the mutated (too-low) threshold would go unnoticed.
#
# float32: real threshold 2^-102, mutated to 2^-110. scale_f = 1.2345 * 2^-105
# sits between them, so the mutated build prescales it to a subnormal
# (1.2345 * 2^-129) and drops mantissa bits. The other fields are neutral:
# offset_f 0, scale_d 2.0 and offset_d -1.0 (harness.c's defaults).
run_mutation_self_test \
  "float32 scale threshold too low" \
  xs256_common.h \
  'define XS256_FLOAT_SCALE_THRESHOLD 0x1p-102f' \
  'define XS256_FLOAT_SCALE_THRESHOLD 0x1p-110f' \
  '\x01\x02\x03\x04\x05\x06\x07\x08\x64\x19\x04\x1e\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\xf0\xbf'

# float64: real threshold 2^-969, mutated to 2^-990. scale_d = 1.2345 * 2^-975
# is between them (prescaled: 1.2345 * 2^-1028, subnormal). Only AVX512FVL
# branches on this, so it needs that backend to be compiled in.
run_mutation_self_test \
  "float64 scale threshold too low" \
  xs256_common.h \
  'define XS256_DOUBLE_SCALE_THRESHOLD 0x1p-969' \
  'define XS256_DOUBLE_SCALE_THRESHOLD 0x1p-990' \
  '\x01\x02\x03\x04\x05\x06\x07\x08\x64\x00\x00\x00\x40\x00\x00\x80\xbf\x8d\x97\x6e\x12\x83\xc0\x03\x03\x00\x00\x00\x00\x00\x00\x00\x00' \
  "__AVX512F__ __AVX512VL__ __AVX512DQ__"
