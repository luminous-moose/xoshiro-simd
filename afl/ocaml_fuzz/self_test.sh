#!/usr/bin/env bash
# Proves harness.ml's invariant checks actually fire on a real bug, instead
# of trusting a clean fuzz run against unmodified lib/ as sufficient
# evidence. Injects one known-bad edit into a scratch copy of the working
# tree (never touching the tracked lib/), rebuilds there with the afl profile,
# and asserts the harness reports the violation. Five scenarios, each in its
# own scratch copy:
#   1. an off-by-one in int_in_range (lib/rand_impl.ml), caught by the range
#      invariant check.
#   2. XS256_FLOAT_SCALE_THRESHOLD set too low (lib/xs256_common.h), so the
#      SIMD float32 range fills prescale a small scale into a subnormal and
#      lose bits; caught by the bulk range fill's comparison against Plain.
#   3. and 4. the "range is NaN" check deleted from float_range
#      (lib/rand_impl.ml) and from fill_float64_range_bigarray
#      (lib/rand_core.ml); min = max = infinity then gets through, and the
#      harness expects a raise.
#   5. float_range returning NaN for a valid range, caught by the explicit NaN
#      check (every ordering comparison against NaN is false, so the range
#      test alone would pass it).
#
# Uses a plain rsync copy rather than `git worktree`, since that only checks
# out committed content -- this needs to work against the current working
# tree even before afl/ocaml_fuzz/ itself is committed.
#
# Usage: ./self_test.sh
set -euo pipefail
cd "$(dirname "$0")/../.."  # repo root

declare -a worktrees=()
# The trap is registered before any mktemp runs; the array is empty until an
# assignment succeeds, so `rm -rf` can never see an empty/unset value (and
# "${worktrees[@]}" expands to nothing, not to a bare `rm -rf`). Every use of
# a worktree path below is quoted too.
trap 'if [ "${#worktrees[@]}" -gt 0 ]; then rm -rf "${worktrees[@]}"; fi' EXIT

# Copies the working tree to a scratch dir, applies one sed mutation to a file
# in it, builds the harness there, feeds it an input, and asserts it fails
# with a message matching a pattern. Uses a plain rsync copy rather than
# `git worktree`, since that only checks out committed content -- this needs
# to work against the current working tree even before it is committed.
#
# $1 label, $2 file to mutate (relative to the repo root), $3 a fixed string
# that must appear in it, $4 the sed expression to apply, $5 the python
# snippet body that writes the input file: it runs with `f` bound to the open
# binary file, `struct` imported, and must not contain single quotes. $6 the
# regex the harness output must match.
run_scenario() {
    local label="$1" file="$2" needle="$3" sed_expr="$4" input_py="$5" pattern="$6"

    local worktree
    worktree="$(mktemp -d)"
    worktrees+=("$worktree")

    rsync -a --exclude=_build --exclude=.git --exclude='afl/findings*' ./ "$worktree/"

    local target="$worktree/$file"
    if ! grep -qF -- "$needle" "$target"; then
        echo "self_test ($label): expected text '$needle' not found in $file -- layout changed?" >&2
        exit 1
    fi
    sed -i "$sed_expr" "$target"
    if cmp -s "$target" "./$file"; then
        echo "self_test ($label): the mutation did not change $file" >&2
        exit 1
    fi

    # --root is required, not just tidy: dune's own root search doesn't stop
    # at this worktree's dune-project -- with no dune-workspace file anywhere
    # above, it keeps climbing and, for a `mktemp -d` scratch dir under /tmp,
    # can fall back to an unrelated stray dune-project sitting in /tmp itself
    # (world-writable, so some other tool's leftover is always possible
    # there). Once dune treats that as root it scans everything beneath it,
    # including directories this user can't read, which turns into read
    # errors/warnings and a build that never produces this worktree's own
    # harness.exe.
    (cd "$worktree" && dune build --root "$worktree" --profile afl afl/ocaml_fuzz/harness.exe 2>&1)

    local harness="$worktree/_build/default/afl/ocaml_fuzz/harness.exe"

    # Written to a file rather than piped through a bash variable -- command
    # substitution truncates at embedded NUL bytes, which packed int64s are
    # full of.
    #
    # input_file is passed via the environment (INPUT_FILE), not spliced
    # directly into the Python source as a string literal -- splicing would
    # mean any shell/Python-special character in the path (a stray quote or
    # backslash) corrupts the script that runs. Not reachable today since
    # input_file is always "$worktree/self_test_input" and $worktree only
    # ever comes from mktemp -d, but there's no reason for this script's
    # safety to depend on that.
    local input_file="$worktree/self_test_input"
    INPUT_FILE="$input_file" python3 -c "
import os
import struct
with open(os.environ['INPUT_FILE'], 'wb') as f:
$input_py
"

    local out status
    set +e
    out="$("$harness" <"$input_file" 2>&1)"
    status=$?
    set -e

    if [ "$status" -eq 0 ]; then
        echo "self_test FAILED ($label): harness did not detect the injected bug" >&2
        exit 1
    fi

    if ! grep -Eq -- "$pattern" <<<"$out"; then
        echo "self_test FAILED ($label): harness exited nonzero (status $status) but" >&2
        echo "not via the expected check (/$pattern/) -- output was:" >&2
        echo "$out" >&2
        exit 1
    fi

    echo "self_test OK ($label): harness correctly caught the injected bug (status $status)"
    echo "  $out"
}

# Off-by-one in int_in_range's result (lib/rand_impl.ml). With min = max,
# int_in_range takes the `full_int state 1` path; bound 1 has exactly one
# legal output (0), so the correct result is deterministically `min` -- no
# randomness involved, so this is a 100%-reproducible trigger rather than a
# probabilistic one. selector=4 (int_in_range), min=max=5.
run_scenario "int_in_range off-by-one" \
    lib/rand_impl.ml \
    '      r + min' \
    's/^      r + min$/      r + min + 1/' \
    "    f.write(bytes([4]) + struct.pack('<q', 5) + struct.pack('<q', 5))" \
    'int_in_range'

# XS256_FLOAT_SCALE_THRESHOLD (lib/xs256_common.h) 2^-102 -> 2^-110. The
# float32 SIMD range fills prescale the scale by 2^-24 while
# |scale| >= threshold, which is only exact while the product stays normal
# (|scale| >= 2^-102). selector=16 (fill_float32_range_bigarray), min=0,
# max=1.2345 * 2^-105, 100 elements: between the real and the mutated
# threshold, so the mutated build prescales into the subnormal range and
# drops mantissa bits, and the output no longer matches Plain. The scale is
# not a power of two on purpose: a power of two survives a subnormal
# prescale exactly. (Plain and the exact path implement the definition.)
run_scenario "float32 scale threshold too low" \
    lib/xs256_common.h \
    'define XS256_FLOAT_SCALE_THRESHOLD 0x1p-102f' \
    's/define XS256_FLOAT_SCALE_THRESHOLD 0x1p-102f/define XS256_FLOAT_SCALE_THRESHOLD 0x1p-110f/' \
    "    f.write(bytes([16]) + struct.pack('<d', 0.0) + struct.pack('<d', 1.2345 * 2.0**-105) + bytes([100]))" \
    'differs from Plain'

# The NaN-span check removed from float_range. selector=11 (float_range),
# min = max = +infinity: max - min is NaN, which passes the "range is
# infinite" ordering test, so without the dedicated check the call returns
# instead of raising. The harness's predicate expects a raise.
run_scenario "float_range NaN span check removed" \
    lib/rand_impl.ml \
    'invalid_arg "Xoshiro_simd.float_range: range is NaN";' \
    's/invalid_arg "Xoshiro_simd.float_range: range is NaN";/();/' \
    "    f.write(bytes([11]) + struct.pack('<d', float('inf')) + struct.pack('<d', float('inf')))" \
    'float_range: expected raise'

# The same check removed from fill_float64_range_bigarray (lib/rand_core.ml),
# which would hand a NaN scale to the C fill. selector=17, 16 elements.
run_scenario "fill_float64_range NaN span check removed" \
    lib/rand_core.ml \
    'invalid_arg "Xoshiro_simd.fill_float64_range_bigarray: range is NaN";' \
    's/invalid_arg "Xoshiro_simd.fill_float64_range_bigarray: range is NaN";/();/' \
    "    f.write(bytes([17]) + struct.pack('<d', float('-inf')) + struct.pack('<d', float('-inf')) + bytes([16]))" \
    'fill_float64_range_bigarray: expected raise'

# float_range returning NaN for a perfectly valid range (0, 1). NaN compares
# false against both bounds, so only the explicit is_nan check in the harness
# can see it.
run_scenario "float_range returns NaN" \
    lib/rand_impl.ml \
    '    r +. min' \
    's/^    r +\. min$/    r +. min +. Float.nan/' \
    "    f.write(bytes([11]) + struct.pack('<d', 0.0) + struct.pack('<d', 1.0))" \
    'float_range: nan out of'
