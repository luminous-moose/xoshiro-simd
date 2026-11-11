#!/bin/sh
# Builds and runs the standard benchmark suite, saving each benchmark's output
# to results/results_<name>, so your numbers can be compared with the ones
# published here. Benchmarks that aren't built because they need a library that
# isn't installed are skipped with a message.
#
# An optional argument is a suffix for the result files, separated from the
# name with an underscore: ./run_benchmarks.sh mylaptop writes
# results/results_<name>_mylaptop. With no argument there is no suffix.
#
# --sleep SECONDS waits that long between the individual one-second
# measurements, so that a machine that heats up under load, such as a laptop,
# can cool down. Fractions are allowed; the default is no sleep. It lengthens
# the run by about SECONDS times the number of measurements (a few hundred per
# benchmark).
#
# Usage: ./run_benchmarks.sh [--sleep SECONDS] [suffix]
set -eu
cd "$(dirname "$0")"

run_sleep=""
if [ "$#" -ge 1 ] && [ "$1" = "--sleep" ]; then
  if [ "$#" -lt 2 ]; then
    echo "run_benchmarks.sh: --sleep needs a number of seconds" >&2
    exit 1
  fi
  run_sleep=$2
  shift 2
fi
case "$run_sleep" in
  *[!0-9.]* | . | *.*.*)
    echo "run_benchmarks.sh: --sleep needs a non-negative number of seconds, got '$run_sleep'" >&2
    exit 1
    ;;
esac
sleep_args=""
if [ -n "$run_sleep" ]; then
  sleep_args="--sleep $run_sleep"
fi

if [ "$#" -gt 1 ]; then
  echo "usage: $0 [--sleep SECONDS] [suffix]" >&2
  exit 1
fi

suffix=""
if [ "$#" -eq 1 ] && [ -n "$1" ]; then
  case "$1" in
    */*)
      echo "run_benchmarks.sh: suffix must not contain '/'" >&2
      exit 1
      ;;
    --*)
      echo "run_benchmarks.sh: unknown option '$1' (a suffix must not start with '--')" >&2
      exit 1
      ;;
  esac
  suffix="_$1"
fi

if ! command -v dune >/dev/null 2>&1; then
  echo "run_benchmarks.sh: dune not found in PATH" >&2
  exit 1
fi

benchmarks="bits30 bool float_backends float_libs float_strict_backends float_range_backends float_range_libs float_range_strict_backends intrange_backends intrange_libs fill_float32_range_backends fill_float32_range_libs fill_float64_range_backends fill_float64_range_libs fill_float64_bulk_vs_scalar"

mkdir -p results

for name in $benchmarks; do
  # Build first, on its own: the only expected failure is a benchmark
  # disabled in bench/dune because an optional dependency (owl) isn't
  # installed, which dune reports as "Don't know how to build". Anything
  # else is a real build error and stops the run.
  # The leading ./ is required: without a path separator, or with one but
  # no ./, dune looks the name up as an installed public name instead of a
  # path.
  if ! build_out=$(dune build --profile bench "./bench_${name}.exe" 2>&1); then
    case "$build_out" in
      *"Don't know how to build"*)
        case "$name" in
          fill_*_libs) missing="owl" ;;
          *) missing="an optional dependency" ;;
        esac
        echo "==> bench_${name}: skipping, not built ($missing isn't installed)"
        continue
        ;;
      *)
        printf '%s\n' "$build_out" >&2
        exit 1
        ;;
    esac
  fi

  out="results/results_${name}${suffix}"
  echo "==> bench_${name} -> $out"
  dune exec --profile bench "./bench_${name}.exe" -- $sleep_args | tee "$out"
done
