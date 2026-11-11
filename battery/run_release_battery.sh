#!/bin/sh
# Runs the full pre-release statistical battery: one BigCrush and one
# BigPract run per xoshiro256 stream, --full where ocaml_battery.exe
# allows it (int64 and float24bulk; float53scalar/float53bulk error out on
# --full -- see validate_full in ocaml_battery.ml -- since their packed
# words already splice several draws' bits together, so a word-level
# reversal there wouldn't test what --full is for).
#
# All 8 runs are independent (fresh OS-entropy seed each, no shared
# state) and launched together in the background, meant for an 8-core
# machine to run all of them genuinely in parallel overnight. Each run's
# log captures the OS-entropy seed-recovery hex line ocaml_battery.exe
# prints, so any run can be replayed exactly later via --seed-hex.
#
# --small substitutes the small/smallpract batteries for big/bigpract, to
# smoke-test the script itself (arguments, parallelism, logging) in
# seconds/minutes instead of hours.
#
# Usage: ./run_release_battery.sh [--small]
set -eu
cd "$(dirname "$0")"

big_testu01=big
big_pract=bigpract
if [ "${1:-}" = "--small" ]; then
  big_testu01=small
  big_pract=smallpract
fi

make

mkdir -p results

exe=../_build/default/battery/ocaml_battery.exe
streams="int64 float53scalar float53bulk float24bulk"

run() {
  stream=$1
  battery=$2
  extra_args=$3
  out="results/results_${stream}_${battery}"
  echo "==> $stream $battery -> $out"
  # shellcheck disable=SC2086 # extra_args is a deliberate word-split flag list
  "$exe" "$stream" "$battery" $extra_args > "$out" 2>&1 &
}

for stream in $streams; do
  case "$stream" in
    int64 | float24bulk) full_flag="--full" ;;
    *) full_flag="" ;;
  esac
  run "$stream" "$big_testu01" "$full_flag"
  run "$stream" "$big_pract" "$full_flag"
done

wait
echo "all runs finished -- see battery/results/"
