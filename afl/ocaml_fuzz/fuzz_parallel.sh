#!/bin/bash
# Runs the OCaml fuzz harness under afl-fuzz on several CPUs at once, using
# afl++'s own parallel mode: one main instance (-M main) and JOBS-1 secondary
# instances (-S s1 ...) sharing one output directory, which they sync through
# (each periodically imports the others' new queue entries). Each instance
# gets its own CPU (afl-fuzz picks a free one itself) and its own log.
#
#   Usage:  fuzz_parallel.sh [JOBS]        (default: one per logical CPU)
#           fuzz_parallel.sh --summary     (just print crashes/hangs found)
#   Also:   make fuzz-ocaml-parallel [JOBS=16]    from afl/
#
# Environment variables (all optional):
#   EXIT_ON_TIME  seconds without new coverage after which an instance stops by
#                 itself (AFL_EXIT_ON_TIME); default 3600. The script ends when
#                 the last instance has stopped. 0 turns it off. An hour of 8
#                 instances is about 13M executions, which is what the whole
#                 14.5 h single-instance run needed to reach its last find; 20
#                 minutes was short enough to stop before a late find.
#   EXIT_WHEN_DONE  1 (the default) also sets AFL_EXIT_WHEN_DONE for every
#                 instance, as `make fuzz-ocaml` does. afl-fuzz only acts on it
#                 after more than 100 consecutive queue cycles without a new
#                 find (it was never reached in a 200 s test on a trivial
#                 target, and a queue cycle of the real corpus takes tens of
#                 minutes), and every instance has to get there itself, so
#                 more instances do not make it come sooner. EXIT_ON_TIME is
#                 the one that ends a run in practice. 0 turns it off.
#   FUZZ_SECONDS  also stop every instance after this long (afl-fuzz -V)
#   OUT           output directory (default findings_parallel here; ignored by
#                 git like findings*/). If it already has a main/ in it the
#                 run resumes from the saved queues instead of starting over.
#   CORPUS, HARNESS  input corpus and harness executable, for testing the
#                 script itself (HARNESS skips the rebuild)
#
# Ctrl-C (or FUZZ_SECONDS running out) stops all of them and prints a summary.
# While it runs, from another terminal:   afl-whatsup -s <OUT>
#
# Each instance is limited by the afl-persistent stop/continue handshake with
# afl-fuzz (about 250-500 executions per second even for a harness that does
# nothing), not by the CPU, so the total scales with the number of instances.
# A laptop will still heat up and slow down under 16 of them: the throughput at
# the end of an hour is lower than at the start. With EXIT_WHEN_DONE the
# instances finish at different times, so the rate drops towards the end.
#
# What a finding means: crashes/ hold inputs where the harness reported a
# contract violation (its `bug` function raises), hangs/ hold inputs that ran
# past the timeout (a function that never returns, such as float_strict with
# an infinite bound if its check were lost). Decode one with
#   _build/default/afl/ocaml_fuzz/decode.exe < FILE
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
out=${OUT:-$here/findings_parallel}
harness=${HARNESS:-$root/_build/default/afl/ocaml_fuzz/harness.exe}
decode="$root/_build/default/afl/ocaml_fuzz/decode.exe"
corpus=${CORPUS:-$here/corpus}

summary() {
  echo "== instances in $out (is the search finished? look at 'last new coverage')"
  local f
  for f in "$out"/*/fuzzer_stats; do
    [ -f "$f" ] || continue
    awk -F' *: *' -v n="$(basename "$(dirname "$f")")" '
      $1 == "run_time" { rt = $2 } $1 == "execs_done" { ex = $2 }
      $1 == "edges_found" { ed = $2 } $1 == "cycles_done" { cy = $2 }
      $1 == "last_find" { lf = $2 } $1 == "last_update" { lu = $2 }
      END {
        last = (lf == 0) ? "none" : sprintf("%.1f min before the end", (lu - lf) / 60)
        printf "  %-6s ran %6.1f min  execs %9d  edges %4d  cycles %4d  last new coverage: %s\n", n, rt / 60, ex, ed, cy, last
      }' "$f"
  done
  echo "== findings in $out"
  local kinds="crashes hangs" k n
  for k in $kinds; do
    n=$(find "$out" -path "*/$k/id:*" -type f 2>/dev/null | wc -l)
    echo "$k: $n"
    find "$out" -path "*/$k/id:*" -type f 2>/dev/null | head -5 | while read -r f; do
      echo "  $f"
      [ -x "$decode" ] && "$decode" <"$f" 2>&1 | head -1 | sed 's/^/    /'
    done
  done
}

if [ "${1:-}" = "--summary" ]; then
  summary
  exit 0
fi

jobs=${1:-$(nproc)}
if ! command -v afl-fuzz >/dev/null 2>&1; then
  echo "fuzz_parallel.sh: afl-fuzz not found (install the afl++ package)" >&2
  exit 1
fi

# Always rebuild: the harness is only useful if it matches the current lib/.
if [ -z "${HARNESS:-}" ]; then
  (cd "$root" && dune build --profile afl ./afl/ocaml_fuzz/harness.exe ./afl/ocaml_fuzz/decode.exe)
fi

# Same environment as the single-instance `make fuzz-ocaml` (see afl/Makefile),
# plus no TUI: sixteen of them cannot share a terminal.
export AFL_SKIP_CPUFREQ=1
export AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1
export AFL_NO_UI=1
if [ "${EXIT_WHEN_DONE:-1}" != "0" ]; then export AFL_EXIT_WHEN_DONE=1; fi
exit_on_time=${EXIT_ON_TIME:-3600}
if [ "$exit_on_time" != "0" ]; then export AFL_EXIT_ON_TIME="$exit_on_time"; fi

if [ -d "$out/main" ]; then
  input=-
  echo "resuming from $out"
else
  input="$corpus"
fi
vflag=()
if [ -n "${FUZZ_SECONDS:-}" ]; then vflag=(-V "$FUZZ_SECONDS"); fi

# Different power schedules spread the instances over different strategies
# (afl++ recommends this); the main instance keeps the default.
schedules=(explore fast coe lin quad exploit rare)

mkdir -p "$out/logs"
pids=()
stop() {
  trap - INT TERM
  echo
  echo "stopping ${#pids[@]} instances..."
  kill "${pids[@]}" 2>/dev/null || true
  wait 2>/dev/null || true
  summary
  exit 0
}
trap stop INT TERM

# CPU order for pinning: one thread of every physical core first, then the SMT
# siblings. afl-fuzz's own choice is the lowest free CPU ids, and on a CPU that
# numbers siblings next to each other (0-1, 2-3, ...) that puts the first
# JOBS instances on JOBS/2 cores. Without lscpu (not Linux) afl-fuzz chooses.
cpus=()
if command -v lscpu >/dev/null 2>&1; then
  mapfile -t cpus < <(lscpu -p=CPU,CORE,SOCKET 2>/dev/null | awk -F, '
    /^#/ { next }
    { k = $2 "," $3; if (!(k in seen)) { seen[k] = 1; f[++n] = $1 } else r[++m] = $1 }
    END { for (i = 1; i <= n; i++) print f[i]; for (i = 1; i <= m; i++) print r[i] }')
fi
bind_for() { # bind_for INDEX -> the afl-fuzz -b option for that instance, or nothing
  if ((${#cpus[@]} > $1)); then echo "-b ${cpus[$1]}"; fi
}

# shellcheck disable=SC2046  # bind_for's output is deliberately word-split
afl-fuzz -M main $(bind_for 0) -i "$input" -o "$out" "${vflag[@]}" -- "$harness" \
  >"$out/logs/main.log" 2>&1 &
pids+=($!)
for ((i = 1; i < jobs; i++)); do
  sched=${schedules[$(((i - 1) % ${#schedules[@]}))]}
  # shellcheck disable=SC2046
  afl-fuzz -S "s$i" $(bind_for "$i") -p "$sched" -i "$input" -o "$out" "${vflag[@]}" -- "$harness" \
    >"$out/logs/s$i.log" 2>&1 &
  pids+=($!)
done

echo "started $jobs afl-fuzz instances, output in $out (logs in $out/logs)"
echo "status: afl-whatsup -s $out      stop: Ctrl-C here"
wait "${pids[@]}" || true
trap - INT TERM
summary
