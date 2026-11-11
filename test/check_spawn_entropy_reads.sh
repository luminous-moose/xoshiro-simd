#!/usr/bin/env bash
# Checks the README's claim that spawning domains reads no OS entropy: runs
# spawn_entropy_probe.exe with getrandom_enosys_shim.so preloaded (it logs every
# getrandom()/getentropy() call, then fails it so the /dev/urandom fallback
# supplies the bytes), once with no spawns and once with several, and requires
# the same number of calls. OCaml 5 only, and Linux and the BSDs only, like
# check_urandom_fallback.sh.
#
# Usage: check_spawn_entropy_reads.sh SHIM PROBE
set -euo pipefail

os=$(uname -s)
case "$os" in
Linux | FreeBSD | OpenBSD | NetBSD | DragonFly) ;;
*)
    echo "skip: check_spawn_entropy_reads.sh doesn't support $os"
    exit 0
    ;;
esac

shim="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
probe=$2

log=$(mktemp)
trap 'rm -f "$log"' EXIT

entropy_calls() {
    : >"$log"
    local out
    out=$(LD_PRELOAD="$shim" XOSHIRO_SHIM_LOG="$log" "$probe" "$1")
    if [ "$out" != ok ]; then
        echo "FAIL: spawn_entropy_probe.exe $1 printed: $out" >&2
        exit 1
    fi
    grep -c -e '^getrandom$' -e '^getentropy$' "$log" || true
}

baseline=$(entropy_calls 0)
if [ "$baseline" -lt 1 ]; then
    echo "FAIL: the shim saw no getrandom()/getentropy() calls, so" \
        "LD_PRELOAD is probably not taking effect and nothing was measured" >&2
    exit 1
fi
spawned=$(entropy_calls 4)
if [ "$spawned" -ne "$baseline" ]; then
    echo "FAIL: spawning 4 domains (each spawning one more) made" \
        "$spawned entropy calls, against $baseline with no spawns" >&2
    exit 1
fi
echo "ok: spawning 8 domains read no OS entropy on $os" \
    "($baseline calls with and without spawning)"
