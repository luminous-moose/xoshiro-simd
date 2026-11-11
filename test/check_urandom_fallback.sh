#!/usr/bin/env bash
# Exercises the /dev/urandom fallback in read_entropy (lib/xs256_common.c),
# which every supported Unix normally skips because getrandom()/getentropy()
# succeed first. Two runs of urandom_fallback_probe.exe:
#
# 1. With getrandom_enosys_shim.so preloaded: every getrandom() and
#    getentropy() call fails, so the fallback has to supply all the entropy.
# 2. With urandom_eintr_shim.so preloaded: the same, and in addition the first
#    read() from every /dev/urandom descriptor fails with EINTR, so the
#    fallback only works if read_urandom retries on EINTR.
#
# Runs on Linux, FreeBSD, OpenBSD, NetBSD and DragonFly, whose dynamic linkers
# all honour LD_PRELOAD. Elsewhere it prints why and exits 0: macOS would need
# DYLD_INSERT_LIBRARIES and dyld's __interpose section, and Windows has no
# fallback to test (it only uses BCryptGenRandom).
#
# Usage: check_urandom_fallback.sh SHIM.so EINTR_SHIM.so PROBE.exe
set -euo pipefail

os=$(uname -s)
case "$os" in
Linux | FreeBSD | OpenBSD | NetBSD | DragonFly) ;;
*)
    echo "skip: check_urandom_fallback.sh doesn't support $os"
    exit 0
    ;;
esac

# LD_PRELOAD wants a path the dynamic linker can open from the probe's own
# working directory, so make it absolute. Not realpath(1): it's missing from
# older NetBSD base systems.
absolute() {
    echo "$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
}
shim=$(absolute "$1")
eintr_shim=$(absolute "$2")
probe=$3

log=$(mktemp)
trap 'rm -f "$log"' EXIT

# The probe makes six entropy reads (three State.init, State.reseed, self_init,
# reseed), plus one more at startup on OCaml 4, where the module-level
# generator is created eagerly. Each one is a getrandom()/getentropy() call
# that the shims fail, and in run 2 also one injected EINTR.
expected_min=6

# Number of lines in the log equal to one of the given words. grep -c exits 1
# when it counts nothing, which isn't an error here.
count() {
    local pattern
    pattern=$(printf '^%s$\n' "$@")
    grep -c -e "$pattern" "$log" || true
}

check_entropy_calls() {
    local calls
    calls=$(count getrandom getentropy)
    if [ "$calls" -lt "$expected_min" ]; then
        echo "FAIL ($1): the shim saw $calls getrandom()/getentropy() calls," \
            "expected at least $expected_min; LD_PRELOAD is probably not" \
            "taking effect, so the fallback was not exercised" >&2
        exit 1
    fi
    echo "$calls"
}

# Run 1: the fallback on its own.
: >"$log"
if ! out=$(LD_PRELOAD="$shim" XOSHIRO_SHIM_LOG="$log" "$probe"); then
    echo "FAIL (fallback): urandom_fallback_probe.exe failed with getrandom()" \
        "and getentropy() disabled" >&2
    exit 1
fi
calls=$(check_entropy_calls fallback)
if [ "$out" != ok ]; then
    echo "FAIL (fallback): unexpected probe output: $out" >&2
    exit 1
fi
echo "ok: /dev/urandom fallback supplied all entropy on $os ($calls" \
    "getrandom()/getentropy() calls forced to fail)"

# Run 2: the fallback with one EINTR injected into each /dev/urandom read.
: >"$log"
if ! out=$(LD_PRELOAD="$eintr_shim" XOSHIRO_SHIM_LOG="$log" "$probe"); then
    if [ "$(count eintr)" -gt 0 ]; then
        echo "FAIL (EINTR): read_urandom gave up after an injected EINTR" \
            "instead of retrying the read" >&2
    else
        echo "FAIL (EINTR): urandom_fallback_probe.exe failed, but no EINTR" \
            "was injected, so the cause is something else" >&2
    fi
    exit 1
fi
calls=$(check_entropy_calls EINTR)
injected=$(count eintr)
if [ "$injected" -lt "$expected_min" ]; then
    echo "FAIL (EINTR): only $injected EINTRs were injected, expected at" \
        "least $expected_min; the shim's open/read interception is probably" \
        "missing the calls read_urandom makes on $os" >&2
    exit 1
fi
if [ "$out" != ok ]; then
    echo "FAIL (EINTR): unexpected probe output: $out" >&2
    exit 1
fi
echo "ok: read_urandom retried after $injected injected EINTRs on $os"
