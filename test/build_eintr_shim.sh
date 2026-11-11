#!/usr/bin/env bash
# Builds urandom_eintr_shim.so for check_urandom_fallback.sh: both shims in one
# library, since one preload library behaves the same everywhere, while the
# separator for a list of several varies between dynamic linkers.
#
# dlsym needs -ldl with glibc before 2.34 (from 2.34 it's in libc and libdl is
# an empty stub, which still links, as it does with musl). The BSDs have dlsym
# in libc, and OpenBSD has no libdl at all, so -ldl is passed on Linux only.
#
# Usage: build_eintr_shim.sh OUT.so CC [CC-FLAGS...]
set -euo pipefail

out=$1
shift

libs=()
if [ "$(uname -s)" = Linux ]; then
    libs=(-ldl)
fi

# ${libs[@]+...}: bash before 4.4 treats an empty array as unset under set -u.
"$@" -shared -fPIC -o "$out" getrandom_enosys_shim.c urandom_eintr_shim.c \
    ${libs[@]+"${libs[@]}"}
