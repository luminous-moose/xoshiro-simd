#!/bin/sh
# Fails if the given archive exports any global defined symbol that is
# neither an OCaml-runtime symbol (caml_*) nor carries this project's
# xs256simd_/xs256simdintf_ prefix.
set -eu

# Skipped on macOS: Mach-O gives every C symbol a leading underscore
# (_xs256simd_..., _caml_...), which the patterns below don't allow for, and
# this has never been run on a Mac to check a fix.
if [ "$(uname -s)" = Darwin ]; then
  echo "skip: check_exported_symbols.sh doesn't support macOS"
  exit 0
fi

archive=$1

# __x86.get_pc_thunk.{ax,bx,cx,dx,si,di} (older gcc: __i686.get_pc_thunk.*)
# are compiler-synthesized, not project code: on i386 -fPIC, GCC emits these
# to fetch the PC into a register (x86_64 PIC uses RIP-relative addressing
# instead, so they never appear there). They show up here as GLOBAL/defined
# (hence caught by `nm -g`), but each is also HIDDEN visibility and placed in
# its own COMDAT group, which is precisely GCC's mechanism for letting many
# object files/static libraries define the identical helper without a
# multiple-definition error -- the collision this script otherwise guards
# against is already handled for these by the compiler and linker.
#
# .refptr.<name> (mingw-w64 x86-64 only) is the same kind of thing: a pointer
# slot GCC generates when code takes the address of a symbol defined in
# another object file (here backend_table's *_is_supported entries, and
# libgcc's __cpu_model), placed in its own COMDAT section so identical copies
# from different objects or libraries are merged by the linker.
#
# __retguard_<n> (OpenBSD only) is too: RETGUARD, OpenBSD's default
# return-address protection, makes clang emit one random cookie per protected
# function, hidden visibility, each in its own COMDAT section.
#
# OpenBSD's nm also lists file symbols (one per source file, e.g.
# "xs256_stubs.c") under -g, with type letter F. They name source files, not
# anything a linker resolves, so they are dropped by type.
#
# nm runs on its own line so set -e catches it failing: in a pipeline only the
# last command's status counts. The filtering is a single awk, not
# grep | grep | sort -u, for two reasons. grep -v exits 1 when it filters out
# everything, which is the passing case. And on mingw, `sort` can resolve to
# Windows' own sort.exe, which takes -u as a file name and fails; with the old
# `|| true` that failure was swallowed, the list came out empty, and this check
# passed on mingw without checking anything.
#
# Only POSIX's -g, not GNU's --defined-only: OpenBSD's nm doesn't have it (and
# rejecting it was swallowed the same way). It isn't needed: undefined symbols
# are printed without an address, so the awk's hex-address test on the first
# field already drops them, along with the per-member "file.o:" header lines.
symbols=$(nm -g "$archive")
bad=$(printf '%s\n' "$symbols" | awk '
  $1 ~ /^[0-9a-fA-F]+$/ {
    name = $NF
    if ($2 == "F" || $2 == "f") next
    if (name ~ /^(caml_|xs256simd_|xs256simdintf_)/) next
    if (name ~ /^__(x86|i686)\.get_pc_thunk\./) next
    if (name ~ /^\.refptr\./) next
    if (name ~ /^__retguard_[0-9]+$/) next
    if (!seen[name]++) print name
  }')

if [ -n "$bad" ]; then
  echo "error: unprefixed global symbol(s) exported from $archive:" >&2
  echo "$bad" | sed 's/^/  /' >&2
  echo "every cross-file C symbol must be either 'static' or carry the xs256simd_/xs256simdintf_ prefix." >&2
  exit 1
fi

echo "ok: all exported symbols in $archive are properly prefixed or caml_*"
