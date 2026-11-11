# harness links afl-persistent, not in xoshiro-simd.opam's depends --
# every stanza in this directory's dune is gated with
# `(enabled_if (= %{profile} afl))`, so a plain `dune build` from the
# project root never needs it. This Makefile just fronts the one-flag
# opt-in so `make` here is enough to remember, without editing the root
# `dune` file (see its own comment on why afl/ isn't dirs-excluded, and on
# why the (env (afl ...)) instrumentation of lib/ doesn't depend on this
# either).
#
# `dune build --profile afl` below runs with this directory as cwd, not
# the project root, but lib/ still gets instrumented: --profile is one
# global setting for the whole invocation, not scoped to cwd, and
# harness.exe's dependency on the xoshiro_simd library pulls lib/'s
# sources into the build regardless of which directory you ran dune from.
# Each lib/*.ml file then picks up -afl-instrument based on its own
# position in the directory tree (a descendant of the root's env stanza),
# not based on the invoking directory.
#
# Dependencies:
#   - opam: the deps listed in xoshiro-simd-afl.opam (ocaml, dune,
#     afl-persistent) -- `opam install ./xoshiro-simd-afl.opam --deps-only`
#     from the repo root.
#   - python3, rsync: used by ./self_test.sh (python3 to write a packed
#     binary test input via struct.pack; rsync to copy the working tree
#     into a scratch dir for the injected-bug scenario -- see that script's
#     own comments for why).
#   - afl-fuzz (the `afl++` package) is needed to actually run the fuzzer
#     (`make fuzz-ocaml` from ../Makefile), but not to build or self-test
#     here. Unlike the C harness (afl/build.sh), no afl-clang-fast/C
#     compiler wrapper is needed for the OCaml harness: ocamlopt's own
#     `-afl-instrument` flag (enabled via the `afl` dune profile, see
#     above) instruments OCaml code directly -- verified with `afl-showmap`
#     against a `--profile afl` build, which reported real, input-dependent
#     coverage tuples with no afl-clang-fast involved anywhere in the
#     build.

.PHONY: all clean
all:
	dune build --profile afl

clean:
	dune clean
