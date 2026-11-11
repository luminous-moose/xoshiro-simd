# Builds both fuzz harnesses that live under afl/ and proves both actually
# catch a real bug before you trust a clean fuzzing run:
#
#   - the C differential harness (harness.c, built by ./build.sh): links
#     lib/xs256_*.c directly, no OCaml involved, and compares the SIMD
#     backends against each other.
#   - the OCaml harness (ocaml_fuzz/harness.ml, built via dune under the
#     `afl` profile): exercises the bound/range contracts in
#     lib/rand_impl.ml that the C harness never touches, since it bypasses
#     OCaml entirely.
#
# `make` (or `make all`) -- build both harnesses, then run both self-tests
#                            (./self_test.sh and ocaml_fuzz/self_test.sh).
# `make fuzz`            -- run afl-fuzz against the C differential harness
#                            (corpus/findings in afl/). Rebuilds first with
#                            CC=afl-clang-fast so it's actually instrumented,
#                            not just the ASan smoke-test build `make harness`
#                            gives you when afl-clang-fast isn't on PATH.
# `make fuzz-ocaml`      -- run afl-fuzz against the OCaml harness
#                            (corpus/findings in ocaml_fuzz/).
# `make fuzz-ocaml-parallel [JOBS=16]`
#                          -- the same OCaml harness on several CPUs at once
#                            (afl++ main/secondary mode, shared output in
#                            ocaml_fuzz/findings_parallel; default one job per
#                            logical CPU). Each instance stops itself after
#                            EXIT_ON_TIME seconds (default 3600) without new
#                            coverage; FUZZ_SECONDS=N stops everything after N
#                            seconds, otherwise Ctrl-C. See
#                            ocaml_fuzz/fuzz_parallel.sh.
# `make clean`           -- remove build artifacts (leaves corpus/findings
#                            alone).
#
# Prerequisite for `make fuzz`/`make fuzz-ocaml`: afl-fuzz and
# afl-clang-fast on PATH (e.g. the `afl++` package). Without them,
# `make`/`make all` still works -- build.sh falls back to plain clang with
# a warning, fine for the ASan self-tests, just not for real fuzzing.
#
# `make`/`make all` (and `make self-test` on its own) have their own real
# prerequisites, separate from the fuzz-specific ones just above: building
# `ocaml-harness` needs dune/ocaml/afl-persistent (opam), and self-test
# needs python3 and rsync for ocaml_fuzz/self_test.sh's scratch-copy/input
# generation -- see ocaml_fuzz/Makefile's own comment for the full,
# checked list and why each one is needed. Not duplicated here since that
# file is the one place responsible for keeping it current.
#
# Both fuzz targets set:
#   AFL_SKIP_CPUFREQ=1               -- skip afl-fuzz's scaling-governor
#                                        check; harmless to leave unset on a
#                                        machine with "performance" governor
#                                        already, but avoids the abort on
#                                        one that doesn't.
#   AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1
#                                     -- both harnesses are built with
#                                        ASan/UBSan (see build.sh/dune's afl
#                                        profile), which afl-fuzz otherwise
#                                        refuses to fuzz without this set,
#                                        since a sanitizer's own crash
#                                        handler can shadow AFL's.
#   AFL_EXIT_WHEN_DONE=1              -- have afl-fuzz stop itself once a
#                                        full queue cycle turns up nothing
#                                        new, instead of running until
#                                        interrupted. Unset this (or run
#                                        afl-fuzz directly instead of via
#                                        `make fuzz`/`fuzz-ocaml`) for an
#                                        open-ended session.
export AFL_SKIP_CPUFREQ := 1
export AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES := 1
export AFL_EXIT_WHEN_DONE := 1

.PHONY: all harness ocaml-harness self-test fuzz fuzz-ocaml fuzz-ocaml-parallel clean

all: harness ocaml-harness self-test

# Deliberately phony even though build.sh really does produce a file
# literally named `harness` here -- this isn't a missing-dependency-list
# oversight. Two things rule out real file prerequisites (harness.c
# ../lib/*.c ../lib/*.h):
#   1. `all`/`harness` build with the default CC (falling back to plain
#      clang), while `fuzz` below explicitly overrides CC=afl-clang-fast --
#      same output filename, different toolchain. A file-list-based target
#      can't detect "CC changed" from source mtimes alone, so it would
#      risk silently serving a stale binary built with the wrong compiler.
#   2. build.sh's rebuild cost is small (~2s: one clang invocation over all
#      7 .c files, not an incremental multi-TU build with per-object-file
#      savings to preserve), so there's little to gain from real dependency
#      tracking here even setting (1) aside.
# Non-phony with no prerequisites would in fact be worse than always
# rebuilding: once `harness` exists, Make treats a target with an
# up-to-date-looking file and no prerequisites as satisfied permanently,
# so it would silently stop rebuilding at all, even after editing
# harness.c or any lib/ file -- verified empirically, not just reasoned
# about, with a throwaway Makefile.
harness:
	./build.sh

ocaml-harness:
	$(MAKE) -C ocaml_fuzz

self-test: harness ocaml-harness
	./self_test.sh
	./ocaml_fuzz/self_test.sh

fuzz:
	CC=afl-clang-fast ./build.sh
	afl-fuzz -i corpus -o findings -- ./harness @@

fuzz-ocaml: ocaml-harness
	afl-fuzz -i ocaml_fuzz/corpus -o ocaml_fuzz/findings -- \
	  ../_build/default/afl/ocaml_fuzz/harness.exe

JOBS ?= $(shell nproc)

fuzz-ocaml-parallel:
	./ocaml_fuzz/fuzz_parallel.sh $(JOBS)

clean:
	rm -f harness
	$(MAKE) -C ocaml_fuzz clean
