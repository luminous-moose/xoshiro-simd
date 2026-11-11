# Opt-in build for the benchmarks: a plain `dune build` never touches them,
# since their stanzas in bench/dune are gated on `--profile bench`.
#
# Prerequisite: opam install ./xoshiro-simd-bench.opam --deps-only (bechamel,
# cppo, splittable_random, pringo, xoshiro). gsl and owl are optional and not
# in that file, since neither builds on every platform (e.g. gsl on 32-bit
# ARM). sync_optional_deps.py, run below, edits bench/dune in place so the
# rest still builds without whichever of them isn't installed (see that
# script's own comment for how). It needs python3 and ocamlfind.

.PHONY: all clean
all:
	python3 sync_optional_deps.py
	dune build --profile bench

clean:
	dune clean
