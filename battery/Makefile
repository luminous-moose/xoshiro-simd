# Statistical-battery harness: OCaml-driven only (ocaml_battery.exe),
# exercising the real public Xoshiro_simd API rather than internal C entry
# points.
#
# Setup (not vendored into git -- see .gitignore):
#   1. sudo apt-get install -y libtestu01-0-dev testu01-bin testu01-data
#   2. Download PractRand_<version>.zip from
#      https://sourceforge.net/projects/pracrand/ into this directory
#   3. make practrand_src
#   4. make
#
# `make` builds ocaml_battery.exe via `dune build --profile battery` --
# that stanza is gated the same way bench/'s are (see ../dune and
# battery/dune's own comments): invisible to a plain `dune build`, built
# here once PractRand.a exists, since ocaml_practrand_driver.cpp links
# directly against PractRand's own static lib.

CXX := c++

.PHONY: all clean deepclean practrand_src ocaml_battery
all: ocaml_battery

# Unzips whichever PractRand_*.zip was downloaded into this directory.
# Re-running is safe as long as practrand_src/ was removed first (e.g.
# `rm -rf practrand_src`) -- unzip -o overwrites in place otherwise, which
# is fine, but a stale tree is still better cleared explicitly.
practrand_src:
	@zipfile=$$(ls PractRand_*.zip 2>/dev/null | head -1); \
	if [ -z "$$zipfile" ]; then \
		echo "no PractRand_*.zip found -- download one from https://sourceforge.net/projects/pracrand/ into this directory first"; \
		exit 1; \
	fi; \
	mkdir -p practrand_src && unzip -q -o "$$zipfile" -d practrand_src

# PractRand ships no build system of its own (see
# practrand_src/PractRand/doc/installation.txt) -- this is its documented
# manual build recipe: compile every src/**/*.cpp into a static PractRand.a.
# Kept under its own practrand_obj/, not mixed with anything dune-facing --
# no dune foreign_stubs stanza names anything under practrand_src/ (see
# battery/dune), so this was never at risk of the Makefile/dune object
# collision that motivated separating build directories elsewhere in this
# project's history, but keeping it separate still avoids conflating the
# two origins.
PRACTRAND_SRC := $(wildcard practrand_src/PractRand/src/*.cpp practrand_src/PractRand/src/RNGs/*.cpp practrand_src/PractRand/src/RNGs/other/*.cpp)
PRACTRAND_OBJDIR := practrand_obj
PRACTRAND_OBJS := $(patsubst practrand_src/PractRand/src/%.cpp,$(PRACTRAND_OBJDIR)/%.o,$(PRACTRAND_SRC))
PractRand.a: $(PRACTRAND_OBJS)
	@if [ -z "$(PRACTRAND_OBJS)" ]; then \
		echo "no PractRand sources found in practrand_src/ -- run 'make practrand_src' first"; \
		exit 1; \
	fi
	ar rcs $@ $(PRACTRAND_OBJS)

$(PRACTRAND_OBJDIR)/%.o: practrand_src/PractRand/src/%.cpp
	@mkdir -p $(dir $@)
	$(CXX) -std=c++11 -O3 -Ipractrand_src/PractRand/include -Wno-constant-logical-operand -c -o $@ $<

# dune build scopes to the current directory's own default alias when
# invoked without an explicit target (confirmed directly: running `dune
# build` from inside a subdirectory only builds that subtree), so this
# needs no path argument -- it builds exactly ocaml_battery.exe, the only
# dune stanza in this directory.
ocaml_battery: PractRand.a
	dune build --profile battery

clean:
	rm -rf $(PRACTRAND_OBJDIR)

# clean plus everything derived from the downloaded PractRand zip
# (practrand_src/ is itself a build artifact -- the unzip step run by the
# practrand_src target -- not something to keep around across a
# from-scratch rebuild test) and everything dune built for this
# directory. Deliberately does NOT remove the PractRand_*.zip itself --
# that's the actual downloaded source, not something `make` generates,
# and there's no need to re-download it just to test a from-scratch
# build. `dune clean` clears the *whole project's* build cache, not just
# battery/ -- there's no narrower dune command for "just this directory",
# but it's always safe (dune just rebuilds whatever it's asked for next).
deepclean: clean
	rm -rf practrand_src
	rm -f PractRand.a
	cd .. && dune clean
