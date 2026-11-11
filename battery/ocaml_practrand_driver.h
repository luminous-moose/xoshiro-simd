// A small, purpose-built PractRand driver for the OCaml-bridge streams,
// using only PractRand's public Battery/TestManager API (test_batteries.h,
// TestManager.h) -- deliberately NOT a copy of tools/RNG_test.cpp's main()
// this time, since that file is argv/name-driven (built for selecting one
// of PractRand's own built-in RNGs by string) and doesn't fit "OCaml is
// already the one selecting/parameterizing the run." This prints
// PractRand's own computed p-values plainly, with no attempt to
// reproduce RNG_test.cpp's "smart threshold" anomaly-flagging heuristic
// -- same posture as the TestU01 side, which already just prints raw
// p-values and leaves the significance judgement to the reader.
#ifndef XOSHIRO_OCAML_PRACTRAND_DRIVER_H
#define XOSHIRO_OCAML_PRACTRAND_DRIVER_H

// PractRand's own headers (test_helpers.h, TestManager.h, etc.) assume the
// including file already pulled these in -- tools/RNG_test.cpp does so
// before including PractRand_full.h itself, which is why compiling it
// directly never surfaces this; a fresh includer doesn't get it for free.
#include <cmath>
#include <cstdio>
#include <cstring>
#include <list>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <string>
#include <vector>

#include "PractRand_full.h"

// Runs PractRand's core test battery (standard folding) against `rng` for
// `max_bytes` bytes of output, printing one line per test result, followed
// by the subset of results that look like outliers -- same two-tailed
// "distance from the nearer edge of [0,1]" criterion as
// battery/practrand_outliers.py, done inline so a run's interesting results
// are visible immediately rather than requiring a second pass over a saved
// results file.
//
// `alpha` is a target *family-wise* false-positive rate for the whole
// battery, not a raw per-test p-value cutoff: the number of named
// statistics PractRand runs grows with `max_bytes` (observed ~101 at 1M
// bytes, ~227 at 1G, >300 at 80G), so a fixed per-test threshold would
// flag a growing number of pure-chance outliers at longer run lengths. The
// per-test cutoff is derived internally via Bonferroni correction
// (alpha / (2 * N), N = number of statistics actually run), floored at
// 1e-4 -- PractRand's own author's documented "suspicious" convention
// (Tests_results.txt) -- so it can never become more conservative than
// the vendor's own recommendation.
void run_practrand_core_battery(
    PractRand::RNGs::vRNG64* rng,
    PractRand::Uint64 max_bytes,
    double alpha
);

#endif // XOSHIRO_OCAML_PRACTRAND_DRIVER_H
