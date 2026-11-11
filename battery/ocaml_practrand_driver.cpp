#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <string>
#include <vector>

#include "ocaml_practrand_driver.h"

namespace {

// Distance from the nearer edge of [0,1] -- smaller is more suspicious.
// Mirrors practrand_outliers.py's extremity(): PractRand p-values are
// uniform on [0,1] when passing, so a value near 1 is exactly as
// suspicious as one near 0, not just values near 0.
double extremity(double p) { return std::min(p, 1.0 - p); }

// Formats an elapsed duration for a glance-friendly summary line: plain
// seconds below a minute, otherwise minutes/hours broken out (a raw
// "3712.481s" is much harder to read at a glance than "1h02m52.48s").
std::string format_duration(double seconds) {
    char buf[64];
    if (seconds < 60.0) {
        std::snprintf(buf, sizeof(buf), "%.3fs", seconds);
    } else if (seconds < 3600.0) {
        int minutes = (int) (seconds / 60.0);
        double secs = seconds - minutes * 60.0;
        std::snprintf(buf, sizeof(buf), "%dm%05.2fs", minutes, secs);
    } else {
        int hours = (int) (seconds / 3600.0);
        int minutes = (int) ((seconds - hours * 3600.0) / 60.0);
        double secs = seconds - hours * 3600.0 - minutes * 60.0;
        std::snprintf(
            buf, sizeof(buf), "%dh%02dm%05.2fs", hours, minutes, secs);
    }
    return std::string(buf);
}

} // namespace

// tools/TestManager.h and PractRand/RNGs/all.h both assume `using
// namespace PractRand;` is already in effect at the point they're
// included (their own bodies reference e.g. `Uint64`/`hc256` unqualified)
// -- tools/RNG_test.cpp does this at file scope before including either,
// so it never surfaces there; matched here rather than qualifying every
// reference throughout both vendored headers.
using namespace PractRand;

// PractRand/RNGs/all.h (specific algorithm implementations, incl. hc256
// used below as TestManager's "known good" reference source) isn't part
// of PractRand_full.h -- see tools/RNG_test.cpp's own comment on why.
#include "PractRand/RNGs/all.h"
#include "TestManager.h"

void run_practrand_core_battery(
    PractRand::RNGs::vRNG64* rng,
    PractRand::Uint64 max_bytes,
    double alpha
) {
    Tests::ListOfTests tests =
        Tests::Batteries::apply_standard_foldings(rng, Tests::Batteries::get_core_tests);
    RNGs::Polymorphic::hc256 known_good(SEED_AUTO);
    TestManager tman(&tests, &known_good);
    tman.reset(rng);

    Uint64 blocks = max_bytes / Tests::TestBlock::SIZE;
    if (blocks == 0) blocks = 1;

    const auto start = std::chrono::steady_clock::now();
    tman.test(blocks);
    const auto elapsed = std::chrono::steady_clock::now() - start;
    const double elapsed_seconds =
        std::chrono::duration<double>(elapsed).count();

    std::vector<TestResult> results;
    tman.get_results(results);

    Uint64 tested_bytes = tman.get_blocks_so_far() * Tests::TestBlock::SIZE;
    std::printf(
        "rng=%s, length=%llu bytes (2^%.2f), wall_clock=%s\n",
        rng->get_name().c_str(),
        (unsigned long long) tested_bytes,
        std::log((double) tested_bytes) / std::log(2.0),
        format_duration(elapsed_seconds).c_str()
    );
    for (size_t i = 0; i < results.size(); i++) {
        std::printf(
            "  %-40s p-value=%.6g\n",
            results[i].name.c_str(),
            results[i].get_pvalue()
        );
    }

    // Bonferroni-style correction: the number of named statistics PractRand
    // runs grows with `max_bytes` (more folds/parameterizations become
    // active the longer it runs -- observed ~101 at 1M bytes up to ~227 at
    // 1G), so a fixed per-test p-value threshold doesn't give a stable
    // family-wise false-positive rate across different --tlmax values. `p
    // <= T || p >= 1-T` doubles the per-test false-positive chance (two
    // tails), hence the factor of 2. Slightly conservative in practice
    // since folded variants share underlying data and aren't fully
    // independent, but that's the right direction to err in for a
    // defect-hunting tool.
    //
    // Floored at 1e-4: PractRand's own author's documented convention
    // (practrand_src/PractRand/doc/Tests_results.txt: "any non-failing
    // p-value less than 1e-4 or greater than (1-1e-4) is considered
    // suspicious"), empirically calibrated against a large set of known-bad
    // PRNGs. At the N observed above, alpha/(2N) alone lands well below
    // that (~2e-5 to 5e-5 at alpha=0.01) -- tighter than the author's own
    // bar, and in practice too conservative to catch real, subtle defects
    // (confirmed directly: missed a deliberately injected 1-in-256 bit
    // bias). The floor means this can still tighten automatically below
    // 1e-4 for a large `alpha` or small N, but never becomes stricter than
    // the author's recommendation.
    const double per_test_threshold =
        std::max(alpha / (2.0 * (double) results.size()), 1e-4);

    std::vector<TestResult> outliers;
    for (size_t i = 0; i < results.size(); i++) {
        if (extremity(results[i].get_pvalue()) <= per_test_threshold) {
            outliers.push_back(results[i]);
        }
    }
    std::sort(
        outliers.begin(),
        outliers.end(),
        [](const TestResult& a, const TestResult& b) {
            return extremity(a.get_pvalue()) < extremity(b.get_pvalue());
        }
    );

    std::printf(
        "\n%zu/%zu outlier(s) at per-test threshold %.3g "
        "(family-wise alpha=%.3g, N=%zu)\n",
        outliers.size(),
        results.size(),
        per_test_threshold,
        alpha,
        results.size()
    );
    for (size_t i = 0; i < outliers.size(); i++) {
        double p = outliers[i].get_pvalue();
        std::printf(
            "  %.6g  (%s)  %s\n",
            p,
            (p < 0.5) ? "low" : "high",
            outliers[i].name.c_str()
        );
    }

    std::fflush(stdout);
}
