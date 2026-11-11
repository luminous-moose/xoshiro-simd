// AFL/ASan fuzz harness for xoshiro-simd's C core.
//
// This bypasses the OCaml layer entirely and links lib/xs256_*.c directly.
// Fuzzer input is an 8-byte seed (the same uint64_t xs256simd_setup() takes)
// and a 1-byte length, optionally followed by the four range parameters (see
// INPUT_LEN and FULL_INPUT_LEN below), fed to every SIMD backend
// xs256simd_available_backends() reports as usable on this machine. All backends
// must produce byte-identical bitstream, doublestream, and bulk-fill
// (float32/double, plain and range-scaled) output, and end in the same s[]
// state, for the same seed -- a mismatch means a correctness bug in one of
// the SIMD cores, and a crash under ASan means a memory-safety bug. The
// length byte (0-255) is reused across all four xs256_fill_*_ba_fn calls:
// that range spans several full blocks plus every possible remainder for
// both the 8-element float32 block and the 4-element double block,
// exercising the tail/remainder buffer-then-memcpy pattern.
//
// Scale and offset for the two range variants are fuzzed, because they are
// not just a rescaling of the same draws: the magnitude of scale selects the
// code path. The float32 SIMD backends and AVX512FVL's float64 fill compare
// |scale| against XS256_FLOAT_SCALE_THRESHOLD / XS256_DOUBLE_SCALE_THRESHOLD
// (xs256_common.h) to choose between a loop that folds the 2^-k constant into
// scale and one that keeps the extra multiply, and on 32-bit ARM the
// flush-to-zero fallback to Plain also looks at offset. A fuzzer that only
// mutates bytes will not decompose a floating-point compare on its own, so
// the corpus carries hand-made inputs on both sides of every threshold (see
// corpus/range_* and gen_corpus.py, which writes them), and a single mutated
// exponent byte then sweeps across it. NaN and infinite scale/offset are
// replaced by the defaults: the library's public API rejects them before
// they reach C, and NaN payload and sign bits are not something the backends
// promise to agree on (memcmp below compares them).
//
// Cross-backend comparison is the only correctness check possible at this
// layer, not one option among several: the C core's entire output is the
// raw bitstream/doublestream/bulk fills, with none of the higher-level
// semantics (int ranges, shuffling) that live in OCaml above it, so there's
// no known-answer value to check here, only "do the backends agree
// with each other". Since afl-fuzz itself is coverage-guided and has no way
// to know this abort() is the signal worth finding, self_test.sh separately
// proves this comparison actually fires on a real divergence, by injecting
// one and asserting the harness catches it.
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "xs256_common.h"
#include "xs256_dispatch.h"

// An 8-byte seed plus a 1-byte length (see top comment). Shorter inputs are
// ignored.
#define INPUT_LEN 9
// Optional trailing fields, all little-endian: float32 scale (4 bytes),
// float32 offset (4), float64 scale (8), float64 offset (8). An input that
// stops before the end of these fields uses the defaults below for all of
// them, so 9-byte inputs (the original corpus) stay valid.
#define FULL_INPUT_LEN (INPUT_LEN + 4 + 4 + 8 + 8)

static const float DEFAULT_SCALE_F = 2.0F;
static const float DEFAULT_OFFSET_F = -1.0F;
static const double DEFAULT_SCALE_D = 2.0;
static const double DEFAULT_OFFSET_D = -1.0;

static uint64_t read_le(const uint8_t* p, int bytes) {
    uint64_t v = 0;
    for (int i = bytes - 1; i >= 0; i--) {
        v = (v << 8) | p[i];
    }
    return v;
}

static float read_float(const uint8_t* p, float fallback) {
    uint32_t bits = (uint32_t)read_le(p, 4);
    float f;
    memcpy(&f, &bits, sizeof(f));
    return isfinite(f) ? f : fallback;
}

static double read_double(const uint8_t* p, double fallback) {
    uint64_t bits = read_le(p, 8);
    double d;
    memcpy(&d, &bits, sizeof(d));
    return isfinite(d) ? d : fallback;
}

static int run_one(const uint8_t* data, size_t size) {
    if (size < INPUT_LEN) {
        return 0;
    }

    uint64_t seed;
    memcpy(&seed, data, sizeof(seed));
    size_t length = data[8];

    xoshiro256pp_backend backends[XS256_MAX_BACKENDS];
    size_t n_backends = xs256simd_available_backends(backends);

    xoshiro256pp ref;
    uint32_t ref_bitstream[QWORDS * 2];
    double ref_doublestream[FLOATWORDS];
    float ref_f32[UINT8_MAX];
    double ref_f64[UINT8_MAX];
    float ref_f32_range[UINT8_MAX];
    double ref_f64_range[UINT8_MAX];
    int have_ref = 0;

    float scale_f = DEFAULT_SCALE_F;
    float offset_f = DEFAULT_OFFSET_F;
    double scale_d = DEFAULT_SCALE_D;
    double offset_d = DEFAULT_OFFSET_D;
    if (size >= FULL_INPUT_LEN) {
        const uint8_t* p = data + INPUT_LEN;
        scale_f = read_float(p, DEFAULT_SCALE_F);
        offset_f = read_float(p + 4, DEFAULT_OFFSET_F);
        scale_d = read_double(p + 8, DEFAULT_SCALE_D);
        offset_d = read_double(p + 16, DEFAULT_OFFSET_D);
    }

    for (size_t i = 0; i < n_backends; i++) {
        xoshiro256pp state;
        // Every backend here came from xs256simd_available_backends(), so
        // setup can only fail if dispatch and that list disagree. Abort
        // rather than call through the state's unset function pointers.
        int retval = xs256simd_setup(&state, seed, backends[i]);
        if (retval != 0) {
            fprintf(
                stderr,
                "setup failed: backend %d returned %d\n",
                (int)backends[i],
                retval
            );
            abort();
        }
        state.xs256_block_fn(&state);
        // Run a second block so the SIMD lane-advance/carry logic across a
        // block boundary is exercised too, not just the first fill -- this
        // is purely about the state transition running twice in a row
        // (xoshiro-simd has no rekeying concept to worry about here, unlike
        // a forward-key-erasure generator would).
        state.xs256_block_fn(&state);
        state.xs256_double_block_fn(&state);
        state.xs256_double_block_fn(&state);

        float f32[UINT8_MAX];
        double f64[UINT8_MAX];
        float f32_range[UINT8_MAX];
        double f64_range[UINT8_MAX];
        state.xs256_fill_32bit_float_ba_fn(&state, f32, length);
        state.xs256_fill_double_ba_fn(&state, f64, length);
        state.xs256_fill_32bit_float_range_ba_fn(
            &state,
            f32_range,
            scale_f,
            offset_f,
            length
        );
        state.xs256_fill_double_range_ba_fn(
            &state,
            f64_range,
            scale_d,
            offset_d,
            length
        );

        if (!have_ref) {
            memcpy(ref_bitstream, state.bitstream, sizeof(ref_bitstream));
            memcpy(
                ref_doublestream,
                state.doublestream,
                sizeof(ref_doublestream)
            );
            memcpy(ref_f32, f32, length * sizeof(float));
            memcpy(ref_f64, f64, length * sizeof(double));
            memcpy(ref_f32_range, f32_range, length * sizeof(float));
            memcpy(ref_f64_range, f64_range, length * sizeof(double));
            memcpy(&ref, &state, sizeof(ref));
            have_ref = 1;
            continue;
        }

        if (memcmp(ref_bitstream, state.bitstream, sizeof(ref_bitstream)) !=
                0 ||
            memcmp(
                ref_doublestream,
                state.doublestream,
                sizeof(ref_doublestream)
            ) != 0 ||
            memcmp(ref_f32, f32, length * sizeof(float)) != 0 ||
            memcmp(ref_f64, f64, length * sizeof(double)) != 0 ||
            memcmp(ref_f32_range, f32_range, length * sizeof(float)) != 0 ||
            memcmp(ref_f64_range, f64_range, length * sizeof(double)) != 0 ||
            memcmp(ref.s, state.s, sizeof(ref.s)) != 0) {
            fprintf(
                stderr,
                "backend mismatch: backend %d vs backend %d (length %zu, "
                "scale_f %a offset_f %a scale_d %a offset_d %a)\n",
                (int)ref.backend_id,
                (int)state.backend_id,
                length,
                (double)scale_f,
                (double)offset_f,
                scale_d,
                offset_d
            );
            abort();
        }
    }

    return 0;
}

int main(int argc, char** argv) {
    FILE* f = argc > 1 ? fopen(argv[1], "rb") : stdin;
    if (!f) {
        perror("fopen");
        return 1;
    }

    uint8_t buf[4096];
    size_t n = fread(buf, 1, sizeof(buf), f);
    if (f != stdin) {
        fclose(f);
    }

    return run_one(buf, n);
}
