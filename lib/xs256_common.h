#ifndef XOSHIRO_SIMD_CORE_H
#define XOSHIRO_SIMD_CORE_H

#include <stddef.h>
#include <stdint.h>

// Types and constants

#define QWORDS 128
#define FLOATWORDS 128

#define XS256_LANES 4

#define XS256_DOUBLE_ONE_BITS 0x3ff0000000000000ULL
#define XS256_FLOAT_SCALE 0x1p-24f
#define XS256_DOUBLE_SCALE 0x1p-53

// At or above these thresholds, scale * XS256_*_SCALE is a normal float and
// exact, so it can be folded into one multiply over each element. Below these
// thresholds, it would be subnormal and lose bits, so the two multiplies stay
// separate to keep precision and match Plain's output.
#define XS256_DOUBLE_SCALE_THRESHOLD 0x1p-969
#define XS256_FLOAT_SCALE_THRESHOLD 0x1p-102f

#if QWORDS % XS256_LANES != 0 || FLOATWORDS % XS256_LANES != 0
#error "QWORDS and FLOATWORDS must be multiples of XS256_LANES."
#endif

// These are internal errors that the stubs turn into OCaml exceptions.
#define XS256_ENTROPY_READ_ERROR (-1)
#define XS256_ENTROPY_ALL_ZERO (-2)
#define XS256_BACKEND_NOT_COMPILED (-3)
#define XS256_BACKEND_NOT_SUPPORTED (-4)

#if defined(__BYTE_ORDER__) && defined(__ORDER_BIG_ENDIAN__)
#define XS256_BIG_ENDIAN (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
#else // !(defined(__BYTE_ORDER__) && defined(__ORDER_BIG_ENDIAN__))
#error "cannot determine host byte order (no __BYTE_ORDER__)"
#endif // defined(__BYTE_ORDER__) && defined(__ORDER_BIG_ENDIAN__)

static inline uint64_t xs256simd_swap_halves64(uint64_t x) {
    return (x << 32) | (x >> 32);
}

static inline uint64_t xs256simd_swap_quarters64(uint64_t x) {
    uint64_t t = __builtin_bswap64(x);
    return ((t & 0x00ff00ff00ff00ffULL) << 8) |
           ((t >> 8) & 0x00ff00ff00ff00ffULL);
}

// The builtin can't be passed to another function as an argument, so this
// inline function works around that.
static inline uint64_t xs256simd_swap_bytes64(uint64_t x) {
    return __builtin_bswap64(x);
}

typedef enum {
    XOSHIRO_SIMD_BACKEND_AUTO = 0,
    XOSHIRO_SIMD_BACKEND_PLAIN = 1,
    XOSHIRO_SIMD_BACKEND_SSE2 = 2,
    XOSHIRO_SIMD_BACKEND_AVX2 = 3,
    XOSHIRO_SIMD_BACKEND_AVX512FVL = 4,
    XOSHIRO_SIMD_BACKEND_NEON = 5,
} xoshiro256pp_backend;

typedef struct xoshiro256pp_s {
    // The state consists of 4 (XS256_LANES) xoshiro256 states, each 4 qwords,
    // in parallel, structured so SIMD units can work on them in parallel.
    uint64_t s[4 * XS256_LANES];
    // The bitstream is uint32_t because the common cases of ranged-int draws
    // and bools only need 32 bits, and they use up the buffer at half the rate
    // of 64 bit draws.
    _Alignas(8) uint32_t bitstream[QWORDS * 2];
    double doublestream[FLOATWORDS];
    xoshiro256pp_backend backend_id;
    void (*xs256_block_fn)(struct xoshiro256pp_s* state);
    void (*xs256_double_block_fn)(struct xoshiro256pp_s* state);
    void (*xs256_fill_blocks_fn)(
        struct xoshiro256pp_s* state,
        void* dst,
        size_t count
    );
    void (*xs256_fill_32bit_float_ba_fn)(
        struct xoshiro256pp_s* state,
        float* dst,
        size_t length
    );
    void (*xs256_fill_double_ba_fn)(
        struct xoshiro256pp_s* state,
        double* dst,
        size_t length
    );
    void (*xs256_fill_32bit_float_range_ba_fn)(
        struct xoshiro256pp_s* state,
        float* dst,
        float scale,
        float offset,
        size_t length
    );
    void (*xs256_fill_double_range_ba_fn)(
        struct xoshiro256pp_s* state,
        double* dst,
        double scale,
        double offset,
        size_t length
    );
} xoshiro256pp;

// State initialization

int xs256simd_setup(
    xoshiro256pp* state,
    const uint64_t seed,
    xoshiro256pp_backend backend
);

int xs256simd_setup_from_string(
    xoshiro256pp* state,
    const void* seed,
    size_t seed_length,
    xoshiro256pp_backend backend
);

int xs256simd_setup_from_array(
    xoshiro256pp* state,
    const uint64_t* words,
    size_t n_words,
    xoshiro256pp_backend backend
);

int xs256simd_setup_from_entropy(
    xoshiro256pp* state,
    xoshiro256pp_backend backend
);

int xs256simd_seed_from_entropy(xoshiro256pp* state);

// Byte fills

void xs256simd_fill_bytes(xoshiro256pp* state, uint8_t* dst, size_t length);

#if XS256_BIG_ENDIAN
void xs256simd_fill_bytes_be_int8(
    xoshiro256pp* state,
    uint8_t* dst,
    size_t length
);
void xs256simd_fill_bytes_be_int16(
    xoshiro256pp* state,
    uint8_t* dst,
    size_t length
);
void xs256simd_fill_bytes_be_int32(
    xoshiro256pp* state,
    uint8_t* dst,
    size_t length
);
#endif // XS256_BIG_ENDIAN

#endif // XOSHIRO_SIMD_CORE_H
