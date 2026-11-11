#ifndef XOSHIRO_SIMD_SCALAR_STEP_H
#define XOSHIRO_SIMD_SCALAR_STEP_H

#include <stdint.h>
#include <stddef.h>

#include "xs256_common.h"

static inline uint64_t xs256_scalar_rotl(const uint64_t x, int k) {
    return (x << k) | (x >> (64 - k));
}

// Needed by both xs256_core_plain_c.c and by the jump function in
// xs256_common.c.
static inline uint64_t xs256plusplus_scalar_next(
    uint64_t s[static 4 * XS256_LANES],
    size_t lane
) {
    uint64_t s0 = s[lane];
    uint64_t s1 = s[lane + XS256_LANES];
    uint64_t s2 = s[lane + 2 * XS256_LANES];
    uint64_t s3 = s[lane + 3 * XS256_LANES];

    const uint64_t result = xs256_scalar_rotl(s0 + s3, 23) + s0;

    const uint64_t t = s1 << 17;

    s2 ^= s0;
    s3 ^= s1;
    s1 ^= s2;
    s0 ^= s3;

    s2 ^= t;

    s3 = xs256_scalar_rotl(s3, 45);

    s[lane] = s0;
    s[lane + XS256_LANES] = s1;
    s[lane + 2 * XS256_LANES] = s2;
    s[lane + 3 * XS256_LANES] = s3;

    return result;
}

#endif // XOSHIRO_SIMD_SCALAR_STEP_H