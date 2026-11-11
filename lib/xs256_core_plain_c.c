#include "xs256_core_plain_c.h"

#include <string.h>

#include "xs256_common.h"
#include "xs256_scalar_step.h"

void xs256simd_block_plain(xoshiro256pp* state) {
    for (size_t i = 0; i < QWORDS / XS256_LANES; i++) {
        for (size_t lane = 0; lane < XS256_LANES; lane++) {
            uint64_t qword = xs256plusplus_scalar_next(state->s, lane);
#if XS256_BIG_ENDIAN
            // All big-endian hosts use this path. Store the low half first so
            // the bitstream matches little-endian.
            size_t idx = i * XS256_LANES * 2 + lane * 2;
            state->bitstream[idx] = (uint32_t)qword;
            state->bitstream[idx + 1] = (uint32_t)(qword >> 32);
#else  // !XS256_BIG_ENDIAN
            memcpy(
                &state->bitstream[i * XS256_LANES * 2 + lane * 2],
                &qword,
                sizeof(uint64_t)
            );
#endif // XS256_BIG_ENDIAN
        }
    }
}

void xs256simd_fill_blocks_plain(xoshiro256pp* state, void* dst, size_t count) {
    uint8_t* out = (uint8_t*)dst;
    for (size_t i = 0; i < count; i++) {
        for (size_t lane = 0; lane < XS256_LANES; lane++) {
            uint64_t qword = xs256plusplus_scalar_next(state->s, lane);
            memcpy(
                out + (i * XS256_LANES + lane) * sizeof(uint64_t),
                &qword,
                sizeof(uint64_t)
            );
        }
    }
}

static inline uint64_t xs256plus_scalar_next(
    uint64_t s[static 4 * XS256_LANES],
    size_t lane
) {
    uint64_t s0 = s[lane];
    uint64_t s1 = s[lane + XS256_LANES];
    uint64_t s2 = s[lane + 2 * XS256_LANES];
    uint64_t s3 = s[lane + 3 * XS256_LANES];

    const uint64_t result = s0 + s3;

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

static inline void fill_double_blocks_plain(
    xoshiro256pp* state,
    double* dst,
    size_t count
) {
    for (size_t i = 0; i < count; i++) {
        for (size_t lane = 0; lane < XS256_LANES; lane++) {
            uint64_t qword = xs256plus_scalar_next(state->s, lane);
            uint64_t mant = qword >> 11;
            double d = (double)mant;
            dst[i * XS256_LANES + lane] = d * XS256_DOUBLE_SCALE;
        }
    }
}

void xs256simd_double_block_plain(xoshiro256pp* state) {
    fill_double_blocks_plain(
        state,
        state->doublestream,
        FLOATWORDS / XS256_LANES
    );
}

static inline void fill_32bit_float_blocks_plain(
    xoshiro256pp* state,
    float* dst,
    size_t count
) {
    for (size_t i = 0; i < count; i++) {
        for (size_t lane = 0; lane < XS256_LANES; lane++) {
            size_t base = i * (XS256_LANES * 2) + lane * 2;
            uint64_t qword = xs256plus_scalar_next(state->s, lane);
            uint32_t hi = (uint32_t)(qword >> 32);
            uint32_t lo = (uint32_t)(qword & 0xffffffffU);
            // lo then hi matches the vector order.
            uint32_t u24_lo = lo >> 8;
            float f_lo = (float)(int32_t)u24_lo;
            dst[base] = f_lo * XS256_FLOAT_SCALE;
            uint32_t u24_hi = hi >> 8;
            float f_hi = (float)(int32_t)u24_hi;
            dst[base + 1] = f_hi * XS256_FLOAT_SCALE;
        }
    }
}

void xs256simd_fill_32bit_float_ba_plain(
    xoshiro256pp* state,
    float* dst,
    size_t length
) {
    size_t num_blocks = length / (XS256_LANES * 2);
    size_t remainder = length % (XS256_LANES * 2);
    fill_32bit_float_blocks_plain(state, dst, num_blocks);
    if (remainder > 0) {
        float buffer[XS256_LANES * 2];
        fill_32bit_float_blocks_plain(state, buffer, 1);
        memcpy(
            dst + num_blocks * XS256_LANES * 2,
            buffer,
            remainder * sizeof(float)
        );
    }
}

void xs256simd_fill_double_ba_plain(
    xoshiro256pp* state,
    double* dst,
    size_t length
) {
    size_t num_blocks = length / XS256_LANES;
    size_t remainder = length % XS256_LANES;
    fill_double_blocks_plain(state, dst, num_blocks);
    if (remainder > 0) {
        double buffer[XS256_LANES];
        fill_double_blocks_plain(state, buffer, 1);
        memcpy(
            dst + num_blocks * XS256_LANES,
            buffer,
            remainder * sizeof(double)
        );
    }
}

static inline void fill_32bit_float_range_blocks_plain(
    xoshiro256pp* state,
    float* dst,
    float scale,
    float offset,
    size_t count
) {
    for (size_t i = 0; i < count; i++) {
        for (size_t lane = 0; lane < XS256_LANES; lane++) {
            size_t base = i * (XS256_LANES * 2) + lane * 2;
            uint64_t qword = xs256plus_scalar_next(state->s, lane);
            uint32_t hi = (uint32_t)(qword >> 32);
            uint32_t lo = (uint32_t)(qword & 0xffffffffU);
            uint32_t u24_lo = lo >> 8;
            // The two multiplications for each 32 bit float here are on
            // purpose: this handles the case where scale * XS256_FLOAT_SCALE
            // would become subnormal and lose precision.
            float f_lo = (float)(int32_t)u24_lo * XS256_FLOAT_SCALE;
            f_lo = f_lo * scale + offset;
            dst[base] = f_lo;
            uint32_t u24_hi = hi >> 8;
            float f_hi = (float)(int32_t)u24_hi * XS256_FLOAT_SCALE;
            f_hi = f_hi * scale + offset;
            dst[base + 1] = f_hi;
        }
    }
}

static inline void fill_double_range_blocks_plain(
    xoshiro256pp* state,
    double* dst,
    double scale,
    double offset,
    size_t count
) {
    for (size_t i = 0; i < count; i++) {
        for (size_t lane = 0; lane < XS256_LANES; lane++) {
            uint64_t qword = xs256plus_scalar_next(state->s, lane);
            uint64_t mant = qword >> 11;
            // Same reasoning as above.
            double d = (double)mant * XS256_DOUBLE_SCALE;
            dst[i * XS256_LANES + lane] = d * scale + offset;
        }
    }
}

void xs256simd_fill_32bit_float_range_ba_plain(
    xoshiro256pp* state,
    float* dst,
    float scale,
    float offset,
    size_t length
) {
    size_t num_blocks = length / (XS256_LANES * 2);
    size_t remainder = length % (XS256_LANES * 2);
    fill_32bit_float_range_blocks_plain(state, dst, scale, offset, num_blocks);
    if (remainder > 0) {
        float buffer[XS256_LANES * 2];
        fill_32bit_float_range_blocks_plain(state, buffer, scale, offset, 1);
        memcpy(
            dst + num_blocks * XS256_LANES * 2,
            buffer,
            remainder * sizeof(float)
        );
    }
}

void xs256simd_fill_double_range_ba_plain(
    xoshiro256pp* state,
    double* dst,
    double scale,
    double offset,
    size_t length
) {
    size_t num_blocks = length / XS256_LANES;
    size_t remainder = length % XS256_LANES;
    fill_double_range_blocks_plain(state, dst, scale, offset, num_blocks);
    if (remainder > 0) {
        double buffer[XS256_LANES];
        fill_double_range_blocks_plain(state, buffer, scale, offset, 1);
        memcpy(
            dst + num_blocks * XS256_LANES,
            buffer,
            remainder * sizeof(double)
        );
    }
}

int xs256simd_plain_is_supported(void) { return 1; }
