#if defined(__AVX512F__) && defined(__AVX512VL__) && defined(__AVX512DQ__)

#include "xs256_core_avx512fvl.h"

#include <math.h>
#include <string.h>
#include <immintrin.h>

#include "xs256_common.h"

#if QWORDS % 4 != 0
#error "AVX512FVL is only supported with a multiple of 4 qwords output."
#endif

static inline __m256i xs256plusplus_avx512fvl_next(__m256i s[static 4]) {
    __m256i s0 = s[0];
    __m256i s1 = s[1];
    __m256i s2 = s[2];
    __m256i s3 = s[3];

    const __m256i result =
        _mm256_add_epi64(_mm256_rol_epi64(_mm256_add_epi64(s0, s3), 23), s0);

    const __m256i t = _mm256_slli_epi64(s1, 17);

    s2 = _mm256_xor_si256(s2, s0);
    s3 = _mm256_xor_si256(s3, s1);
    s1 = _mm256_xor_si256(s1, s2);
    s0 = _mm256_xor_si256(s0, s3);

    s2 = _mm256_xor_si256(s2, t);

    s3 = _mm256_rol_epi64(s3, 45);

    s[0] = s0;
    s[1] = s1;
    s[2] = s2;
    s[3] = s3;

    return result;
}

static inline void fill_int_blocks_avx512fvl(
    __m256i s[static 4],
    uint8_t* dst,
    size_t count
) {
    for (size_t i = 0; i < count; i++) {
        __m256i result = xs256plusplus_avx512fvl_next(s);
        _mm256_storeu_si256(
            (__m256i*)(dst + i * XS256_LANES * sizeof(uint64_t)),
            result
        );
    }
}

static inline void load_s(__m256i s[static 4], const xoshiro256pp* state) {
    for (size_t i = 0; i < 4; i++) {
        s[i] = _mm256_loadu_si256((const __m256i*)&state->s[i * XS256_LANES]);
    }
}

static inline void store_s(__m256i s[static 4], xoshiro256pp* state) {
    for (size_t i = 0; i < 4; i++) {
        _mm256_storeu_si256((__m256i*)&state->s[i * XS256_LANES], s[i]);
    }
}

void xs256simd_block_avx512fvl(xoshiro256pp* state) {
    __m256i s_simd[4];
    load_s(s_simd, state);

    fill_int_blocks_avx512fvl(
        s_simd,
        (uint8_t*)state->bitstream,
        QWORDS / XS256_LANES
    );

    store_s(s_simd, state);
}

void xs256simd_fill_blocks_avx512fvl(
    xoshiro256pp* state,
    void* dst,
    size_t count
) {
    __m256i s_simd[XS256_LANES];
    load_s(s_simd, state);

    fill_int_blocks_avx512fvl(s_simd, (uint8_t*)dst, count);

    store_s(s_simd, state);
}

static inline __m256i xs256plus_avx512fvl_next(__m256i s[static 4]) {
    __m256i s0 = s[0];
    __m256i s1 = s[1];
    __m256i s2 = s[2];
    __m256i s3 = s[3];

    const __m256i result = _mm256_add_epi64(s0, s3);

    const __m256i t = _mm256_slli_epi64(s1, 17);

    s2 = _mm256_xor_si256(s2, s0);
    s3 = _mm256_xor_si256(s3, s1);
    s1 = _mm256_xor_si256(s1, s2);
    s0 = _mm256_xor_si256(s0, s3);

    s2 = _mm256_xor_si256(s2, t);

    s3 = _mm256_rol_epi64(s3, 45);

    s[0] = s0;
    s[1] = s1;
    s[2] = s2;
    s[3] = s3;

    return result;
}

static inline void fill_double_blocks_avx512fvl(
    __m256i s[static 4],
    double* dst,
    size_t count
) {
    const __m256d scale = _mm256_set1_pd(XS256_DOUBLE_SCALE);

    for (size_t i = 0; i < count; i++) {
        __m256i result = xs256plus_avx512fvl_next(s);

        __m256i mant = _mm256_srli_epi64(result, 11);
        __m256d unscaled = _mm256_cvtepu64_pd(mant);
        __m256d d = _mm256_mul_pd(unscaled, scale);
        _mm256_storeu_pd(&dst[i * XS256_LANES], d);
    }
}

void xs256simd_double_block_avx512fvl(xoshiro256pp* state) {
    __m256i s_simd[XS256_LANES];
    load_s(s_simd, state);

    fill_double_blocks_avx512fvl(
        s_simd,
        state->doublestream,
        FLOATWORDS / XS256_LANES
    );

    store_s(s_simd, state);
}

static inline void fill_32bit_float_blocks_avx512fvl(
    __m256i s[static 4],
    float* dst,
    __m256 scale,
    size_t count
) {
    for (size_t i = 0; i < count; i++) {
        __m256i result = xs256plus_avx512fvl_next(s);
        __m256i u24 = _mm256_srli_epi32(result, 8);
        __m256 f = _mm256_cvtepi32_ps(u24);
        __m256 scaled = _mm256_mul_ps(f, scale);
        _mm256_storeu_ps(&(dst[i * XS256_LANES * 2]), scaled);
    }
}

void xs256simd_fill_32bit_float_ba_avx512fvl(
    xoshiro256pp* state,
    float* dst,
    size_t length
) {
    __m256i s_simd[XS256_LANES];
    load_s(s_simd, state);
    const __m256 scale = _mm256_set1_ps(XS256_FLOAT_SCALE);

    size_t num_blocks = length / (XS256_LANES * 2);
    size_t remainder = length % (XS256_LANES * 2);
    fill_32bit_float_blocks_avx512fvl(s_simd, dst, scale, num_blocks);
    if (remainder > 0) {
        float buffer[XS256_LANES * 2];
        fill_32bit_float_blocks_avx512fvl(s_simd, buffer, scale, 1);
        memcpy(
            dst + num_blocks * XS256_LANES * 2,
            buffer,
            remainder * sizeof(float)
        );
    }

    store_s(s_simd, state);
}

void xs256simd_fill_double_ba_avx512fvl(
    xoshiro256pp* state,
    double* dst,
    size_t length
) {
    __m256i s_simd[XS256_LANES];
    load_s(s_simd, state);

    size_t num_blocks = length / XS256_LANES;
    size_t remainder = length % XS256_LANES;
    fill_double_blocks_avx512fvl(s_simd, dst, num_blocks);
    if (remainder > 0) {
        double buffer[XS256_LANES];
        fill_double_blocks_avx512fvl(s_simd, buffer, 1);
        memcpy(
            dst + num_blocks * XS256_LANES,
            buffer,
            remainder * sizeof(double)
        );
    }

    store_s(s_simd, state);
}

static inline __attribute__((always_inline)) void
fill_32bit_float_range_blocks_common_avx512fvl(
    __m256i s[static 4],
    float* dst,
    __m256 scale,
    __m256 offset,
    size_t count,
    int exact
) {
    const __m256 scale_down = _mm256_set1_ps(XS256_FLOAT_SCALE);

    for (size_t i = 0; i < count; i++) {
        __m256i result = xs256plus_avx512fvl_next(s);
        __m256i u24 = _mm256_srli_epi32(result, 8);
        __m256 unscaled = _mm256_cvtepi32_ps(u24);
        __m256 scaled = exact ? _mm256_mul_ps(unscaled, scale_down) : unscaled;
        scaled = _mm256_mul_ps(scaled, scale);
        __m256 out = _mm256_add_ps(scaled, offset);
        _mm256_storeu_ps(&(dst[i * XS256_LANES * 2]), out);
    }
}

static inline void fill_32bit_float_range_blocks_avx512fvl(
    __m256i s[static 4],
    float* dst,
    __m256 scale,
    __m256 offset,
    size_t count
) {
    fill_32bit_float_range_blocks_common_avx512fvl(
        s,
        dst,
        scale,
        offset,
        count,
        0
    );
}

static inline void fill_32bit_float_range_blocks_exact_avx512fvl(
    __m256i s[static 4],
    float* dst,
    __m256 scale,
    __m256 offset,
    size_t count
) {
    fill_32bit_float_range_blocks_common_avx512fvl(
        s,
        dst,
        scale,
        offset,
        count,
        1
    );
}

void xs256simd_fill_32bit_float_range_ba_avx512fvl(
    xoshiro256pp* state,
    float* dst,
    float scale,
    float offset,
    size_t length
) {
    __m256i s_simd[XS256_LANES];
    load_s(s_simd, state);

    const __m256 offset256 = _mm256_set1_ps(offset);

    size_t num_blocks = length / (XS256_LANES * 2);
    size_t remainder = length % (XS256_LANES * 2);

    if (fabsf(scale) >= XS256_FLOAT_SCALE_THRESHOLD) {
        const __m256 scale256 = _mm256_set1_ps(scale * XS256_FLOAT_SCALE);
        fill_32bit_float_range_blocks_avx512fvl(
            s_simd,
            dst,
            scale256,
            offset256,
            num_blocks
        );
        if (remainder > 0) {
            float buffer[XS256_LANES * 2];
            fill_32bit_float_range_blocks_avx512fvl(
                s_simd,
                buffer,
                scale256,
                offset256,
                1
            );
            memcpy(
                dst + num_blocks * XS256_LANES * 2,
                buffer,
                remainder * sizeof(float)
            );
        }
    } else {
        const __m256 scale256 = _mm256_set1_ps(scale);
        fill_32bit_float_range_blocks_exact_avx512fvl(
            s_simd,
            dst,
            scale256,
            offset256,
            num_blocks
        );
        if (remainder > 0) {
            float buffer[XS256_LANES * 2];
            fill_32bit_float_range_blocks_exact_avx512fvl(
                s_simd,
                buffer,
                scale256,
                offset256,
                1
            );
            memcpy(
                dst + num_blocks * XS256_LANES * 2,
                buffer,
                remainder * sizeof(float)
            );
        }
    }

    store_s(s_simd, state);
}

static inline __attribute__((always_inline)) void
fill_double_range_blocks_common_avx512fvl(
    __m256i s[static 4],
    double* dst,
    __m256d scale,
    __m256d offset,
    size_t count,
    int exact
) {
    const __m256d scale_down = _mm256_set1_pd(XS256_DOUBLE_SCALE);

    for (size_t i = 0; i < count; i++) {
        __m256i result = xs256plus_avx512fvl_next(s);

        __m256i mant = _mm256_srli_epi64(result, 11);
        __m256d unscaled = _mm256_cvtepu64_pd(mant);
        __m256d scaled = exact ? _mm256_mul_pd(unscaled, scale_down) : unscaled;
        scaled = _mm256_mul_pd(scaled, scale);
        __m256d d = _mm256_add_pd(scaled, offset);
        _mm256_storeu_pd(&dst[i * XS256_LANES], d);
    }
}

static inline void fill_double_range_blocks_avx512fvl(
    __m256i s[static 4],
    double* dst,
    __m256d scale,
    __m256d offset,
    size_t count
) {
    fill_double_range_blocks_common_avx512fvl(s, dst, scale, offset, count, 0);
}

static inline void fill_double_range_blocks_exact_avx512fvl(
    __m256i s[static 4],
    double* dst,
    __m256d scale,
    __m256d offset,
    size_t count
) {
    fill_double_range_blocks_common_avx512fvl(s, dst, scale, offset, count, 1);
}

void xs256simd_fill_double_range_ba_avx512fvl(
    xoshiro256pp* state,
    double* dst,
    double scale,
    double offset,
    size_t length
) {
    __m256i s_simd[XS256_LANES];
    load_s(s_simd, state);

    const __m256d offset256 = _mm256_set1_pd(offset);

    size_t num_blocks = length / XS256_LANES;
    size_t remainder = length % XS256_LANES;

    // Only this backend needs the float64 threshold: SSE2/AVX2/NEON32 use the
    // sub trick, and NEON64 uses vcvtq_n_f64_u64, both of which produce the
    // double in [0,1) with no multiply. See XS256_DOUBLE_SCALE_THRESHOLD.
    if (fabs(scale) >= XS256_DOUBLE_SCALE_THRESHOLD) {
        const __m256d scale256 = _mm256_set1_pd(scale * XS256_DOUBLE_SCALE);
        fill_double_range_blocks_avx512fvl(
            s_simd,
            dst,
            scale256,
            offset256,
            num_blocks
        );
        if (remainder > 0) {
            double buffer[XS256_LANES];
            fill_double_range_blocks_avx512fvl(
                s_simd,
                buffer,
                scale256,
                offset256,
                1
            );
            memcpy(
                dst + num_blocks * XS256_LANES,
                buffer,
                remainder * sizeof(double)
            );
        }
    } else {
        const __m256d scale256 = _mm256_set1_pd(scale);
        fill_double_range_blocks_exact_avx512fvl(
            s_simd,
            dst,
            scale256,
            offset256,
            num_blocks
        );
        if (remainder > 0) {
            double buffer[XS256_LANES];
            fill_double_range_blocks_exact_avx512fvl(
                s_simd,
                buffer,
                scale256,
                offset256,
                1
            );
            memcpy(
                dst + num_blocks * XS256_LANES,
                buffer,
                remainder * sizeof(double)
            );
        }
    }

    store_s(s_simd, state);
}

int xs256simd_avx512fvl_is_supported(void) {
    if (__builtin_cpu_supports("avx512f") &&
        __builtin_cpu_supports("avx512vl") &&
        __builtin_cpu_supports("avx512dq")) {
        return 1;
    }
    return 0;
}

#endif // __AVX512F__ && __AVX512VL__ && __AVX512DQ__
