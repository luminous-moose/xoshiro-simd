#if defined(__AVX2__)

#include "xs256_core_avx2.h"

#include <math.h>
#include <string.h>
#include <immintrin.h>

#include "xs256_common.h"

#if QWORDS % 4 != 0
#error "AVX2 is only supported with a multiple of 4 qwords output."
#endif

static inline __attribute__((always_inline)) __m256i rotl64(__m256i v, int n) {
    __m256i left = _mm256_slli_epi64(v, n);
    __m256i right = _mm256_srli_epi64(v, 64 - n);
    return _mm256_or_si256(left, right);
}

static inline __m256i xs256plusplus_avx2_next(__m256i s[static 4]) {
    __m256i s0 = s[0];
    __m256i s1 = s[1];
    __m256i s2 = s[2];
    __m256i s3 = s[3];

    const __m256i result =
        _mm256_add_epi64(rotl64(_mm256_add_epi64(s0, s3), 23), s0);

    const __m256i t = _mm256_slli_epi64(s1, 17);

    s2 = _mm256_xor_si256(s2, s0);
    s3 = _mm256_xor_si256(s3, s1);
    s1 = _mm256_xor_si256(s1, s2);
    s0 = _mm256_xor_si256(s0, s3);

    s2 = _mm256_xor_si256(s2, t);

    s3 = rotl64(s3, 45);

    s[0] = s0;
    s[1] = s1;
    s[2] = s2;
    s[3] = s3;

    return result;
}

static inline void fill_int_blocks_avx2(
    __m256i s[static 4],
    uint8_t* dst,
    size_t count
) {
    for (size_t i = 0; i < count; i++) {
        __m256i result = xs256plusplus_avx2_next(s);
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

void xs256simd_block_avx2(xoshiro256pp* state) {
    __m256i s_simd[4];
    load_s(s_simd, state);

    fill_int_blocks_avx2(
        s_simd,
        (uint8_t*)state->bitstream,
        QWORDS / XS256_LANES
    );

    store_s(s_simd, state);
}

void xs256simd_fill_blocks_avx2(xoshiro256pp* state, void* dst, size_t count) {
    __m256i s_simd[XS256_LANES];
    load_s(s_simd, state);

    fill_int_blocks_avx2(s_simd, (uint8_t*)dst, count);

    store_s(s_simd, state);
}

static inline __m256i xs256plus_avx2_next(__m256i s[static 4]) {
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

    s3 = rotl64(s3, 45);

    s[0] = s0;
    s[1] = s1;
    s[2] = s2;
    s[3] = s3;

    return result;
}

static inline void fill_double_blocks_avx2(
    __m256i s[static 4],
    double* dst,
    size_t count
) {
    const __m256i double_one_bits = _mm256_set1_epi64x(XS256_DOUBLE_ONE_BITS);

    for (size_t i = 0; i < count; i++) {
        // See fill_double_blocks_sse2 in xs256_core_sse2.c.
        __m256i result = xs256plus_avx2_next(s);
        __m256i mant = _mm256_srli_epi64(result, 12);
        __m256i onetwo = _mm256_or_si256(mant, double_one_bits);
        __m256i b = _mm256_srli_epi64(_mm256_slli_epi64(result, 52), 63);
        __m256i one_bits_minus_b = _mm256_sub_epi64(double_one_bits, b);
        __m256d d = _mm256_sub_pd(
            _mm256_castsi256_pd(onetwo),
            _mm256_castsi256_pd(one_bits_minus_b)
        );
        _mm256_storeu_pd(&dst[i * XS256_LANES], d);
    }
}

void xs256simd_double_block_avx2(xoshiro256pp* state) {
    __m256i s_simd[XS256_LANES];
    load_s(s_simd, state);

    fill_double_blocks_avx2(
        s_simd,
        state->doublestream,
        FLOATWORDS / XS256_LANES
    );

    store_s(s_simd, state);
}

static inline void fill_32bit_float_blocks_avx2(
    __m256i s[static 4],
    float* dst,
    __m256 scale,
    size_t count
) {
    for (size_t i = 0; i < count; i++) {
        __m256i result = xs256plus_avx2_next(s);
        __m256i u24 = _mm256_srli_epi32(result, 8);
        __m256 f = _mm256_cvtepi32_ps(u24);
        __m256 scaled = _mm256_mul_ps(f, scale);
        _mm256_storeu_ps(&(dst[i * XS256_LANES * 2]), scaled);
    }
}

void xs256simd_fill_32bit_float_ba_avx2(
    xoshiro256pp* state,
    float* dst,
    size_t length
) {
    __m256i s_simd[XS256_LANES];
    load_s(s_simd, state);
    const __m256 scale = _mm256_set1_ps(XS256_FLOAT_SCALE);

    size_t num_blocks = length / (XS256_LANES * 2);
    size_t remainder = length % (XS256_LANES * 2);
    fill_32bit_float_blocks_avx2(s_simd, dst, scale, num_blocks);
    if (remainder > 0) {
        float buffer[XS256_LANES * 2];
        fill_32bit_float_blocks_avx2(s_simd, buffer, scale, 1);
        memcpy(
            dst + num_blocks * XS256_LANES * 2,
            buffer,
            remainder * sizeof(float)
        );
    }

    store_s(s_simd, state);
}

void xs256simd_fill_double_ba_avx2(
    xoshiro256pp* state,
    double* dst,
    size_t length
) {
    __m256i s_simd[XS256_LANES];
    load_s(s_simd, state);

    size_t num_blocks = length / XS256_LANES;
    size_t remainder = length % XS256_LANES;
    fill_double_blocks_avx2(s_simd, dst, num_blocks);
    if (remainder > 0) {
        double buffer[XS256_LANES];
        fill_double_blocks_avx2(s_simd, buffer, 1);
        memcpy(
            dst + num_blocks * XS256_LANES,
            buffer,
            remainder * sizeof(double)
        );
    }

    store_s(s_simd, state);
}

static inline __attribute__((always_inline)) void
fill_32bit_float_range_blocks_common_avx2(
    __m256i s[static 4],
    float* dst,
    __m256 scale,
    __m256 offset,
    size_t count,
    int exact
) {
    const __m256 scale_down = _mm256_set1_ps(XS256_FLOAT_SCALE);

    for (size_t i = 0; i < count; i++) {
        __m256i result = xs256plus_avx2_next(s);
        __m256i u24 = _mm256_srli_epi32(result, 8);
        __m256 f = _mm256_cvtepi32_ps(u24);
        __m256 scaled = exact ? _mm256_mul_ps(f, scale_down) : f;
        scaled = _mm256_mul_ps(scaled, scale);
        __m256 out = _mm256_add_ps(scaled, offset);
        _mm256_storeu_ps(&(dst[i * XS256_LANES * 2]), out);
    }
}

static inline void fill_32bit_float_range_blocks_avx2(
    __m256i s[static 4],
    float* dst,
    __m256 scale,
    __m256 offset,
    size_t count
) {
    fill_32bit_float_range_blocks_common_avx2(s, dst, scale, offset, count, 0);
}

static inline void fill_32bit_float_range_blocks_exact_avx2(
    __m256i s[static 4],
    float* dst,
    __m256 scale,
    __m256 offset,
    size_t count
) {
    fill_32bit_float_range_blocks_common_avx2(s, dst, scale, offset, count, 1);
}

void xs256simd_fill_32bit_float_range_ba_avx2(
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
        fill_32bit_float_range_blocks_avx2(
            s_simd,
            dst,
            scale256,
            offset256,
            num_blocks
        );
        if (remainder > 0) {
            float buffer[XS256_LANES * 2];
            fill_32bit_float_range_blocks_avx2(
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
        fill_32bit_float_range_blocks_exact_avx2(
            s_simd,
            dst,
            scale256,
            offset256,
            num_blocks
        );
        if (remainder > 0) {
            float buffer[XS256_LANES * 2];
            fill_32bit_float_range_blocks_exact_avx2(
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

static inline void fill_double_range_blocks_avx2(
    __m256i s[static 4],
    double* dst,
    __m256d scale,
    __m256d offset,
    size_t count
) {
    const __m256i double_one_bits = _mm256_set1_epi64x(XS256_DOUBLE_ONE_BITS);

    for (size_t i = 0; i < count; i++) {
        // See fill_double_blocks_sse2 in xs256_core_sse2.c.
        __m256i result = xs256plus_avx2_next(s);
        __m256i mant = _mm256_srli_epi64(result, 12);
        __m256i onetwo = _mm256_or_si256(mant, double_one_bits);
        __m256i b = _mm256_srli_epi64(_mm256_slli_epi64(result, 52), 63);
        __m256i one_bits_minus_b = _mm256_sub_epi64(double_one_bits, b);
        __m256d d = _mm256_sub_pd(
            _mm256_castsi256_pd(onetwo),
            _mm256_castsi256_pd(one_bits_minus_b)
        );
        __m256d scaled = _mm256_mul_pd(d, scale);
        __m256d out = _mm256_add_pd(scaled, offset);
        _mm256_storeu_pd(&dst[i * XS256_LANES], out);
    }
}

void xs256simd_fill_double_range_ba_avx2(
    xoshiro256pp* state,
    double* dst,
    double scale,
    double offset,
    size_t length
) {
    __m256i s_simd[XS256_LANES];
    load_s(s_simd, state);

    const __m256d scale256 = _mm256_set1_pd(scale);
    const __m256d offset256 = _mm256_set1_pd(offset);

    size_t num_blocks = length / XS256_LANES;
    size_t remainder = length % XS256_LANES;
    fill_double_range_blocks_avx2(s_simd, dst, scale256, offset256, num_blocks);
    if (remainder > 0) {
        double buffer[XS256_LANES];
        fill_double_range_blocks_avx2(s_simd, buffer, scale256, offset256, 1);
        memcpy(
            dst + num_blocks * XS256_LANES,
            buffer,
            remainder * sizeof(double)
        );
    }

    store_s(s_simd, state);
}

int xs256simd_avx2_is_supported(void) {
    if (__builtin_cpu_supports("avx2")) {
        return 1;
    }
    return 0;
}

#endif // __AVX2__
