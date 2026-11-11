#if defined(__SSE2__)

#include "xs256_core_sse2.h"

#include <math.h>
#include <string.h>
#include <immintrin.h>

#include "xs256_common.h"

#if QWORDS % 4 != 0
#error "SSE2 is only supported with a multiple of 4 qwords output."
#endif

static inline __attribute__((always_inline)) __m128i rotl64(__m128i v, int n) {
    __m128i left = _mm_slli_epi64(v, n);
    __m128i right = _mm_srli_epi64(v, 64 - n);
    return _mm_or_si128(left, right);
}

static inline __m128i xs256plusplus_sse2_next(
    __m128i s[static 2 * XS256_LANES],
    size_t lane_div_2
) {
    __m128i s0 = s[lane_div_2];
    __m128i s1 = s[lane_div_2 + (XS256_LANES / 2)];
    __m128i s2 = s[lane_div_2 + 2 * (XS256_LANES / 2)];
    __m128i s3 = s[lane_div_2 + 3 * (XS256_LANES / 2)];

    const __m128i result = _mm_add_epi64(rotl64(_mm_add_epi64(s0, s3), 23), s0);

    const __m128i t = _mm_slli_epi64(s1, 17);

    s2 = _mm_xor_si128(s2, s0);
    s3 = _mm_xor_si128(s3, s1);
    s1 = _mm_xor_si128(s1, s2);
    s0 = _mm_xor_si128(s0, s3);

    s2 = _mm_xor_si128(s2, t);

    s3 = rotl64(s3, 45);

    s[lane_div_2] = s0;
    s[lane_div_2 + (XS256_LANES / 2)] = s1;
    s[lane_div_2 + 2 * (XS256_LANES / 2)] = s2;
    s[lane_div_2 + 3 * (XS256_LANES / 2)] = s3;

    return result;
}

static inline void fill_int_blocks_sse2(
    __m128i s[2 * XS256_LANES],
    uint8_t* dst,
    size_t count
) {
    for (size_t i = 0; i < count; i++) {
        _Pragma("GCC unroll 2") for (size_t lane = 0; lane < XS256_LANES / 2;
                                     lane++) {
            __m128i result = xs256plusplus_sse2_next(s, lane);
            _mm_storeu_si128(
                (__m128i*)(dst + (i * XS256_LANES + lane * 2) * sizeof(uint64_t)
                ),
                result
            );
        }
    }
}

static inline void load_s(
    __m128i s[static XS256_LANES * 2],
    const xoshiro256pp* state
) {
    _Pragma("GCC unroll 8") for (size_t i = 0; i < 8; i++) {
        s[i] = _mm_loadu_si128((const __m128i*)&state->s[i * 2]);
    }
}

static inline void store_s(
    __m128i s[static XS256_LANES * 2],
    xoshiro256pp* state
) {
    _Pragma("GCC unroll 8") for (size_t i = 0; i < 8; i++) {
        _mm_storeu_si128((__m128i*)&state->s[i * 2], s[i]);
    }
}

void xs256simd_block_sse2(xoshiro256pp* state) {
    __m128i s_simd[2 * XS256_LANES];
    load_s(s_simd, state);

    fill_int_blocks_sse2(
        s_simd,
        (uint8_t*)state->bitstream,
        QWORDS / XS256_LANES
    );

    store_s(s_simd, state);
}

void xs256simd_fill_blocks_sse2(xoshiro256pp* state, void* dst, size_t count) {
    __m128i s_simd[2 * XS256_LANES];
    load_s(s_simd, state);

    fill_int_blocks_sse2(s_simd, (uint8_t*)dst, count);

    store_s(s_simd, state);
}

static inline __m128i xs256plus_sse2_next(
    __m128i s[static 2 * XS256_LANES],
    size_t lane_div_2
) {
    __m128i s0 = s[lane_div_2];
    __m128i s1 = s[lane_div_2 + (XS256_LANES / 2)];
    __m128i s2 = s[lane_div_2 + 2 * (XS256_LANES / 2)];
    __m128i s3 = s[lane_div_2 + 3 * (XS256_LANES / 2)];

    const __m128i result = _mm_add_epi64(s0, s3);

    const __m128i t = _mm_slli_epi64(s1, 17);

    s2 = _mm_xor_si128(s2, s0);
    s3 = _mm_xor_si128(s3, s1);
    s1 = _mm_xor_si128(s1, s2);
    s0 = _mm_xor_si128(s0, s3);

    s2 = _mm_xor_si128(s2, t);

    s3 = rotl64(s3, 45);

    s[lane_div_2] = s0;
    s[lane_div_2 + (XS256_LANES / 2)] = s1;
    s[lane_div_2 + 2 * (XS256_LANES / 2)] = s2;
    s[lane_div_2 + 3 * (XS256_LANES / 2)] = s3;

    return result;
}

static inline void fill_double_blocks_sse2(
    __m128i s[static 2 * XS256_LANES],
    double* dst,
    size_t count
) {
    const __m128i double_one_bits = _mm_set1_epi64x(XS256_DOUBLE_ONE_BITS);

    for (size_t i = 0; i < count; i++) {
        _Pragma("GCC unroll 2") for (size_t lane = 0; lane < XS256_LANES / 2;
                                     lane++) {
            // Generate 53 bit random doubles on hardware without a dedicated
            // float conversion instruction like AVX512's _mm256_cvtepu64_pd or
            // NEON64's vcvtq_n_f64_u64.
            //
            // Let w be the 64 bit word, and let W = w >> 11 = 2M + b, with
            // M = w >> 12 and b = bit 11 of w, so W = 2*M + b. We want
            // d = W * 2^-53, which = M*2^-52 + b*2^-53.
            //
            // OR M into the bits of 1.0 to get onetwo = 1 + M*2^-52 (dSFMT's
            // 52 bit trick). Then subtract the double whose bits are
            // ONE_BITS - b: that is 1.0 when b = 0, and the next double below
            // 1.0 (1 - 2^-52) when b = 1, because positive doubles order like
            // their bit patterns. The result is M*2^-52 + b*2^-53 = W*2^-53.
            //
            // By Sterbenz's lemma, if y <= x <= 2y, then x - y is exactly
            // representable. x is in [1, 2 - 2^-52] and y is 1 or 1 - 2^-53,
            // and 2(1 - 2^-53) = 2 - 2^-52. Therefore, the subtraction is
            // exact. test_backend.ml compares all SIMD float64 output against
            // Plain.
            __m128i result = xs256plus_sse2_next(s, lane);
            __m128i mant = _mm_srli_epi64(result, 12);
            __m128i onetwo = _mm_or_si128(mant, double_one_bits);
            __m128i b = _mm_srli_epi64(_mm_slli_epi64(result, 52), 63);
            __m128i one_bits_minus_b = _mm_sub_epi64(double_one_bits, b);
            __m128d d = _mm_sub_pd(
                _mm_castsi128_pd(onetwo),
                _mm_castsi128_pd(one_bits_minus_b)
            );

            _mm_storeu_pd(&(dst[i * XS256_LANES + lane * 2]), d);
        }
    }
}

void xs256simd_double_block_sse2(xoshiro256pp* state) {
    __m128i s_simd[2 * XS256_LANES];
    load_s(s_simd, state);

    fill_double_blocks_sse2(
        s_simd,
        state->doublestream,
        FLOATWORDS / XS256_LANES
    );

    store_s(s_simd, state);
}

static inline void fill_32bit_float_blocks_sse2(
    __m128i s[static 2 * XS256_LANES],
    float* dst,
    __m128 scale,
    size_t count
) {
    for (size_t i = 0; i < count; i++) {
        _Pragma("GCC unroll 2") for (size_t lane = 0; lane < XS256_LANES / 2;
                                     lane++) {
            __m128i result = xs256plus_sse2_next(s, lane);
            __m128i u24 = _mm_srli_epi32(result, 8);
            __m128 f = _mm_cvtepi32_ps(u24);
            __m128 scaled = _mm_mul_ps(f, scale);
            _mm_storeu_ps(&(dst[i * XS256_LANES * 2 + lane * 4]), scaled);
        }
    }
}

void xs256simd_fill_32bit_float_ba_sse2(
    xoshiro256pp* state,
    float* dst,
    size_t length
) {
    __m128i s_simd[2 * XS256_LANES];
    load_s(s_simd, state);
    const __m128 scale = _mm_set1_ps(XS256_FLOAT_SCALE);

    size_t num_blocks = length / (XS256_LANES * 2);
    size_t remainder = length % (XS256_LANES * 2);
    fill_32bit_float_blocks_sse2(s_simd, dst, scale, num_blocks);
    if (remainder > 0) {
        float buffer[XS256_LANES * 2];
        fill_32bit_float_blocks_sse2(s_simd, buffer, scale, 1);
        memcpy(
            dst + num_blocks * XS256_LANES * 2,
            buffer,
            remainder * sizeof(float)
        );
    }

    store_s(s_simd, state);
}

void xs256simd_fill_double_ba_sse2(
    xoshiro256pp* state,
    double* dst,
    size_t length
) {
    __m128i s_simd[2 * XS256_LANES];
    load_s(s_simd, state);

    size_t num_blocks = length / XS256_LANES;
    size_t remainder = length % XS256_LANES;
    fill_double_blocks_sse2(s_simd, dst, num_blocks);
    if (remainder > 0) {
        double buffer[XS256_LANES];
        fill_double_blocks_sse2(s_simd, buffer, 1);
        memcpy(
            dst + num_blocks * XS256_LANES,
            buffer,
            remainder * sizeof(double)
        );
    }

    store_s(s_simd, state);
}

static inline __attribute__((always_inline)) void
fill_32bit_float_range_blocks_common_sse2(
    __m128i s[static 2 * XS256_LANES],
    float* dst,
    __m128 scale,
    __m128 offset,
    size_t count,
    int exact
) {
    const __m128 scale_down = _mm_set1_ps(XS256_FLOAT_SCALE);

    for (size_t i = 0; i < count; i++) {
        _Pragma("GCC unroll 2") for (size_t lane = 0; lane < XS256_LANES / 2;
                                     lane++) {
            __m128i result = xs256plus_sse2_next(s, lane);
            __m128i u24 = _mm_srli_epi32(result, 8);
            __m128 f = _mm_cvtepi32_ps(u24);
            __m128 scaled = exact ? _mm_mul_ps(f, scale_down) : f;
            scaled = _mm_mul_ps(scaled, scale);
            __m128 out = _mm_add_ps(scaled, offset);
            _mm_storeu_ps(&(dst[i * XS256_LANES * 2 + lane * 4]), out);
        }
    }
}

static inline void fill_32bit_float_range_blocks_sse2(
    __m128i s[static 2 * XS256_LANES],
    float* dst,
    __m128 scale,
    __m128 offset,
    size_t count
) {
    fill_32bit_float_range_blocks_common_sse2(s, dst, scale, offset, count, 0);
}

static inline void fill_32bit_float_range_blocks_exact_sse2(
    __m128i s[static 2 * XS256_LANES],
    float* dst,
    __m128 scale,
    __m128 offset,
    size_t count
) {
    fill_32bit_float_range_blocks_common_sse2(s, dst, scale, offset, count, 1);
}

void xs256simd_fill_32bit_float_range_ba_sse2(
    xoshiro256pp* state,
    float* dst,
    float scale,
    float offset,
    size_t length
) {
    __m128i s_simd[2 * XS256_LANES];
    load_s(s_simd, state);

    const __m128 offset128 = _mm_set1_ps(offset);

    size_t num_blocks = length / (XS256_LANES * 2);
    size_t remainder = length % (XS256_LANES * 2);

    if (fabsf(scale) >= XS256_FLOAT_SCALE_THRESHOLD) {
        const __m128 scale128 = _mm_set1_ps(scale * XS256_FLOAT_SCALE);
        fill_32bit_float_range_blocks_sse2(
            s_simd,
            dst,
            scale128,
            offset128,
            num_blocks
        );
        if (remainder > 0) {
            float buffer[XS256_LANES * 2];
            fill_32bit_float_range_blocks_sse2(
                s_simd,
                buffer,
                scale128,
                offset128,
                1
            );
            memcpy(
                dst + num_blocks * XS256_LANES * 2,
                buffer,
                remainder * sizeof(float)
            );
        }
    } else {
        const __m128 scale128 = _mm_set1_ps(scale);
        fill_32bit_float_range_blocks_exact_sse2(
            s_simd,
            dst,
            scale128,
            offset128,
            num_blocks
        );
        if (remainder > 0) {
            float buffer[XS256_LANES * 2];
            fill_32bit_float_range_blocks_exact_sse2(
                s_simd,
                buffer,
                scale128,
                offset128,
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

static inline void fill_double_range_blocks_sse2(
    __m128i s[static 2 * XS256_LANES],
    double* dst,
    __m128d scale,
    __m128d offset,
    size_t count
) {
    const __m128i double_one_bits = _mm_set1_epi64x(XS256_DOUBLE_ONE_BITS);

    for (size_t i = 0; i < count; i++) {
        _Pragma("GCC unroll 2") for (size_t lane = 0; lane < XS256_LANES / 2;
                                     lane++) {
            // See the comment in fill_double_blocks_sse2 above.
            __m128i result = xs256plus_sse2_next(s, lane);
            __m128i mant = _mm_srli_epi64(result, 12);
            __m128i onetwo = _mm_or_si128(mant, double_one_bits);
            __m128i b = _mm_srli_epi64(_mm_slli_epi64(result, 52), 63);
            __m128i one_bits_minus_b = _mm_sub_epi64(double_one_bits, b);
            __m128d d = _mm_sub_pd(
                _mm_castsi128_pd(onetwo),
                _mm_castsi128_pd(one_bits_minus_b)
            );

            __m128d scaled = _mm_mul_pd(d, scale);
            __m128d out = _mm_add_pd(scaled, offset);
            _mm_storeu_pd(&(dst[i * XS256_LANES + lane * 2]), out);
        }
    }
}

void xs256simd_fill_double_range_ba_sse2(
    xoshiro256pp* state,
    double* dst,
    double scale,
    double offset,
    size_t length
) {
    __m128i s_simd[2 * XS256_LANES];
    load_s(s_simd, state);

    const __m128d scale128 = _mm_set1_pd(scale);
    const __m128d offset128 = _mm_set1_pd(offset);

    size_t num_blocks = length / XS256_LANES;
    size_t remainder = length % XS256_LANES;
    fill_double_range_blocks_sse2(s_simd, dst, scale128, offset128, num_blocks);
    if (remainder > 0) {
        double buffer[XS256_LANES];
        fill_double_range_blocks_sse2(s_simd, buffer, scale128, offset128, 1);
        memcpy(
            dst + num_blocks * XS256_LANES,
            buffer,
            remainder * sizeof(double)
        );
    }

    store_s(s_simd, state);
}

int xs256simd_sse2_is_supported(void) {
#if defined(__x86_64__)
    return 1; // mandatory on x86-64
#else         // !defined(__x86_64__)
    if (__builtin_cpu_supports("sse2")) {
        return 1;
    }
    return 0;
#endif        // defined(__x86_64__)
}

#endif // __SSE2__
