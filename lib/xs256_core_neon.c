#if defined(__ARM_NEON)

#include "xs256_core_neon.h"

#include <math.h>
#include <string.h>
#include <arm_neon.h>

#if !defined(__aarch64__)
#include "xs256_core_plain_c.h"
// ARM32 NEON flushes subnormals to 0.
#define XS256_NEON_FLUSHES_SUBNORMALS 1
#if defined(__linux__) || defined(__ANDROID__)
#include <sys/auxv.h>
#include <asm/hwcap.h>
#elif defined(__FreeBSD__) || defined(__OpenBSD__)
#include <sys/auxv.h>
#include <machine/elf.h>
#elif defined(__NetBSD__)
#include <sys/sysctl.h>
#endif // defined(__linux__) || etc
#else  // defined(__aarch64__)
#define XS256_NEON_FLUSHES_SUBNORMALS 0
#endif // !defined(__aarch64__)

#include "xs256_common.h"

#if QWORDS % 4 != 0
#error "NEON is only supported with a multiple of 4 qwords output."
#endif

#define ROTL64(v, n) vsliq_n_u64(vshrq_n_u64(v, 64 - n), v, n)

static inline uint64x2_t xs256plusplus_neon_next(
    uint64x2_t s[static 2 * XS256_LANES],
    size_t lane_div_2
) {
    uint64x2_t s0 = s[lane_div_2];
    uint64x2_t s1 = s[lane_div_2 + (XS256_LANES / 2)];
    uint64x2_t s2 = s[lane_div_2 + 2 * (XS256_LANES / 2)];
    uint64x2_t s3 = s[lane_div_2 + 3 * (XS256_LANES / 2)];

    const uint64x2_t result = vaddq_u64(ROTL64(vaddq_u64(s0, s3), 23), s0);

    const uint64x2_t t = vshlq_n_u64(s1, 17);

    s2 = veorq_u64(s2, s0);
    s3 = veorq_u64(s3, s1);
    s1 = veorq_u64(s1, s2);
    s0 = veorq_u64(s0, s3);

    s2 = veorq_u64(s2, t);

    s3 = ROTL64(s3, 45);

    s[lane_div_2] = s0;
    s[lane_div_2 + (XS256_LANES / 2)] = s1;
    s[lane_div_2 + 2 * (XS256_LANES / 2)] = s2;
    s[lane_div_2 + 3 * (XS256_LANES / 2)] = s3;

    return result;
}

static inline void fill_int_blocks_neon(
    uint64x2_t s_simd[2 * XS256_LANES],
    uint8_t* dst,
    size_t count
) {
    for (size_t i = 0; i < count; i++) {
        for (size_t lane = 0; lane < XS256_LANES / 2; lane++) {
            uint64x2_t result = xs256plusplus_neon_next(s_simd, lane);
            vst1q_u8(
                dst + (i * XS256_LANES + lane * 2) * sizeof(uint64_t),
                vreinterpretq_u8_u64(result)
            );
        }
    }
}

static inline void load_s(
    uint64x2_t s[static XS256_LANES * 2],
    const xoshiro256pp* state
) {
    _Pragma("GCC unroll 8") for (size_t i = 0; i < 8; i++) {
        s[i] = vld1q_u64(&state->s[i * 2]);
    }
}

static inline void store_s(
    uint64x2_t s[static XS256_LANES * 2],
    xoshiro256pp* state
) {
    _Pragma("GCC unroll 8") for (size_t i = 0; i < 8; i++) {
        vst1q_u64(&state->s[i * 2], s[i]);
    }
}

void xs256simd_block_neon(xoshiro256pp* state) {
    uint64x2_t s_simd[2 * XS256_LANES];
    load_s(s_simd, state);

    uint8_t* bitstream = __builtin_assume_aligned(state->bitstream, 8);
    fill_int_blocks_neon(s_simd, bitstream, QWORDS / XS256_LANES);

    store_s(s_simd, state);
}

void xs256simd_fill_blocks_neon(xoshiro256pp* state, void* dst, size_t count) {
    uint64x2_t s_simd[2 * XS256_LANES];
    load_s(s_simd, state);

    fill_int_blocks_neon(s_simd, (uint8_t*)dst, count);

    store_s(s_simd, state);
}

static inline uint64x2_t xs256plus_neon_next(
    uint64x2_t s[static 2 * XS256_LANES],
    size_t lane_div_2
) {
    uint64x2_t s0 = s[lane_div_2];
    uint64x2_t s1 = s[lane_div_2 + (XS256_LANES / 2)];
    uint64x2_t s2 = s[lane_div_2 + 2 * (XS256_LANES / 2)];
    uint64x2_t s3 = s[lane_div_2 + 3 * (XS256_LANES / 2)];

    const uint64x2_t result = vaddq_u64(s0, s3);

    const uint64x2_t t = vshlq_n_u64(s1, 17);

    s2 = veorq_u64(s2, s0);
    s3 = veorq_u64(s3, s1);
    s1 = veorq_u64(s1, s2);
    s0 = veorq_u64(s0, s3);

    s2 = veorq_u64(s2, t);

    s3 = ROTL64(s3, 45);

    s[lane_div_2] = s0;
    s[lane_div_2 + (XS256_LANES / 2)] = s1;
    s[lane_div_2 + 2 * (XS256_LANES / 2)] = s2;
    s[lane_div_2 + 3 * (XS256_LANES / 2)] = s3;

    return result;
}

static inline void fill_32bit_float_blocks_neon(
    uint64x2_t s[static 2 * XS256_LANES],
    float* dst,
    float32x4_t scale,
    size_t count
) {
    for (size_t i = 0; i < count; i++) {
        for (size_t lane = 0; lane < XS256_LANES / 2; lane++) {
            uint64x2_t result = xs256plus_neon_next(s, lane);
            uint32x4_t as32 = vreinterpretq_u32_u64(result);
            uint32x4_t u24 = vshrq_n_u32(as32, 8);
            float32x4_t f = vcvtq_f32_u32(u24);
            float32x4_t scaled = vmulq_f32(f, scale);
            vst1q_f32(&(dst[i * XS256_LANES * 2 + lane * 4]), scaled);
        }
    }
}

void xs256simd_fill_32bit_float_ba_neon(
    xoshiro256pp* state,
    float* dst,
    size_t length
) {
    uint64x2_t s_simd[2 * XS256_LANES];
    load_s(s_simd, state);
    const float32x4_t scale = vdupq_n_f32(XS256_FLOAT_SCALE);

    size_t num_blocks = length / (XS256_LANES * 2);
    size_t remainder = length % (XS256_LANES * 2);
    fill_32bit_float_blocks_neon(s_simd, dst, scale, num_blocks);
    if (remainder > 0) {
        float buffer[XS256_LANES * 2];
        fill_32bit_float_blocks_neon(s_simd, buffer, scale, 1);
        memcpy(
            dst + num_blocks * XS256_LANES * 2,
            buffer,
            remainder * sizeof(float)
        );
    }

    store_s(s_simd, state);
}

static inline __attribute__((always_inline)) void
fill_32bit_float_range_blocks_common_neon(
    uint64x2_t s[static 2 * XS256_LANES],
    float* dst,
    float32x4_t scale,
    float32x4_t offset,
    size_t count,
    int exact
) {
    const float32x4_t scale_down = vdupq_n_f32(XS256_FLOAT_SCALE);

    for (size_t i = 0; i < count; i++) {
        for (size_t lane = 0; lane < XS256_LANES / 2; lane++) {
            uint64x2_t result = xs256plus_neon_next(s, lane);
            uint32x4_t as32 = vreinterpretq_u32_u64(result);
            uint32x4_t u24 = vshrq_n_u32(as32, 8);
            float32x4_t f = vcvtq_f32_u32(u24);
            float32x4_t scaled = exact ? vmulq_f32(f, scale_down) : f;
            float32x4_t out = vmlaq_f32(offset, scaled, scale);
            vst1q_f32(&(dst[i * XS256_LANES * 2 + lane * 4]), out);
        }
    }
}

static inline void fill_32bit_float_range_blocks_neon(
    uint64x2_t s[static 2 * XS256_LANES],
    float* dst,
    float32x4_t scale,
    float32x4_t offset,
    size_t count
) {
    fill_32bit_float_range_blocks_common_neon(s, dst, scale, offset, count, 0);
}

static inline void fill_32bit_float_range_blocks_exact_neon(
    uint64x2_t s[static 2 * XS256_LANES],
    float* dst,
    float32x4_t scale,
    float32x4_t offset,
    size_t count
) {
    fill_32bit_float_range_blocks_common_neon(s, dst, scale, offset, count, 1);
}

void xs256simd_fill_32bit_float_range_ba_neon(
    xoshiro256pp* state,
    float* dst,
    float scale,
    float offset,
    size_t length
) {
#if !defined(__aarch64__)
    // On ARM32 only, NEON flushes subnormals to 0, so we have to use Plain when
    // a result could be subnormal. A small offset of opposite sign can cancel a
    // scaled value into the subnormal range: e.g. scale = 1.5 * 2^-102, offset
    // = -2^-125, u24 = 1 gives -2^-127, which is flushed to -0.0. Requiring
    // abs(offset) >= XS256_FLOAT_SCALE_THRESHOLD in order to use NEON prevents
    // this. Tested in test_backend.ml ("Cancellation to a subnormal").
    if (fabsf(scale) < XS256_FLOAT_SCALE_THRESHOLD ||
        (offset != 0.0f && fabsf(offset) < XS256_FLOAT_SCALE_THRESHOLD)) {
        xs256simd_fill_32bit_float_range_ba_plain(
            state,
            dst,
            scale,
            offset,
            length
        );
        return;
    }
#endif // !defined(__aarch64__)
    uint64x2_t s_simd[2 * XS256_LANES];
    load_s(s_simd, state);

    const float32x4_t offset128 = vdupq_n_f32(offset);

    size_t num_blocks = length / (XS256_LANES * 2);
    size_t remainder = length % (XS256_LANES * 2);

    // On ARM32, the check above has already sent every scale below the
    // threshold to Plain, so only the prescaled loop can be reached here.
    if (XS256_NEON_FLUSHES_SUBNORMALS ||
        fabsf(scale) >= XS256_FLOAT_SCALE_THRESHOLD) {
        const float32x4_t scale128 = vdupq_n_f32(scale * XS256_FLOAT_SCALE);
        fill_32bit_float_range_blocks_neon(
            s_simd,
            dst,
            scale128,
            offset128,
            num_blocks
        );
        if (remainder > 0) {
            float buffer[XS256_LANES * 2];
            fill_32bit_float_range_blocks_neon(
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
        const float32x4_t scale128 = vdupq_n_f32(scale);
        fill_32bit_float_range_blocks_exact_neon(
            s_simd,
            dst,
            scale128,
            offset128,
            num_blocks
        );
        if (remainder > 0) {
            float buffer[XS256_LANES * 2];
            fill_32bit_float_range_blocks_exact_neon(
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

// Only ARM64 NEON has the float64x2_t type, so the code for these
// double operations differs between ARM64 and ARM32.
#if defined(__aarch64__)

static inline void fill_double_blocks_neon(
    uint64x2_t s[static 2 * XS256_LANES],
    double* dst,
    size_t count
) {
    for (size_t i = 0; i < count; i++) {
        for (size_t lane = 0; lane < XS256_LANES / 2; lane++) {
            uint64x2_t result = xs256plus_neon_next(s, lane);
            uint64x2_t mant = vshrq_n_u64(result, 11);
            float64x2_t d = vcvtq_n_f64_u64(mant, 53);
            vst1q_f64(&(dst[i * XS256_LANES + lane * 2]), d);
        }
    }
}

void xs256simd_double_block_neon(xoshiro256pp* state) {
    uint64x2_t s_simd[2 * XS256_LANES];
    load_s(s_simd, state);

    fill_double_blocks_neon(
        s_simd,
        state->doublestream,
        FLOATWORDS / XS256_LANES
    );

    store_s(s_simd, state);
}

void xs256simd_fill_double_ba_neon(
    xoshiro256pp* state,
    double* dst,
    size_t length
) {
    uint64x2_t s_simd[2 * XS256_LANES];
    load_s(s_simd, state);

    size_t num_blocks = length / XS256_LANES;
    size_t remainder = length % XS256_LANES;
    fill_double_blocks_neon(s_simd, dst, num_blocks);
    if (remainder > 0) {
        double buffer[XS256_LANES];
        fill_double_blocks_neon(s_simd, buffer, 1);
        memcpy(
            dst + num_blocks * XS256_LANES,
            buffer,
            remainder * sizeof(double)
        );
    }

    store_s(s_simd, state);
}

static inline void fill_double_range_blocks_neon(
    uint64x2_t s[static 2 * XS256_LANES],
    double* dst,
    float64x2_t scale,
    float64x2_t offset,
    size_t count
) {
    for (size_t i = 0; i < count; i++) {
        for (size_t lane = 0; lane < XS256_LANES / 2; lane++) {
            uint64x2_t result = xs256plus_neon_next(s, lane);
            uint64x2_t mant = vshrq_n_u64(result, 11);
            float64x2_t d = vcvtq_n_f64_u64(mant, 53);
            float64x2_t scaled = vmulq_f64(d, scale);
            float64x2_t out = vaddq_f64(scaled, offset);
            vst1q_f64(&(dst[i * XS256_LANES + lane * 2]), out);
        }
    }
}

void xs256simd_fill_double_range_ba_neon(
    xoshiro256pp* state,
    double* dst,
    double scale,
    double offset,
    size_t length
) {
    uint64x2_t s_simd[2 * XS256_LANES];
    load_s(s_simd, state);

    const float64x2_t scale128 = vdupq_n_f64(scale);
    const float64x2_t offset128 = vdupq_n_f64(offset);

    size_t num_blocks = length / XS256_LANES;
    size_t remainder = length % XS256_LANES;
    fill_double_range_blocks_neon(s_simd, dst, scale128, offset128, num_blocks);
    if (remainder > 0) {
        double buffer[XS256_LANES];
        fill_double_range_blocks_neon(s_simd, buffer, scale128, offset128, 1);
        memcpy(
            dst + num_blocks * XS256_LANES,
            buffer,
            remainder * sizeof(double)
        );
    }

    store_s(s_simd, state);
}

#else // !defined(__aarch64__)

static inline void fill_double_blocks_neon(
    uint64x2_t s[static 2 * XS256_LANES],
    double* dst,
    size_t count
) {
    const uint64x2_t double_one_bits = vdupq_n_u64(XS256_DOUBLE_ONE_BITS);

    for (size_t i = 0; i < count; i++) {
        for (size_t lane = 0; lane < XS256_LANES / 2; lane++) {
            // See fill_double_blocks_sse2 in xs256_core_sse2.c. This is only
            // partly SIMD but uses the same technique.
            uint64x2_t result = xs256plus_neon_next(s, lane);
            uint64x2_t mant = vshrq_n_u64(result, 12);
            uint64x2_t onetwo = vorrq_u64(mant, double_one_bits);
            uint64x2_t b = vshrq_n_u64(vshlq_n_u64(result, 52), 63);
            uint64x2_t one_bits_minus_b = vsubq_u64(double_one_bits, b);
            uint64_t onetwo_bits0 = vgetq_lane_u64(onetwo, 0);
            uint64_t onetwo_bits1 = vgetq_lane_u64(onetwo, 1);
            double onetwo0;
            double onetwo1;
            memcpy(&onetwo0, &onetwo_bits0, sizeof(uint64_t));
            memcpy(&onetwo1, &onetwo_bits1, sizeof(uint64_t));

            uint64_t minus_b_bits0 = vgetq_lane_u64(one_bits_minus_b, 0);
            uint64_t minus_b_bits1 = vgetq_lane_u64(one_bits_minus_b, 1);
            double minus_b0;
            double minus_b1;
            memcpy(&minus_b0, &minus_b_bits0, sizeof(uint64_t));
            memcpy(&minus_b1, &minus_b_bits1, sizeof(uint64_t));

            double d0 = onetwo0 - minus_b0;
            double d1 = onetwo1 - minus_b1;

            dst[i * XS256_LANES + lane * 2] = d0;
            dst[i * XS256_LANES + lane * 2 + 1] = d1;
        }
    }
}

void xs256simd_double_block_neon(xoshiro256pp* state) {
    uint64x2_t s_simd[2 * XS256_LANES];
    // On ARM32, with GCC 12, memcpy produces more efficient code than the
    // load_s/store_s.
    memcpy(s_simd, state->s, sizeof(uint64_t) * XS256_LANES * 4);

    fill_double_blocks_neon(
        s_simd,
        state->doublestream,
        FLOATWORDS / XS256_LANES
    );

    memcpy(state->s, s_simd, sizeof(uint64_t) * XS256_LANES * 4);
}

void xs256simd_fill_double_ba_neon(
    xoshiro256pp* state,
    double* dst,
    size_t length
) {
    uint64x2_t s_simd[2 * XS256_LANES];
    memcpy(s_simd, state->s, sizeof(uint64_t) * XS256_LANES * 4);

    size_t num_blocks = length / XS256_LANES;
    size_t remainder = length % XS256_LANES;
    fill_double_blocks_neon(s_simd, dst, num_blocks);
    if (remainder > 0) {
        double buffer[XS256_LANES];
        fill_double_blocks_neon(s_simd, buffer, 1);
        memcpy(
            dst + num_blocks * XS256_LANES,
            buffer,
            remainder * sizeof(double)
        );
    }

    memcpy(state->s, s_simd, sizeof(uint64_t) * XS256_LANES * 4);
}

static inline void fill_double_range_blocks_neon(
    uint64x2_t s[static 2 * XS256_LANES],
    double* dst,
    double scale,
    double offset,
    size_t count
) {
    const uint64x2_t double_one_bits = vdupq_n_u64(XS256_DOUBLE_ONE_BITS);

    for (size_t i = 0; i < count; i++) {
        for (size_t lane = 0; lane < XS256_LANES / 2; lane++) {
            // See fill_double_blocks_sse2 in xs256_core_sse2.c. This is only
            // partly SIMD but uses the same technique.
            uint64x2_t result = xs256plus_neon_next(s, lane);
            uint64x2_t mant = vshrq_n_u64(result, 12);
            uint64x2_t onetwo = vorrq_u64(mant, double_one_bits);
            uint64x2_t b = vshrq_n_u64(vshlq_n_u64(result, 52), 63);
            uint64x2_t one_bits_minus_b = vsubq_u64(double_one_bits, b);
            uint64_t onetwo_bits0 = vgetq_lane_u64(onetwo, 0);
            uint64_t onetwo_bits1 = vgetq_lane_u64(onetwo, 1);
            double onetwo0;
            double onetwo1;
            memcpy(&onetwo0, &onetwo_bits0, sizeof(uint64_t));
            memcpy(&onetwo1, &onetwo_bits1, sizeof(uint64_t));

            uint64_t minus_b_bits0 = vgetq_lane_u64(one_bits_minus_b, 0);
            uint64_t minus_b_bits1 = vgetq_lane_u64(one_bits_minus_b, 1);
            double minus_b0;
            double minus_b1;
            memcpy(&minus_b0, &minus_b_bits0, sizeof(uint64_t));
            memcpy(&minus_b1, &minus_b_bits1, sizeof(uint64_t));

            double d0 = onetwo0 - minus_b0;
            double d1 = onetwo1 - minus_b1;

            dst[i * XS256_LANES + lane * 2] = d0 * scale + offset;
            dst[i * XS256_LANES + lane * 2 + 1] = d1 * scale + offset;
        }
    }
}

void xs256simd_fill_double_range_ba_neon(
    xoshiro256pp* state,
    double* dst,
    double scale,
    double offset,
    size_t length
) {
    uint64x2_t s_simd[2 * XS256_LANES];
    memcpy(s_simd, state->s, sizeof(uint64_t) * XS256_LANES * 4);

    size_t num_blocks = length / XS256_LANES;
    size_t remainder = length % XS256_LANES;

    fill_double_range_blocks_neon(s_simd, dst, scale, offset, num_blocks);
    if (remainder > 0) {
        double buffer[XS256_LANES];
        fill_double_range_blocks_neon(s_simd, buffer, scale, offset, 1);
        memcpy(
            dst + num_blocks * XS256_LANES,
            buffer,
            remainder * sizeof(double)
        );
    }

    memcpy(state->s, s_simd, sizeof(uint64_t) * XS256_LANES * 4);
}

#endif // defined(__aarch64__)

// Runtime check for NEON. This is more complicated than the x86 SIMD backends
// because ARM32 feature registers can't be read from user mode, so we have to
// use OS calls and each OS has its own.
int xs256simd_neon_is_supported(void) {
#if defined(__aarch64__)
    return 1; // mandatory on ARM64

#elif defined(__linux__) || defined(__ANDROID__)
    return (getauxval(AT_HWCAP) & HWCAP_NEON) != 0;

#elif defined(__FreeBSD__) || defined(__OpenBSD__)
    unsigned long hwcap = 0;
    if (elf_aux_info(AT_HWCAP, &hwcap, sizeof(hwcap)) != 0) {
        return 0;
    }
    return (hwcap & HWCAP_NEON) != 0;

#elif defined(__NetBSD__)
    int neon = 0;
    size_t len = sizeof(neon);
    if (sysctlbyname("machdep.neon_present", &neon, &len, NULL, 0) != 0) {
        return 0;
    }
    return neon != 0;

#elif defined(__APPLE__)
    // All Apple ARM devices that run a 32 bit ARM userspace have NEON.
    return 1;
#else // !defined(__aarch64__) && etc
#error "Unsupported OS for 32 bit ARM NEON detection"
#endif // defined(__aarch64__)
}

#endif // __ARM_NEON
