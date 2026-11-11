#ifndef XOSHIRO_SIMD_CORE_SSE2_H
#define XOSHIRO_SIMD_CORE_SSE2_H

#include "xs256_common.h"

void xs256simd_block_sse2(xoshiro256pp* state);
void xs256simd_double_block_sse2(xoshiro256pp* state);
void xs256simd_fill_blocks_sse2(xoshiro256pp* state, void* dst, size_t count);
void xs256simd_fill_32bit_float_ba_sse2(
    xoshiro256pp* state,
    float* dst,
    size_t length
);
void xs256simd_fill_double_ba_sse2(
    xoshiro256pp* state,
    double* dst,
    size_t length
);
void xs256simd_fill_32bit_float_range_ba_sse2(
    xoshiro256pp* state,
    float* dst,
    float scale,
    float offset,
    size_t length
);
void xs256simd_fill_double_range_ba_sse2(
    xoshiro256pp* state,
    double* dst,
    double scale,
    double offset,
    size_t length
);
int xs256simd_sse2_is_supported(void);

#endif // XOSHIRO_SIMD_CORE_SSE2_H
