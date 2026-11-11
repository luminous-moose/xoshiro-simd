#ifndef XOSHIRO_SIMD_DISPATCH_H
#define XOSHIRO_SIMD_DISPATCH_H

#include "xs256_common.h"
#include "xs256_core_plain_c.h"

#if defined(XOSHIRO_SIMD_HAS_SSE2)
#include "xs256_core_sse2.h"
#endif

#if defined(XOSHIRO_SIMD_HAS_AVX2)
#include "xs256_core_avx2.h"
#endif

#if defined(XOSHIRO_SIMD_HAS_AVX512FVL)
#include "xs256_core_avx512fvl.h"
#endif

#if defined(XOSHIRO_SIMD_HAS_NEON)
#include "xs256_core_neon.h"
#endif

int xs256simd_dispatch_init(
    xoshiro256pp* state,
    xoshiro256pp_backend requested
);

void xs256simd_set_backend_limit(xoshiro256pp_backend limit_id);

#define XS256_MAX_BACKENDS 5

size_t xs256simd_available_backends(xoshiro256pp_backend out[]);

#endif // XOSHIRO_SIMD_DISPATCH_H
