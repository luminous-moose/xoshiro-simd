#include "xs256_dispatch.h"

#include <stdlib.h>
#include <stddef.h>
#include <stdatomic.h>

#include "xs256_common.h"

// The backend limit is threadsafe by design, unlike the state itself, as it is
// intended to affect the entire process. It holds an index into backend_table.
// AUTO's search starts with index xs256_backend_limit to allow preventing
// backends from being automatically selected.
static _Atomic int xs256_backend_limit = 0;

static void set_backend_limit(int limit) {
    atomic_store(&xs256_backend_limit, limit);
}

static int get_backend_limit(void) { return atomic_load(&xs256_backend_limit); }

typedef struct {
    xoshiro256pp_backend id;
    void (*block_fn)(xoshiro256pp* state);
    void (*double_block_fn)(xoshiro256pp* state);
    void (*fill_blocks_fn)(
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
    int (*is_supported)(void);
} backend_entry;

// Ordered best first for AUTO. This file is built without -m flags to enable
// producing code that will work without SIMD hardware, so we can't test for
// __AVX2__ and similar. XOSHIRO_SIMD_HAS_<BACKEND> defines are set in dune
// based on ISA and tell this file what backends have been compiled, and
// *_is_supported checks the CPU at runtime.
static const backend_entry backend_table[] = {
#if defined(XOSHIRO_SIMD_HAS_NEON)
    {XOSHIRO_SIMD_BACKEND_NEON,
     xs256simd_block_neon,
     xs256simd_double_block_neon,
     xs256simd_fill_blocks_neon,
     xs256simd_fill_32bit_float_ba_neon,
     xs256simd_fill_double_ba_neon,
     xs256simd_fill_32bit_float_range_ba_neon,
     xs256simd_fill_double_range_ba_neon,
     xs256simd_neon_is_supported},
#endif // defined(XOSHIRO_SIMD_HAS_NEON)
#if defined(XOSHIRO_SIMD_HAS_AVX512FVL)
    {XOSHIRO_SIMD_BACKEND_AVX512FVL,
     xs256simd_block_avx512fvl,
     xs256simd_double_block_avx512fvl,
     xs256simd_fill_blocks_avx512fvl,
     xs256simd_fill_32bit_float_ba_avx512fvl,
     xs256simd_fill_double_ba_avx512fvl,
     xs256simd_fill_32bit_float_range_ba_avx512fvl,
     xs256simd_fill_double_range_ba_avx512fvl,
     xs256simd_avx512fvl_is_supported},
#endif // defined(XOSHIRO_SIMD_HAS_AVX512FVL)
#if defined(XOSHIRO_SIMD_HAS_AVX2)
    {XOSHIRO_SIMD_BACKEND_AVX2,
     xs256simd_block_avx2,
     xs256simd_double_block_avx2,
     xs256simd_fill_blocks_avx2,
     xs256simd_fill_32bit_float_ba_avx2,
     xs256simd_fill_double_ba_avx2,
     xs256simd_fill_32bit_float_range_ba_avx2,
     xs256simd_fill_double_range_ba_avx2,
     xs256simd_avx2_is_supported},
#endif // defined(XOSHIRO_SIMD_HAS_AVX2)
#if defined(XOSHIRO_SIMD_HAS_SSE2)
    {XOSHIRO_SIMD_BACKEND_SSE2,
     xs256simd_block_sse2,
     xs256simd_double_block_sse2,
     xs256simd_fill_blocks_sse2,
     xs256simd_fill_32bit_float_ba_sse2,
     xs256simd_fill_double_ba_sse2,
     xs256simd_fill_32bit_float_range_ba_sse2,
     xs256simd_fill_double_range_ba_sse2,
     xs256simd_sse2_is_supported},
#endif // defined(XOSHIRO_SIMD_HAS_SSE2)
    {XOSHIRO_SIMD_BACKEND_PLAIN,
     xs256simd_block_plain,
     xs256simd_double_block_plain,
     xs256simd_fill_blocks_plain,
     xs256simd_fill_32bit_float_ba_plain,
     xs256simd_fill_double_ba_plain,
     xs256simd_fill_32bit_float_range_ba_plain,
     xs256simd_fill_double_range_ba_plain,
     xs256simd_plain_is_supported},
};

#define N_BACKENDS (sizeof(backend_table) / sizeof(backend_table[0]))
_Static_assert(
    N_BACKENDS <= XS256_MAX_BACKENDS,
    "XS256_MAX_BACKENDS is smaller than backend_table"
);

void xs256simd_set_backend_limit(xoshiro256pp_backend limit_id) {
    if (limit_id == XOSHIRO_SIMD_BACKEND_AUTO) {
        set_backend_limit(0);
        return;
    }
    for (size_t i = 0; i < N_BACKENDS; i++) {
        backend_entry entry = backend_table[i];
        if (entry.id == limit_id) {
            set_backend_limit((int)i);
            return;
        }
    }
}

static void apply_backend(xoshiro256pp* state, const backend_entry* entry) {
    state->xs256_block_fn = entry->block_fn;
    state->xs256_double_block_fn = entry->double_block_fn;
    state->xs256_fill_blocks_fn = entry->fill_blocks_fn;
    state->xs256_fill_32bit_float_ba_fn = entry->xs256_fill_32bit_float_ba_fn;
    state->xs256_fill_double_ba_fn = entry->xs256_fill_double_ba_fn;
    state->xs256_fill_32bit_float_range_ba_fn =
        entry->xs256_fill_32bit_float_range_ba_fn;
    state->xs256_fill_double_range_ba_fn = entry->xs256_fill_double_range_ba_fn;
    state->backend_id = entry->id;
}

int xs256simd_dispatch_init(
    xoshiro256pp* state,
    xoshiro256pp_backend requested
) {
    if (requested == XOSHIRO_SIMD_BACKEND_AUTO) {
        for (size_t i = get_backend_limit(); i < N_BACKENDS; i++) {
            if (backend_table[i].is_supported()) {
                apply_backend(state, &backend_table[i]);
                return 0;
            }
        }
        // PLAIN is always the last entry in backend_table and is always
        // supported, so this is unreachable in practice.
        abort();
    }

    for (size_t i = 0; i < N_BACKENDS; i++) {
        if (backend_table[i].id == requested) {
            if (!backend_table[i].is_supported()) {
                return XS256_BACKEND_NOT_SUPPORTED;
            }
            apply_backend(state, &backend_table[i]);
            return 0;
        }
    }

    return XS256_BACKEND_NOT_COMPILED;
}

size_t xs256simd_available_backends(xoshiro256pp_backend out[]) {
    size_t n = 0;
    for (size_t i = 0; i < N_BACKENDS; i++) {
        if (backend_table[i].is_supported()) {
            out[n++] = backend_table[i].id;
        }
    }
    return n;
}
