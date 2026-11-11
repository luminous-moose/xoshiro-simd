#define CAML_NAME_SPACE
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/version.h>

#include <stdlib.h>
#include <assert.h>

#include "xs256_common.h"
#include "xs256_dispatch.h"

// Custom OCaml block: state wrapper.

#define Xs256_state_val(v) (*((xoshiro256pp**)Data_custom_val(v)))

static void xs256_finalize(value v) {
    xoshiro256pp* state = Xs256_state_val(v);
    if (state != NULL) {
        free(state);
    }
}

static struct custom_operations xs256_ops = {
    "xoshiro_simd.xs256_v1",
    xs256_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

static value create_state_uninitialized(void) {
    CAMLparam0();
    CAMLlocal1(v);

    v = caml_alloc_custom_mem(
        &xs256_ops,
        sizeof(xoshiro256pp*),
        sizeof(xoshiro256pp)
    );
    Xs256_state_val(v) = NULL;
    xoshiro256pp* state = malloc(sizeof(xoshiro256pp));
    if (state == NULL) {
        caml_raise_out_of_memory();
    }
    Xs256_state_val(v) = state;
    CAMLreturn(v);
}

static value create_bitstream_ba(xoshiro256pp* state) {
    CAMLparam0();
    CAMLlocal1(v);

    intnat dims[1];
    dims[0] = QWORDS * 2;

    // CAML_BA_EXTERNAL: OCaml does not own or free the data, the pointer
    // aliases directly into state->bitstream.
    v = caml_ba_alloc(
        CAML_BA_INT32 | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL,
        1,
        state->bitstream,
        dims
    );

    CAMLreturn(v);
}

static value create_doublestream_ba(xoshiro256pp* state) {
    CAMLparam0();
    CAMLlocal1(v);

    intnat dims[1];
    dims[0] = FLOATWORDS;

    v = caml_ba_alloc(
        CAML_BA_FLOAT64 | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL,
        1,
        state->doublestream,
        dims
    );

    CAMLreturn(v);
}

// External interface: state creation and query.

CAMLprim value xs256simdintf_available_backends(value unit_v) {
    CAMLparam1(unit_v);
    CAMLlocal2(list_v, cell_v);

    xoshiro256pp_backend backends[XS256_MAX_BACKENDS];

    size_t n = xs256simd_available_backends(backends);
    list_v = Val_emptylist;

    for (size_t i = n; i-- > 0;) {
        cell_v = caml_alloc(2, Tag_cons);
        Store_field(cell_v, 0, Val_long((long)backends[i]));
        Store_field(cell_v, 1, list_v);
        list_v = cell_v;
    }

    CAMLreturn(list_v);
}

static value setup_common(value state_v, xoshiro256pp* state) {
    CAMLparam1(state_v);
    CAMLlocal3(bitstream_v, doublestream_v, tuple_v);

    bitstream_v = create_bitstream_ba(state);
    doublestream_v = create_doublestream_ba(state);

    tuple_v = caml_alloc_tuple(3);

    Store_field(tuple_v, 0, state_v);
    Store_field(tuple_v, 1, bitstream_v);
    Store_field(tuple_v, 2, doublestream_v);

    CAMLreturn(tuple_v);
}

// Only stubs are allowed to raise OCaml exceptions: every other C function must
// return an error code if it can error so callers can clean up where they need
// to.
static __attribute__((noreturn)) void setup_fail(int retval) {
    switch (retval) {
    case XS256_ENTROPY_READ_ERROR:
        caml_failwith(
            "Xoshiro_simd: error initializing PRNG: entropy read failed"
        );
    case XS256_ENTROPY_ALL_ZERO:
        caml_failwith(
            "Xoshiro_simd: error initializing PRNG: entropy returned all 0"
        );
    case XS256_BACKEND_NOT_COMPILED:
        caml_invalid_argument("Xoshiro_simd: backend is not compiled");
    case XS256_BACKEND_NOT_SUPPORTED:
        caml_invalid_argument("Xoshiro_simd: backend is not supported");
    default:
        // should never be reached
        abort();
    }
}

CAMLprim value xs256simdintf_create_initialized(value backend_v) {
    CAMLparam1(backend_v);
    CAMLlocal2(state_v, tuple_v);

    state_v = create_state_uninitialized();

    xoshiro256pp* state = Xs256_state_val(state_v);
    xoshiro256pp_backend backend = (xoshiro256pp_backend)Long_val(backend_v);

    int retval = xs256simd_setup_from_entropy(state, backend);
    if (retval < 0) {
        setup_fail(retval);
    }

    tuple_v = setup_common(state_v, state);

    CAMLreturn(tuple_v);
}

CAMLprim value
xs256simdintf_internal_create_from_seed(value seed_v, value backend_v) {
    CAMLparam2(seed_v, backend_v);
    CAMLlocal2(state_v, tuple_v);

    state_v = create_state_uninitialized();

    uint64_t seed = (uint64_t)Int64_val(seed_v);
    xoshiro256pp_backend backend = (xoshiro256pp_backend)Long_val(backend_v);

    xoshiro256pp* state = Xs256_state_val(state_v);
    int retval = xs256simd_setup(state, seed, backend);
    if (retval < 0) {
        setup_fail(retval);
    }

    tuple_v = setup_common(state_v, state);

    CAMLreturn(tuple_v);
}

CAMLprim value
xs256simdintf_internal_create_from_string(value string_v, value backend_v) {
    CAMLparam2(string_v, backend_v);
    CAMLlocal2(state_v, tuple_v);

    size_t size = caml_string_length(string_v);
    if (size == 0) {
        caml_invalid_argument(
            "Xoshiro_simd: cannot initialize from empty string"
        );
    }

    xoshiro256pp_backend backend = (xoshiro256pp_backend)Long_val(backend_v);

    state_v = create_state_uninitialized();

    const uint8_t* bytes = Bytes_val(string_v);

    xoshiro256pp* state = Xs256_state_val(state_v);
    int retval = xs256simd_setup_from_string(state, bytes, size, backend);
    if (retval < 0) {
        setup_fail(retval);
    }

    tuple_v = setup_common(state_v, state);

    CAMLreturn(tuple_v);
}

CAMLprim value
xs256simdintf_internal_create_from_array(value intarray_v, value backend_v) {
    CAMLparam2(intarray_v, backend_v);
    CAMLlocal2(state_v, tuple_v);

    mlsize_t len = Wosize_val(intarray_v);
    if (len == 0) {
        caml_invalid_argument("Xoshiro_simd: cannot initialize from empty array"
        );
    }

    xoshiro256pp_backend backend = (xoshiro256pp_backend)Long_val(backend_v);

    state_v = create_state_uninitialized();

    size_t size = (size_t)len * sizeof(uint64_t);
    uint64_t* seed_words = malloc(size);
    assert(seed_words != NULL);
    for (mlsize_t i = 0; i < len; i++) {
        value element = Field(intarray_v, i);
        seed_words[i] = (uint64_t)Int64_val(element);
    }

    xoshiro256pp* state = Xs256_state_val(state_v);
    int retval = xs256simd_setup_from_array(state, seed_words, len, backend);
    free(seed_words);
    if (retval < 0) {
        setup_fail(retval);
    }

    tuple_v = setup_common(state_v, state);

    CAMLreturn(tuple_v);
}

CAMLprim value xs256simdintf_state_backend(value state_v) {
    CAMLparam1(state_v);
    xoshiro256pp* state = Xs256_state_val(state_v);
    CAMLreturn(Val_long((long)state->backend_id));
}

CAMLprim value xs256simdintf_set_backend_limit(value backend_v) {
    xoshiro256pp_backend backend = (xoshiro256pp_backend)Long_val(backend_v);
    xs256simd_set_backend_limit(backend);
    return Val_unit;
}

// Build-time constants exported to OCaml.

CAMLprim value xs256simdintf_int_length(value unit_v) {
    CAMLparam1(unit_v);
    CAMLreturn(Val_long(QWORDS * 2));
}

CAMLprim value xs256simdintf_float_length(value unit_v) {
    CAMLparam1(unit_v);
    CAMLreturn(Val_long(FLOATWORDS));
}

CAMLprim value xs256simdintf_backend_ids(value unit_v) {
    CAMLparam1(unit_v);
    CAMLlocal1(ids_v);
    ids_v = caml_alloc(6, 0);
    Store_field(ids_v, 0, Val_long(XOSHIRO_SIMD_BACKEND_AUTO));
    Store_field(ids_v, 1, Val_long(XOSHIRO_SIMD_BACKEND_PLAIN));
    Store_field(ids_v, 2, Val_long(XOSHIRO_SIMD_BACKEND_SSE2));
    Store_field(ids_v, 3, Val_long(XOSHIRO_SIMD_BACKEND_AVX2));
    Store_field(ids_v, 4, Val_long(XOSHIRO_SIMD_BACKEND_AVX512FVL));
    Store_field(ids_v, 5, Val_long(XOSHIRO_SIMD_BACKEND_NEON));
    CAMLreturn(ids_v);
}

// External interface: RNG operations.

CAMLprim value xs256simdintf_reseed(value state_v) {
    CAMLparam1(state_v);

    xoshiro256pp* state = Xs256_state_val(state_v);
    int retval = xs256simd_seed_from_entropy(state);
    if (retval < 0) {
        setup_fail(retval);
    }

    CAMLreturn(Val_unit);
}

CAMLprim value xs256simdintf_refill_block(value state_v) {
    xoshiro256pp* state = Xs256_state_val(state_v);
    state->xs256_block_fn(state);
    return Val_unit;
}

CAMLprim value xs256simdintf_refill_float_block(value state_v) {
    xoshiro256pp* state = Xs256_state_val(state_v);
    state->xs256_double_block_fn(state);
    return Val_unit;
}

// This function takes x_i32 unboxed and the bound and result untagged, which is
// why it looks different than other OCaml C stubs. Returns -1 when the draw
// must be rejected, and the OCaml function int_lemire_c_loop retries with a new
// word.
intnat xs256simdintf_lemire_32bits(int32_t x_i32, intnat bound_n) {
    uint32_t x = (uint32_t)x_i32;
    uint32_t bound = (uint32_t)bound_n;
    uint64_t m = (uint64_t)x * (uint64_t)bound;
    uint32_t l = (uint32_t)m;
    if (l < bound) {
        uint32_t t = (0U - bound) % bound;
        if (l < t) {
            return -1;
        }
    }
    return (intnat)(m >> 32);
}

CAMLprim value xs256simdintf_lemire_32bits_byte(value x_v, value bound_v) {
    return Val_long(
        xs256simdintf_lemire_32bits(Int32_val(x_v), Long_val(bound_v))
    );
}

// This function returns identical bytes on LE and BE systems by byteswapping on
// BE systems using xs256simd_fill_bytes_be_int8.
CAMLprim value
xs256simdintf_get_bytes(value state_v, value bytes_v, value len_v) {
    uint8_t* buffer = (uint8_t*)Bytes_val(bytes_v);
    size_t len = Int_val(len_v);
#if XS256_BIG_ENDIAN
    xs256simd_fill_bytes_be_int8(Xs256_state_val(state_v), buffer, len);
#else  // !XS256_BIG_ENDIAN
    xs256simd_fill_bytes(Xs256_state_val(state_v), buffer, len);
#endif // XS256_BIG_ENDIAN
    return Val_unit;
}

// This function returns identical values on LE and BE systems. Calling it with
// CAML_BA_CAML_INT or CAML_BA_NATIVE_INT types only will produce different
// results on 32 bit and 64 bit platforms: every other array type will produce
// identical output on all platforms.
CAMLprim value xs256simdintf_get_bytes_ba(value state_v, value ba_v) {
    uint8_t* buffer = (uint8_t*)Caml_ba_data_val(ba_v);
    size_t len = caml_ba_byte_size(Caml_ba_array_val(ba_v));
#if !XS256_BIG_ENDIAN
    xs256simd_fill_bytes(Xs256_state_val(state_v), buffer, len);
#else // XS256_BIG_ENDIAN
    // The LE path is the reference. The BE path has to give the same output as
    // it so that the OCaml values are identical. To do this, it calls different
    // functions based on the element type width that swap bytes as necessary.
    struct caml_ba_array* ba = Caml_ba_array_val(ba_v);
    enum caml_ba_kind kind = ba->flags & CAML_BA_KIND_MASK;
    switch (kind) {
    case CAML_BA_SINT8:
    case CAML_BA_UINT8:
    case CAML_BA_CHAR:
        xs256simd_fill_bytes_be_int8(Xs256_state_val(state_v), buffer, len);
        break;
    case CAML_BA_SINT16:
    case CAML_BA_UINT16:
#if OCAML_VERSION >= 50200
    // CAML_BA_FLOAT16 was only added to the enum in 5.2.
    case CAML_BA_FLOAT16:
#endif
        xs256simd_fill_bytes_be_int16(Xs256_state_val(state_v), buffer, len);
        break;
    case CAML_BA_INT32:
    // We include float types not because they make much sense to use with this
    // function, but because the check is basically free and gives us identical
    // values on both byte orders.
    case CAML_BA_FLOAT32:
    case CAML_BA_COMPLEX32:
#ifndef ARCH_SIXTYFOUR
    // int and nativeint follow the word size, so here the output must differ
    // between 32 bit and 64 hosts by design.
    case CAML_BA_CAML_INT:
    case CAML_BA_NATIVE_INT:
#endif
        xs256simd_fill_bytes_be_int32(Xs256_state_val(state_v), buffer, len);
        break;
    case CAML_BA_INT64:
    case CAML_BA_FLOAT64:
    case CAML_BA_COMPLEX64:
#ifdef ARCH_SIXTYFOUR
    case CAML_BA_CAML_INT:
    case CAML_BA_NATIVE_INT:
#endif
        xs256simd_fill_bytes(Xs256_state_val(state_v), buffer, len);
        break;
    default:
        // Unknown future kind: fall back to the plain fill, which is only
        // correct for 64 bit elements.
        xs256simd_fill_bytes(Xs256_state_val(state_v), buffer, len);
    }
#endif // !XS256_BIG_ENDIAN
    return Val_unit;
}

CAMLprim value xs256simdintf_fill_32bit_float_ba(value state_v, value ba_v) {
    struct caml_ba_array* ba = Caml_ba_array_val(ba_v);
    assert((ba->flags & CAML_BA_KIND_MASK) == CAML_BA_FLOAT32);
    xoshiro256pp* state = Xs256_state_val(state_v);
    float* buffer = (float*)Caml_ba_data_val(ba_v);
    size_t length = caml_ba_byte_size(ba) / sizeof(float);
    state->xs256_fill_32bit_float_ba_fn(state, buffer, length);
    return Val_unit;
}

CAMLprim value xs256simdintf_fill_double_ba(value state_v, value ba_v) {
    struct caml_ba_array* ba = Caml_ba_array_val(ba_v);
    assert((ba->flags & CAML_BA_KIND_MASK) == CAML_BA_FLOAT64);
    xoshiro256pp* state = Xs256_state_val(state_v);
    double* buffer = (double*)Caml_ba_data_val(ba_v);
    size_t length = caml_ba_byte_size(ba) / sizeof(double);
    state->xs256_fill_double_ba_fn(state, buffer, length);
    return Val_unit;
}

CAMLprim value xs256simdintf_fill_32bit_float_range_ba(
    value state_v,
    value ba_v,
    value scale_v,
    value offset_v
) {
    struct caml_ba_array* ba = Caml_ba_array_val(ba_v);
    assert((ba->flags & CAML_BA_KIND_MASK) == CAML_BA_FLOAT32);
    xoshiro256pp* state = Xs256_state_val(state_v);
    float* buffer = (float*)Caml_ba_data_val(ba_v);
    size_t length = caml_ba_byte_size(ba) / sizeof(float);
    float scale = (float)Double_val(scale_v);
    float offset = (float)Double_val(offset_v);
    state->xs256_fill_32bit_float_range_ba_fn(
        state,
        buffer,
        scale,
        offset,
        length
    );
    return Val_unit;
}

CAMLprim value xs256simdintf_fill_double_range_ba(
    value state_v,
    value ba_v,
    value scale_v,
    value offset_v
) {
    struct caml_ba_array* ba = Caml_ba_array_val(ba_v);
    assert((ba->flags & CAML_BA_KIND_MASK) == CAML_BA_FLOAT64);
    xoshiro256pp* state = Xs256_state_val(state_v);
    double* buffer = (double*)Caml_ba_data_val(ba_v);
    size_t length = caml_ba_byte_size(ba) / sizeof(double);
    double scale = Double_val(scale_v);
    double offset = Double_val(offset_v);
    state->xs256_fill_double_range_ba_fn(state, buffer, scale, offset, length);
    return Val_unit;
}

// Internal testing only.

CAMLprim value xs256simdintf_test_get_state(
    value state_v,
    value state_s_v,
    value bitstream_v,
    value floatstream_v
) {
    CAMLparam4(state_v, state_s_v, bitstream_v, floatstream_v);
    CAMLlocal1(v);

    assert(caml_array_length(state_s_v) == 4 * XS256_LANES);
    size_t streamlen = QWORDS * 2;
    assert(caml_array_length(bitstream_v) == streamlen);
    assert(caml_array_length(floatstream_v) == FLOATWORDS);

    // The state here is on the C heap, so the GC can't move it below.
    xoshiro256pp* state = Xs256_state_val(state_v);

    for (size_t i = 0; i < 4 * XS256_LANES; i++) {
        v = caml_copy_int64(state->s[i]);
        Store_field(state_s_v, i, v);
    }

    for (size_t i = 0; i < streamlen; i++) {
        v = caml_copy_int32(state->bitstream[i]);
        Store_field(bitstream_v, i, v);
    }

    for (size_t i = 0; i < FLOATWORDS; i++) {
        Store_double_field(floatstream_v, i, state->doublestream[i]);
    }

    CAMLreturn(Val_unit);
}
