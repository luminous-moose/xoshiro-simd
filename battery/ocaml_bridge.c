#include <caml/bigarray.h>
#include <caml/memory.h>

#include <stdlib.h>

#include "ocaml_bridge.h"

// CAMLparam in the calling stub only registers the *parameter's own*
// stack slot as a GC root -- copying that value into a struct field (as
// each *_source_init below does) creates a second, unregistered copy the
// GC doesn't know to update if it moves the closure during a later
// caml_callback. caml_register_generational_global_root on the field's
// address is what makes that copy itself a real, tracked root; without
// it, a closure relocated by GC mid-run leaves src->refill_closure
// dangling -- reproduced as a real segfault (SIGSEGV inside
// caml_callback_exn, corrupted closure) during harness bring-up, not a
// hypothetical.

// --- int64 ---------------------------------------------------------------

void ocaml_int64_source_init(
    ocaml_int64_source* src,
    value bigarray,
    value refill_closure,
    word_transform_fn transform
) {
    src->data = (int64_t*)Caml_ba_data_val(bigarray);
    src->capacity = Caml_ba_array_val(bigarray)->dim[0];
    // The OCaml caller has already run refill_closure once, so the buffer
    // is live starting at element 0 -- no forced refill needed here.
    src->idx = 0;
    src->refill_closure = refill_closure;
    caml_register_generational_global_root(&src->refill_closure);
    src->transform = transform;
}

uint64_t ocaml_int64_source_next(ocaml_int64_source* src) {
    if (src->idx >= src->capacity) {
        caml_callback(src->refill_closure, Val_unit);
        src->idx = 0;
    }
    return src->transform((uint64_t)src->data[src->idx++]);
}

void ocaml_int64_source_cleanup(ocaml_int64_source* src) {
    caml_remove_generational_global_root(&src->refill_closure);
}

// --- double (53-bit) -------------------------------------------------------

void ocaml_double_source_init(
    ocaml_double_source* src,
    value bigarray,
    value refill_closure,
    bitbin_fn_double bitbin
) {
    src->data = (double*)Caml_ba_data_val(bigarray);
    src->capacity = Caml_ba_array_val(bigarray)->dim[0];
    src->refill_closure = refill_closure;
    caml_register_generational_global_root(&src->refill_closure);
    src->bitbin = bitbin;
    src->packed = malloc(src->capacity * sizeof(uint64_t));
    // The OCaml caller has already run refill_closure once, so src->data
    // already holds a live batch -- bit-bin it now.
    src->bitbin(src->data, src->capacity, src->packed, &src->packed_count);
    src->packed_idx = 0;
}

uint64_t ocaml_double_source_next(ocaml_double_source* src) {
    if (src->packed_idx >= src->packed_count) {
        caml_callback(src->refill_closure, Val_unit);
        src->bitbin(src->data, src->capacity, src->packed, &src->packed_count);
        src->packed_idx = 0;
    }
    return src->packed[src->packed_idx++];
}

void ocaml_double_source_cleanup(ocaml_double_source* src) {
    caml_remove_generational_global_root(&src->refill_closure);
    free(src->packed);
}

// --- float (24-bit) --------------------------------------------------------

void ocaml_float_source_init(
    ocaml_float_source* src,
    value bigarray,
    value refill_closure,
    bitbin_fn_float bitbin,
    value_transform_fn_float value_transform
) {
    src->data = (float*)Caml_ba_data_val(bigarray);
    src->capacity = Caml_ba_array_val(bigarray)->dim[0];
    src->refill_closure = refill_closure;
    caml_register_generational_global_root(&src->refill_closure);
    src->bitbin = bitbin;
    src->packed = malloc(src->capacity * sizeof(uint64_t));
    src->value_transform = value_transform;
    // The OCaml caller has already run refill_closure once, so src->data
    // already holds a live batch -- apply the per-value transform (before
    // bitbin splices draws' bits together) and bit-bin it now.
    src->value_transform(src->data, src->capacity);
    src->bitbin(src->data, src->capacity, src->packed, &src->packed_count);
    src->packed_idx = 0;
}

uint64_t ocaml_float_source_next(ocaml_float_source* src) {
    if (src->packed_idx >= src->packed_count) {
        caml_callback(src->refill_closure, Val_unit);
        src->value_transform(src->data, src->capacity);
        src->bitbin(src->data, src->capacity, src->packed, &src->packed_count);
        src->packed_idx = 0;
    }
    return src->packed[src->packed_idx++];
}

void ocaml_float_source_cleanup(ocaml_float_source* src) {
    caml_remove_generational_global_root(&src->refill_closure);
    free(src->packed);
}
