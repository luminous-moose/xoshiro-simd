// Bridges TestU01/PractRand (which pull "give me the next word now,
// whenever I want it") to the real public OCaml API (which can only be
// called from OCaml). The buffer is an OCaml Bigarray, passed once from
// OCaml at setup; its data pointer is stable for the Bigarray's lifetime
// (unlike a pointer taken from Bytes/String, which OCaml's moving minor
// GC can invalidate if it's held across a later allocating call --
// Bigarray data lives outside the OCaml heap and is never moved, so that
// hazard doesn't apply here), so caching it once here and reusing it
// across many refills is safe as long as the OCaml caller keeps the
// Bigarray value reachable for the whole run (guaranteed: it's a local in
// the same OCaml call that's still blocked inside the long-running
// external call into this bridge).
//
// When the current batch is exhausted, refill_and_reset calls back into
// OCaml (caml_callback) to run the real closure that refills this same
// buffer in place -- Xoshiro_simd.State.bits64/uniform_float
// looped, or fill_float64_bigarray/
// fill_float32_bigarray directly, depending on what the OCaml side
// registered. This C code never generates anything itself; it only
// consumes what OCaml already put in the buffer.
//
// Deliberately single-threaded: PractRand's MultithreadedTestManager is
// not used anywhere this header is included, since a second thread
// calling back into the single-threaded OCaml runtime isn't safe.
//
// Every *_source_init registers src->refill_closure as a GC root via
// caml_register_generational_global_root -- the caller MUST call the
// matching *_source_cleanup before the stub function that declared `src`
// returns. `src` is always a stack-local in that stub, so the registered
// root's address is only valid for that function's own stack frame;
// skipping cleanup leaves a dangling root the GC will eventually walk and
// crash on (Hd_val of garbage) during some later, unrelated major GC.
#ifndef XOSHIRO_OCAML_BRIDGE_H
#define XOSHIRO_OCAML_BRIDGE_H

#include <stddef.h>
#include <stdint.h>

#include <caml/callback.h>
#include <caml/memory.h>

#include "bit_reverse.h"
#include "float_bitbin.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*bitbin_fn_double)(const double*, size_t, uint64_t*, size_t*);
typedef void (*bitbin_fn_float)(const float*, size_t, uint64_t*, size_t*);

// Applied to each 64-bit word just before it's returned to the battery --
// xoshiro_identity64 (the default) or xoshiro_bitreverse64, see
// bit_reverse.h. Chosen once at source-init time, same shape as bitbin
// below. Only ocaml_int64_source uses this: its words are each one whole
// draw with nothing packed together, so reversing the word and
// reversing the draw's own bits are the same thing. double_source and
// float_source don't have this field -- see value_transform_fn_float
// below for why a float stream's reversal, where it exists at all, has
// to be shaped differently.
typedef uint64_t (*word_transform_fn)(uint64_t);

// int64 stream: no bit-binning needed, the buffer's elements are already
// the words the battery consumes directly.
typedef struct {
    int64_t* data;   // raw pointer into the OCaml Bigarray, cached once
    size_t capacity; // element count
    size_t idx;      // next unread element
    value refill_closure;
    word_transform_fn transform;
} ocaml_int64_source;

void ocaml_int64_source_init(
    ocaml_int64_source* src,
    value bigarray,
    value refill_closure,
    word_transform_fn transform
);
uint64_t ocaml_int64_source_next(ocaml_int64_source* src);
// Call once, after the last ocaml_int64_source_next, before the stub
// function that declared `src` returns -- see the GC-root note above.
void ocaml_int64_source_cleanup(ocaml_int64_source* src);

// Float streams (53-bit -> double buffer, 24-bit -> float buffer):
// the buffer holds `capacity` finished float/double values (however OCaml
// produced them -- scalar loop or a real bulk fill call); each refill's
// worth gets bit-binned into `packed` before being consumed word-by-word.
typedef struct {
    double* data;
    size_t capacity;
    value refill_closure;
    bitbin_fn_double bitbin;
    uint64_t* packed; // capacity words is always a safe upper bound
    size_t packed_count;
    size_t packed_idx;
} ocaml_double_source;

void ocaml_double_source_init(
    ocaml_double_source* src,
    value bigarray,
    value refill_closure,
    bitbin_fn_double bitbin
);
uint64_t ocaml_double_source_next(ocaml_double_source* src);
// Call once, after the last ocaml_double_source_next, before the stub
// function that declared `src` returns -- see the GC-root note above.
// Also frees `packed`.
void ocaml_double_source_cleanup(ocaml_double_source* src);

// Applied to the whole batch of finished floats in place, right before
// bitbin packs them -- xoshiro_identity_float24_values (the default) or
// xoshiro_reverse_float24_values, see float_bitbin.h. This has to run
// before bitbin, not on its packed output the way int64's
// word_transform_fn runs on its own output: bitbin splices several
// draws' bits together, so a transform applied after packing wouldn't
// be per-draw-bit any more.
typedef void (*value_transform_fn_float)(float*, size_t);

typedef struct {
    float* data;
    size_t capacity;
    value refill_closure;
    bitbin_fn_float bitbin;
    uint64_t* packed;
    size_t packed_count;
    size_t packed_idx;
    value_transform_fn_float value_transform;
} ocaml_float_source;

void ocaml_float_source_init(
    ocaml_float_source* src,
    value bigarray,
    value refill_closure,
    bitbin_fn_float bitbin,
    value_transform_fn_float value_transform
);
uint64_t ocaml_float_source_next(ocaml_float_source* src);
// Call once, after the last ocaml_float_source_next, before the stub
// function that declared `src` returns -- see the GC-root note above.
// Also frees `packed`.
void ocaml_float_source_cleanup(ocaml_float_source* src);

#ifdef __cplusplus
}
#endif

#endif // XOSHIRO_OCAML_BRIDGE_H
