#define CAML_NAME_SPACE
#include <caml/mlvalues.h>

#include <stdint.h>

// Reference implementation of Lemire's method at 32-bit width -- a
// cross-check for int_lemire_native (lib/rand_core.ml) and
// xs256simdintf_lemire_32bits (lib/xs256_stubs.c), exercised only from
// test/test_lemire_reference.ml.
//
// This is the published algorithm transcribed as closely as possible, so a
// reader can check it against the source line by line rather than having to
// re-derive it: "Lemire's Method with Debiased Integer Multiplication
// (optimized)" from Melissa O'Neill, "Efficiently Generating a Number in a
// Range" (pcg-random.org, 2018), which is Lemire's nearly divisionless method
// plus two subtractions that compute the threshold without a division when
// range is large. It is valid for any 32-bit range. The library only allows
// bounds below 2^30, where those two subtractions can never finish the job
// (2^32 >= 4 * range), so the library leaves them out; with them still here,
// this is the general algorithm the library's version is a special case of.
//
// The only change from the published code is that it makes a single attempt:
// where the original draws a fresh x and loops, this returns -1 ("reject --
// the caller should draw a fresh x and call again"); otherwise it returns the
// accepted result in [0, range).
//
// x: raw 32-bit draw, as an OCaml int32. bound: exclusive bound, must satisfy
// 0 < bound < 2^30 -- unchecked here, same convention as the production
// version (the caller validates).
CAMLprim value
xoshiro_simd_test_lemire_reference_attempt(value bound_v, value x_v) {
    uint32_t range = (uint32_t)Long_val(bound_v);
    uint32_t x = (uint32_t)Int32_val(x_v);

    uint64_t m = (uint64_t)x * (uint64_t)range;
    uint32_t l = (uint32_t)m;
    if (l < range) {
        uint32_t t = -range;
        if (t >= range) {
            t -= range;
            if (t >= range) {
                t %= range;
            }
        }
        if (l < t) {
            return Val_long(-1);
        }
    }
    return Val_long((intnat)(m >> 32));
}
