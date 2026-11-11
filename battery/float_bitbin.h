// Bit-binning for the float streams: turns a buffer of *finished* floats
// (the real public output, produced via the OCaml-callback bridge in
// ocaml_bridge.h/.c) into densely packed uint64_t words suitable for
// feeding to TestU01/PractRand.
//
// Exact inversion each of these needs (both are provably exact, no
// rounding): multiply the finished value back by the matching power of two
// (2^53 / 2^24), round to nearest, convert to integer.
//
// Each function packs `count` input values into 64-bit words with no
// padding between them (bits from consecutive input values span word
// boundaries as needed) and returns the number of whole 64-bit words
// written to `out` (via *out_words). `out` must have room for
// ceil(count * bits_per_value / 64) words.
#ifndef XOSHIRO_BATTERY_FLOAT_BITBIN_H
#define XOSHIRO_BATTERY_FLOAT_BITBIN_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// 2^24: the float32 bulk-fill mantissa scale. Shared so float_bitbin.c's
// own inv_f32 and xoshiro_reverse_float24_values's exact round-trip below
// can't silently drift apart into two different constants.
#define XOSHIRO_FLOAT24_SCALE 0x1.p24F

// Packs 53-bit-per-value content (the float64 scalar draw and bulk fill) into
// dense 64-bit words.
void xoshiro_bitbin_53bit(
    const double* values,
    size_t count,
    uint64_t* out,
    size_t* out_words
);

// Packs 24-bit-per-value content (the float32 bulk fill) into dense 64-bit
// words.
void xoshiro_bitbin_24bit(
    const float* values,
    size_t count,
    uint64_t* out,
    size_t* out_words
);

// Reverses the bits of each value's underlying 24-bit mantissa integer (the
// same integer xoshiro_bitbin_24bit's exact-inversion recovers) in place.
// Used by --full for the float32 stream instead of reversing bits of the
// post-bitbin packed words: xoshiro_bitbin_24bit packs several draws'
// worth of bits together, so a word-level reversal there tests something
// different from (and weaker than) reversing each draw's own mantissa
// bits. Chosen the same way xoshiro_identity64/xoshiro_bitreverse64 are
// (see bit_reverse.h): xoshiro_identity_float24_values is the no-op default.
void xoshiro_reverse_float24_values(float* values, size_t count);
void xoshiro_identity_float24_values(float* values, size_t count);

#ifdef __cplusplus
}
#endif

#endif // XOSHIRO_BATTERY_FLOAT_BITBIN_H
