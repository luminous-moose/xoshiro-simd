#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "bit_reverse.h"
#include "float_bitbin.h"

const static double inv_double = 0x1.p53;
const static float inv_f32 = XOSHIRO_FLOAT24_SCALE;

// These two functions pack bits into the output arrays.

static inline void split_bits_64(
    const uint64_t shifted,
    const size_t total_bits,
    const size_t gap,
    uint64_t* out
) {
    size_t out_index = total_bits / 64;
    size_t offset = total_bits % 64;
    if (offset <= gap) {
        size_t left_shift = gap - offset;
        out[out_index] |= shifted << left_shift;
    } else {
        size_t right_shift = offset - gap;
        out[out_index] |= shifted >> right_shift;
        size_t left_shift = 64 - right_shift;
        out[out_index + 1] |= shifted << left_shift;
    }
}

static inline void split_bits_32(
    const uint32_t shifted,
    const size_t total_bits,
    const size_t gap,
    uint32_t* out
) {
    size_t out_index = total_bits / 32;
    size_t offset = total_bits % 32;
    if (offset <= gap) {
        size_t left_shift = gap - offset;
        out[out_index] |= shifted << left_shift;
    } else {
        size_t right_shift = offset - gap;
        out[out_index] |= shifted >> right_shift;
        size_t left_shift = 32 - right_shift;
        out[out_index + 1] |= shifted << left_shift;
    }
}

// These functions recover the original bits for 53 bit and 24 bit mantissa
// floats, and bin all of the bits together in an integer number of
// uint64_ts to output to the tests.

void xoshiro_bitbin_53bit(
    const double* values,
    size_t count,
    uint64_t* out,
    size_t* out_words
) {
    size_t total_bits = 0;
    assert(count * 53 % 64 == 0);
    const size_t out_count = count * 53 / 64;
    memset(out, 0, out_count * sizeof(uint64_t));
    for (size_t i = 0; i < count; i++) {
        double d = values[i];
        double full_d = d * inv_double;
        uint64_t shifted = (uint64_t)full_d;
        split_bits_64(shifted, total_bits, 11, out);
        total_bits += 53;
    }
    assert(total_bits % 64 == 0);
    *out_words = total_bits / 64;
}

void xoshiro_bitbin_24bit(
    const float* values,
    size_t count,
    uint64_t* out,
    size_t* out_words
) {
    size_t total_bits = 0;
    assert(count * 24 % 64 == 0);
    const size_t out_count = count * 24 / 64;
    memset(out, 0, out_count * sizeof(uint64_t));
    uint32_t* outkast = (uint32_t*)out;
    for (size_t i = 0; i < count; i++) {
        float f = values[i];
        float full_f = f * inv_f32;
        uint32_t shifted = (uint32_t)full_f;
        split_bits_32(shifted, total_bits, 8, outkast);
        total_bits += 24;
    }
    assert(total_bits % 64 == 0);
    *out_words = total_bits / 64;
}

// u24 occupies the low 24 bits of a 64-bit word with the top 40 bits
// zero; reversing all 64 bits moves those 24 bits of real content to the
// top of the word (positions 40-63) with zeros below, so >> 40 recovers
// exactly the 24-bit reversal -- no separate table or primitive needed,
// just xoshiro_bitreverse64 itself.
void xoshiro_reverse_float24_values(float* values, size_t count) {
    for (size_t i = 0; i < count; i++) {
        uint32_t u24 = (uint32_t)(values[i] * inv_f32);
        uint32_t reversed = (uint32_t)(xoshiro_bitreverse64((uint64_t)u24) >> 40);
        values[i] = (float)reversed * (1.0F / inv_f32);
    }
}

void xoshiro_identity_float24_values(float* values, size_t count) {
    (void)values;
    (void)count;
}
