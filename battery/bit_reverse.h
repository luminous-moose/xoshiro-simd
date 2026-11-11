// Word-level transforms applied to each 64-bit word just before it reaches
// TestU01/PractRand (see ocaml_bridge.h's word_transform_fn and the
// `transform` field on each source struct). xoshiro_identity64 is the
// default, no-op path; xoshiro_bitreverse64 supports feeding the battery
// the bit-reversed stream instead, as a second opinion alongside the
// forward one -- some tests are far more sensitive to defects in specific
// bit positions than others, so reading the same generator "backwards"
// surfaces defects a forward-only run could miss.
//
// Contract for xoshiro_bitreverse64: reverses all 64 bits of its input
// (bit 0 <-> bit 63, bit 1 <-> bit 62, ..., bit 31 <-> bit 32) and nothing
// else -- it is its own inverse, and must not special-case any bit width
// narrower than 64 (the float streams' packed words have no sub-64-bit
// structure visible at this layer; see float_bitbin.h).
#ifndef XOSHIRO_BATTERY_BIT_REVERSE_H
#define XOSHIRO_BATTERY_BIT_REVERSE_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

static inline uint64_t xoshiro_identity64(uint64_t w) { return w; }

uint64_t xoshiro_bitreverse64(uint64_t w);

#ifdef __cplusplus
}
#endif

#endif // XOSHIRO_BATTERY_BIT_REVERSE_H
