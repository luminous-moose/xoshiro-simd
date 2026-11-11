#!/usr/bin/env python3
"""Writes the range_* boundary inputs of afl/corpus/ (harness.c's 33-byte
form: 8-byte seed, length byte, float32 scale, float32 offset, float64 scale,
float64 offset, little-endian).

Why these exist: the scale magnitude selects the code path in the range fills
(XS256_FLOAT_SCALE_THRESHOLD = 2^-102 for float32 SSE2/AVX2/AVX512FVL/NEON,
XS256_DOUBLE_SCALE_THRESHOLD = 2^-969 for AVX512FVL float64, and on 32-bit ARM
the Plain fallback that also looks at offset), and afl-fuzz does not decompose
a floating-point compare against those constants by itself. These are the same
values test/test_backend.ml uses for its tiny-range tests: just below, at, and
above each threshold, subnormal scales, and the cancellation pair
scale = 1.5 * 2^-102, offset = -2^-125.

Rerun after changing the value lists (the files are deterministic):
    python3 afl/gen_corpus.py
"""
import os
import struct

out = os.path.join(os.path.dirname(os.path.abspath(__file__)), "corpus")


def f32_bits(bits):
    return struct.pack("<I", bits)


def f32(x):
    return struct.pack("<f", x)


def f64(x):
    return struct.pack("<d", x)


def f64_bits(bits):
    return struct.pack("<Q", bits)


def bits32(x):
    return struct.unpack("<I", struct.pack("<f", x))[0]


def bits64(x):
    return struct.unpack("<Q", struct.pack("<d", x))[0]


T32 = 2.0**-102
T64 = 2.0**-969

# (name, scale_f, offset_f); the float32 side.
f32_cases = [
    ("f32_below", f32_bits(bits32(T32) - 1), f32(0.0)),
    ("f32_at", f32(T32), f32(0.0)),
    ("f32_above", f32_bits(bits32(T32) + 1), f32(0.0)),
    ("f32_1p5", f32(1.5 * T32), f32(0.0)),
    ("f32_neg_below", f32_bits(0x80000000 | (bits32(T32) - 1)), f32(0.0)),
    ("f32_neg_at", f32(-T32), f32(-1.0)),
    ("f32_half", f32(0.5 * T32), f32(1.0)),
    ("f32_1e-35", f32(2.0**-116), f32(0.0)),
    ("f32_subnormal", f32_bits(0x00000400), f32(0.0)),
    ("f32_min_subnormal", f32_bits(1), f32(0.0)),
    ("f32_zero", f32(0.0), f32(0.0)),
    ("f32_cancel", f32(1.5 * T32), f32(-(2.0**-125))),
    ("f32_cancel_off_small", f32(1e30), f32(-0.1 * T32)),
    ("f32_off_at", f32(2.0**-100), f32(T32)),
    ("f32_off_below", f32(2.0**-100), f32_bits(bits32(T32) - 1)),
    ("f32_big", f32(3.0e38), f32(-1.0e38)),
]

f64_cases = [
    ("f64_below", f64_bits(bits64(T64) - 1), f64(0.0)),
    ("f64_at", f64(T64), f64(0.0)),
    ("f64_above", f64_bits(bits64(T64) + 1), f64(0.0)),
    ("f64_1p5", f64(1.5 * T64), f64(0.0)),
    ("f64_neg_below", f64_bits((1 << 63) | (bits64(T64) - 1)), f64(0.0)),
    ("f64_neg_at", f64(-T64), f64(-1.0)),
    ("f64_half", f64(0.5 * T64), f64(1.0)),
    ("f64_2p-1000", f64(2.0**-1000), f64(0.0)),
    ("f64_subnormal", f64_bits(0x0000000000100000), f64(0.0)),
    ("f64_min_subnormal", f64_bits(1), f64(0.0)),
    ("f64_zero", f64(0.0), f64(0.0)),
    ("f64_cancel", f64(1.5 * T64), f64(-(2.0**-1022))),
    ("f64_off_small", f64(1e300), f64(-0.1 * T64)),
    ("f64_off_at", f64(2.0**-960), f64(T64)),
    ("f64_off_below", f64(2.0**-960), f64_bits(bits64(T64) - 1)),
    ("f64_big", f64(1.7e308), f64(-8.0e307)),
]

seeds = [
    bytes([1, 2, 3, 4, 5, 6, 7, 8]),
    bytes(8),
    bytes([0xFF] * 8),
    bytes([0xA5] * 8),
]
# Lengths: several blocks with a tail for both the 8-element float32 block and
# the 4-element double block; one that is a whole number of blocks for both; tiny; the max.
lengths = [100, 64, 5, 1, 255]

for f in os.listdir(out):
    if f.startswith("range_"):
        os.remove(os.path.join(out, f))

n = 0
for i, (fname, sf, of) in enumerate(f32_cases):
    for j, (dname, sd, od) in enumerate(f64_cases):
        # The full cross product is 256 files; a diagonal plus a second
        # pairing keeps the corpus small while every value appears twice.
        if j != i and j != (i + 5) % len(f64_cases):
            continue
        seed = seeds[n % len(seeds)]
        length = lengths[n % len(lengths)]
        data = seed + bytes([length]) + sf + of + sd + od
        assert len(data) == 33
        name = f"range_{n:02d}_{fname}__{dname}"
        with open(os.path.join(out, name), "wb") as fh:
            fh.write(data)
        n += 1
print(f"wrote {n} inputs to {out}")

# ---------------------------------------------------------------------------
# ocaml_fuzz/corpus: the bulk range fills (selectors 16 = float32, 17 =
# float64 in afl/ocaml_fuzz/fuzz_input.ml's parse; keep F32/F64 in step with
# it, the numbers shift whenever a case is added before them). Layout:
# selector byte, min as float64 bits (8), max as float64 bits (8), element
# count byte.
# ---------------------------------------------------------------------------
F32, F64 = 16, 17
ocaml_out = os.path.join(os.path.dirname(out), "ocaml_fuzz", "corpus")
for f in os.listdir(ocaml_out):
    if f.startswith("fill_float"):
        os.remove(os.path.join(ocaml_out, f))


def ocaml_input(selector, mn, mx, n):
    return bytes([selector]) + f64(mn) + f64(mx) + bytes([n])


def nextafter64(x):
    return struct.unpack("<d", struct.pack("<Q", bits64(x) + 1))[0]


ocaml_cases = [
    # name, selector, min, max, n
    ("f32_plain", F32, -9000.0, 9000.0, 100),
    ("f64_plain", F64, -9000.0, 9000.0, 100),
    ("f32_unit", F32, 0.0, 1.0, 37),
    ("f64_unit", F64, 0.0, 1.0, 37),
    # Scale (max - min) on both sides of each threshold.
    ("f32_scale_below", F32, 0.0, 0.75 * T32, 100),
    ("f32_scale_above", F32, 0.0, 1.5 * T32, 100),
    ("f32_scale_subnormal", F32, 0.0, 1.2345 * 2.0**-130, 100),
    ("f64_scale_below", F64, 0.0, 0.75 * T64, 100),
    ("f64_scale_above", F64, 0.0, 1.5 * T64, 100),
    ("f64_scale_subnormal", F64, 0.0, 1.2345 * 2.0**-1050, 100),
    # Tiny offset against a tiny scale, and the ARM32 cancellation pair:
    # scale = 1.5 * 2^-102, min = -2^-125.
    ("f32_cancel", F32, -(2.0**-125), -(2.0**-125) + 1.5 * T32, 100),
    ("f64_cancel", F64, -(2.0**-1022), -(2.0**-1022) + 1.5 * T64, 100),
    # min == max (every element is min), neighbouring floats, huge ranges.
    ("f32_degenerate", F32, 5.0, 5.0, 20),
    ("f64_degenerate", F64, 5.0, 5.0, 20),
    ("f64_adjacent", F64, 1.0, nextafter64(1.0), 20),
    ("f64_huge", F64, -1.7e308, 1.7e308 * 0.5, 20),
    # Invalid: reversed, NaN, infinite range, float32-narrowing overflow.
    ("f32_reversed", F32, 2.0, 1.0, 10),
    ("f64_reversed", F64, 2.0, 1.0, 10),
    ("f32_nan", F32, float("nan"), 1.0, 10),
    ("f64_inf_range", F64, -1.7e308, 1.7e308, 10),
    ("f32_narrow_overflow", F32, -3.0e38, 3.0e38, 10),
    # Finite float32 bounds but max beyond float32's range.
    ("f32_max_beyond", F32, 3.0e38, 3.5e38, 100),
    # min = max = +/-infinity: max - min is NaN, which slips past a range check
    # written as an ordering comparison. The library rejects it ("range is
    # NaN"); the float32 fill is also rejected, by its narrowing checks.
    ("f64_inf_equal", F64, float("inf"), float("inf"), 10),
    ("f64_neg_inf_equal", F64, float("-inf"), float("-inf"), 10),
    ("f32_inf_equal", F32, float("inf"), float("inf"), 10),
    ("f32_neg_inf_equal", F32, float("-inf"), float("-inf"), 10),
]
for name, sel, mn, mx, n in ocaml_cases:
    with open(os.path.join(ocaml_out, f"fill_float_range_{name}"), "wb") as fh:
        fh.write(ocaml_input(sel, mn, mx, n))
print(f"wrote {len(ocaml_cases)} inputs to {ocaml_out}")

# The same NaN-span inputs for the scalar range functions (selectors 11 =
# float_range, 12 = float_range_strict). Not wiped above: these names don't
# start with "fill_float", so the loop that removes generated fill seeds leaves
# them alone and they are simply rewritten here.
scalar_cases = [
    ("float_range_inf_equal", 11, float("inf"), float("inf")),
    ("float_range_neg_inf_equal", 11, float("-inf"), float("-inf")),
    ("float_range_strict_inf_equal", 12, float("inf"), float("inf")),
]
for name, sel, mn, mx in scalar_cases:
    with open(os.path.join(ocaml_out, name), "wb") as fh:
        fh.write(bytes([sel]) + f64(mn) + f64(mx))
print(f"wrote {len(scalar_cases)} scalar inputs to {ocaml_out}")

# float_range (selector 11) and float_range_strict (selector 12) each check
# their arguments with one combined condition (min <= max && max - min <=
# max_float for float_range; succ min < max && max - min <= max_float for the
# strict one), so these sit on both sides of every part of them. The same
# pairs go to both functions: what is valid for one is not always valid for
# the other (min = max, for example), and the harness's own predicate decides.
# The "gap" pairs leave the strict function exactly one representable value
# to return (or none), the most likely place for it to loop forever if a check
# were lost, which afl reports as a hang.
MAX_FLOAT = 1.7976931348623157e308
TINY = struct.unpack("<d", f64_bits(1))[0]
range_pairs = [
    # name, min, max
    ("nan_min", float("nan"), 1.0),
    ("nan_max", 1.0, float("nan")),
    ("neg_inf_zero", float("-inf"), 0.0),
    ("zero_inf", 0.0, float("inf")),
    ("full_inf", float("-inf"), float("inf")),
    ("reversed_inf", float("inf"), float("-inf")),
    ("overflow_span", -MAX_FLOAT, MAX_FLOAT),
    ("overflow_span_half", -MAX_FLOAT, 0.5 * MAX_FLOAT),
    ("widest_pos", 0.0, MAX_FLOAT),
    ("widest_neg", -MAX_FLOAT, 0.0),
    ("equal", 5.0, 5.0),
    ("zero_negzero", 0.0, -0.0),
    ("negzero_zero", -0.0, 0.0),
    ("reversed", 2.0, 1.0),
    ("reversed_adjacent", nextafter64(1.0), 1.0),
    ("adjacent", 1.0, nextafter64(1.0)),
    ("one_gap", 1.0, nextafter64(nextafter64(1.0))),
    ("tiny_gap", -0.0, 2 * TINY),
    ("around_zero", -TINY, TINY),
]
for name, mn, mx in range_pairs:
    for sel, prefix in ((11, "float_range"), (12, "float_range_strict")):
        with open(os.path.join(ocaml_out, f"{prefix}_{name}"), "wb") as fh:
            fh.write(bytes([sel]) + f64(mn) + f64(mx))
print(f"wrote {2 * len(range_pairs)} float_range inputs to {ocaml_out}")

# float_strict (selector 14) takes one float. Its bound check is a single
# condition, abs bound > 0 && abs bound <= max_float, so these sit on each side
# of both ends of it, plus NaN (both comparisons false) and the infinities.
# With an infinite or zero bound the draw loop never ends if the check is lost,
# so these seeds make that show up at once as a hang. A bound at or below the
# smallest normal double (bit pattern 0x0010000000000000, 2^-1022) is accepted
# and may be returned itself: the product of the largest draw and such a bound
# rounds back to the bound, which is why the harness only demands a strictly
# interior result above 2^-1022, and why the seeds around that value matter
# (largest subnormal, 2^-1022, the float above it). The smallest subnormal
# (bit pattern 1) already has its own seed, float_strict_subnormal. Names are
# float_strict_*, so the wipe above leaves the two older hand-made seeds alone
# and these are rewritten each run.
SIGN = 1 << 63
MIN_NORMAL_BITS = 0x0010000000000000
float_strict_cases = [
    # name, the bound as 8 little-endian bytes
    ("float_strict_nan", f64(float("nan"))),
    ("float_strict_inf", f64(float("inf"))),
    ("float_strict_neg_inf", f64(float("-inf"))),
    ("float_strict_zero", f64(0.0)),
    ("float_strict_neg_zero", f64(-0.0)),
    ("float_strict_neg_subnormal", f64_bits(SIGN | 1)),
    ("float_strict_largest_subnormal", f64_bits(MIN_NORMAL_BITS - 1)),
    ("float_strict_neg_largest_subnormal", f64_bits(SIGN | (MIN_NORMAL_BITS - 1))),
    ("float_strict_min_normal", f64_bits(MIN_NORMAL_BITS)),
    ("float_strict_neg_min_normal", f64_bits(SIGN | MIN_NORMAL_BITS)),
    ("float_strict_above_min_normal", f64_bits(MIN_NORMAL_BITS + 1)),
    ("float_strict_neg_above_min_normal", f64_bits(SIGN | (MIN_NORMAL_BITS + 1))),
    ("float_strict_max_float", f64(MAX_FLOAT)),
    ("float_strict_neg_max_float", f64(-MAX_FLOAT)),
    ("float_strict_neg_valid", f64(-10.0)),
    ("float_strict_succ_one", f64(nextafter64(1.0))),
]
for name, bound in float_strict_cases:
    with open(os.path.join(ocaml_out, name), "wb") as fh:
        fh.write(bytes([14]) + bound + f64(0.0))
print(f"wrote {len(float_strict_cases)} float_strict inputs to {ocaml_out}")
