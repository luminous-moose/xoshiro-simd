// OCaml-facing C stubs: bridges the real public Xoshiro_simd API (driven
// from OCaml, see ocaml_battery.ml) to TestU01 and PractRand. See
// ocaml_bridge.h for the buffering/refill-via-callback mechanism -- this
// exercises the real OCaml-visible API surface, which is a stronger
// guarantee of library quality than calling internal C entry points
// directly, at the cost of going through the OCaml runtime for every
// refill.
#include <cstdio>
#include <cstring>

extern "C" {
#include <bbattery.h>
#include <unif01.h>
}

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "bit_reverse.h"
#include "ocaml_bridge.h"
#include "ocaml_practrand_driver.h"

// --- TestU01 side ----------------------------------------------------------
// 32-bit-split GetBits approach (unif01_CreateExternGenBits, not the 64-bit
// `L` variant) -- deliberate, not a style preference. TestU01 1.2.3's
// unif01_CreateExternGenBitsL derives GetU01 (unif01.c's GBLong_U01) as
// externGenLong_Bits() / unif01_NORM32 -- the same 32-bit normalization
// constant the ordinary 32-bit path correctly uses, never updated for a
// genuine 64-bit unsigned long. Using the 64-bit `L` variant was tried
// first and produced catastrophic, instant SmallCrush failures (p-value
// ~ eps on the very first sub-tests) even though the raw generator output,
// checked independently, looked entirely normal. Splitting each 64-bit
// draw into two 32-bit halves (the HalfSplitter template below) and using
// the 32-bit CreateExternGenBits instead fixed it outright -- don't
// "simplify" this back to the 64-bit call later; it looks more natural but
// is silently broken in this TestU01 version.

namespace {

template <typename Source, uint64_t (*Next)(Source*)>
struct HalfSplitter {
    static Source* source;
    static uint32_t stored;
    static bool pending;

    static uint32_t next_bits() {
        if (pending) {
            pending = false;
            return stored;
        }
        uint64_t r = Next(source);
        stored = (uint32_t) (r >> 32);
        pending = true;
        return (uint32_t) r;
    }
};
template <typename Source, uint64_t (*Next)(Source*)>
Source* HalfSplitter<Source, Next>::source = nullptr;
template <typename Source, uint64_t (*Next)(Source*)>
uint32_t HalfSplitter<Source, Next>::stored = 0;
template <typename Source, uint64_t (*Next)(Source*)>
bool HalfSplitter<Source, Next>::pending = false;

using Int64Splitter = HalfSplitter<ocaml_int64_source, ocaml_int64_source_next>;
using DoubleSplitter = HalfSplitter<ocaml_double_source, ocaml_double_source_next>;
using FloatSplitter = HalfSplitter<ocaml_float_source, ocaml_float_source_next>;

void run_testu01_battery(unif01_Gen* gen, const char* battery) {
    if (strcmp(battery, "small") == 0) {
        bbattery_SmallCrush(gen);
    } else if (strcmp(battery, "crush") == 0) {
        bbattery_Crush(gen);
    } else if (strcmp(battery, "big") == 0) {
        bbattery_BigCrush(gen);
    } else {
        std::fprintf(stderr, "unknown battery \"%s\" (expected small|crush|big)\n", battery);
    }
    std::fflush(stdout);
}

} // namespace

extern "C" value xoshiro_ocaml_run_testu01_int64(
    value bigarray,
    value refill_closure,
    value battery,
    value name,
    value reverse_bits
) {
    CAMLparam5(bigarray, refill_closure, battery, name, reverse_bits);
    ocaml_int64_source src;
    word_transform_fn transform =
        Bool_val(reverse_bits) ? xoshiro_bitreverse64 : xoshiro_identity64;
    ocaml_int64_source_init(&src, bigarray, refill_closure, transform);
    Int64Splitter::source = &src;
    Int64Splitter::pending = false;

    unif01_Gen* gen = unif01_CreateExternGenBits(
        (char*) String_val(name),
        Int64Splitter::next_bits
    );
    run_testu01_battery(gen, String_val(battery));
    unif01_DeleteExternGenBits(gen);
    ocaml_int64_source_cleanup(&src);

    CAMLreturn(Val_unit);
}

extern "C" value xoshiro_ocaml_run_testu01_float64(
    value bigarray,
    value refill_closure,
    value battery,
    value name
) {
    CAMLparam4(bigarray, refill_closure, battery, name);
    ocaml_double_source src;
    ocaml_double_source_init(&src, bigarray, refill_closure, xoshiro_bitbin_53bit);
    DoubleSplitter::source = &src;
    DoubleSplitter::pending = false;

    unif01_Gen* gen = unif01_CreateExternGenBits(
        (char*) String_val(name),
        DoubleSplitter::next_bits
    );
    run_testu01_battery(gen, String_val(battery));
    unif01_DeleteExternGenBits(gen);
    ocaml_double_source_cleanup(&src);

    CAMLreturn(Val_unit);
}

extern "C" value xoshiro_ocaml_run_testu01_float32(
    value bigarray,
    value refill_closure,
    value battery,
    value name,
    value reverse_bits
) {
    CAMLparam5(bigarray, refill_closure, battery, name, reverse_bits);
    ocaml_float_source src;
    value_transform_fn_float value_transform = Bool_val(reverse_bits)
        ? xoshiro_reverse_float24_values
        : xoshiro_identity_float24_values;
    ocaml_float_source_init(
        &src, bigarray, refill_closure, xoshiro_bitbin_24bit, value_transform);
    FloatSplitter::source = &src;
    FloatSplitter::pending = false;

    unif01_Gen* gen = unif01_CreateExternGenBits(
        (char*) String_val(name),
        FloatSplitter::next_bits
    );
    run_testu01_battery(gen, String_val(battery));
    unif01_DeleteExternGenBits(gen);
    ocaml_float_source_cleanup(&src);

    CAMLreturn(Val_unit);
}

// --- PractRand side ----------------------------------------------------------

namespace {

class OcamlInt64Rng : public PractRand::RNGs::vRNG64 {
public:
    ocaml_int64_source* src;
    std::string name;
    OcamlInt64Rng(ocaml_int64_source* s, const char* n) : src(s), name(n) {}
    PractRand::Uint64 raw64() { return ocaml_int64_source_next(src); }
    void walk_state(PractRand::StateWalkingObject*) {}
    std::string get_name() const { return name; }
};

class OcamlDoubleRng : public PractRand::RNGs::vRNG64 {
public:
    ocaml_double_source* src;
    std::string name;
    OcamlDoubleRng(ocaml_double_source* s, const char* n) : src(s), name(n) {}
    PractRand::Uint64 raw64() { return ocaml_double_source_next(src); }
    void walk_state(PractRand::StateWalkingObject*) {}
    std::string get_name() const { return name; }
};

class OcamlFloatRng : public PractRand::RNGs::vRNG64 {
public:
    ocaml_float_source* src;
    std::string name;
    OcamlFloatRng(ocaml_float_source* s, const char* n) : src(s), name(n) {}
    PractRand::Uint64 raw64() { return ocaml_float_source_next(src); }
    void walk_state(PractRand::StateWalkingObject*) {}
    std::string get_name() const { return name; }
};

} // namespace

extern "C" value xoshiro_ocaml_run_practrand_int64(
    value bigarray,
    value refill_closure,
    value tlmax_bytes,
    value threshold,
    value name,
    value reverse_bits
) {
    CAMLparam5(bigarray, refill_closure, tlmax_bytes, threshold, name);
    CAMLxparam1(reverse_bits);
    ocaml_int64_source src;
    word_transform_fn transform =
        Bool_val(reverse_bits) ? xoshiro_bitreverse64 : xoshiro_identity64;
    ocaml_int64_source_init(&src, bigarray, refill_closure, transform);
    OcamlInt64Rng rng(&src, String_val(name));
    run_practrand_core_battery(
        &rng, (PractRand::Uint64) Long_val(tlmax_bytes), Double_val(threshold));
    ocaml_int64_source_cleanup(&src);
    CAMLreturn(Val_unit);
}

extern "C" value xoshiro_ocaml_run_practrand_float64(
    value bigarray,
    value refill_closure,
    value tlmax_bytes,
    value threshold,
    value name
) {
    CAMLparam5(bigarray, refill_closure, tlmax_bytes, threshold, name);
    ocaml_double_source src;
    ocaml_double_source_init(&src, bigarray, refill_closure, xoshiro_bitbin_53bit);
    OcamlDoubleRng rng(&src, String_val(name));
    run_practrand_core_battery(
        &rng, (PractRand::Uint64) Long_val(tlmax_bytes), Double_val(threshold));
    ocaml_double_source_cleanup(&src);
    CAMLreturn(Val_unit);
}

extern "C" value xoshiro_ocaml_run_practrand_float32(
    value bigarray,
    value refill_closure,
    value tlmax_bytes,
    value threshold,
    value name,
    value reverse_bits
) {
    CAMLparam5(bigarray, refill_closure, tlmax_bytes, threshold, name);
    CAMLxparam1(reverse_bits);
    ocaml_float_source src;
    value_transform_fn_float value_transform = Bool_val(reverse_bits)
        ? xoshiro_reverse_float24_values
        : xoshiro_identity_float24_values;
    ocaml_float_source_init(
        &src, bigarray, refill_closure, xoshiro_bitbin_24bit, value_transform);
    OcamlFloatRng rng(&src, String_val(name));
    run_practrand_core_battery(
        &rng, (PractRand::Uint64) Long_val(tlmax_bytes), Double_val(threshold));
    ocaml_float_source_cleanup(&src);
    CAMLreturn(Val_unit);
}

// The int64 and float32 practrand entry points above take 6 value
// arguments (bigarray, refill_closure, tlmax_bytes, threshold, name,
// reverse_bits), past the 5-argument limit OCaml's native calling
// convention passes directly -- beyond that, an external needs a
// bytecode-calling-convention stub (argv/argn) alongside the native one.
// float64 dropped back to 5 (no reverse_bits -- see validate_full in
// ocaml_battery.ml) and so no longer needs one.

extern "C" value xoshiro_ocaml_run_practrand_int64_bytecode(
    value* argv, int argn
) {
    (void) argn;
    return xoshiro_ocaml_run_practrand_int64(
        argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

extern "C" value xoshiro_ocaml_run_practrand_float32_bytecode(
    value* argv, int argn
) {
    (void) argn;
    return xoshiro_ocaml_run_practrand_float32(
        argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}
