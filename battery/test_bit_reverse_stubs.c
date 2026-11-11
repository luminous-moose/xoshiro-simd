// OCaml binding for xoshiro_bitreverse64, used only by test_bit_reverse.ml.
#include <stdint.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>

#include "bit_reverse.h"

int64_t xoshiro_battery_test_bitreverse64(int64_t w) {
    return (int64_t)xoshiro_bitreverse64((uint64_t)w);
}

value xoshiro_battery_test_bitreverse64_byte(value w) {
    return caml_copy_int64(xoshiro_battery_test_bitreverse64(Int64_val(w)));
}
