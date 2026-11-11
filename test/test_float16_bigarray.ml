(* Bigarray.float16 only exists from OCaml 5.2 on (the type doesn't exist at
   all pre-5.2, so this file can't be written to run on every compiler the
   way test_domain.ml manages for Domain -- there's no library-side
   abstraction to hide behind here), hence a whole separate file gated in
   test/dune rather than an inline runtime check.

   This deliberately only smoke-tests that xs256simdintf_get_bytes_ba
   (xs256_stubs.c) routes CAML_BA_FLOAT16 into the int16/uint16 case instead
   of falling through to caml_invalid_argument -- it does not check
   byte-for-byte exactness against get_insecure_bytes the way the
   char/float32/float64/complex32/complex64 tests in test_primitives.ml do,
   since that would need a hand-written IEEE 754 half-precision decoder (no
   public stdlib function exposes one) purely for this one low-priority
   kind. *)

let () =
  let state = Xoshiro_simd.State.init_deterministic 42L in
  let ba = Bigarray.Array1.create Bigarray.float16 Bigarray.c_layout 16 in
  Xoshiro_simd.State.fill_insecure_bigarray state ba
