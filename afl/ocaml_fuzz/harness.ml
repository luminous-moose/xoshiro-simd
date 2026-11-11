(* AFL persistent-mode harness for xoshiro-simd's OCaml-level bound/range
   API (Xoshiro_simd.State.int, int_in_range, float_range, etc). This
   targets the documented Invalid_argument contracts and result-range
   invariants in lib/rand_impl.ml/lib/rand_core.ml, which the C-level
   differential harness in afl/harness.c never exercises (it bypasses OCaml
   entirely).

   Build/run: dune build --profile afl afl/ocaml_fuzz/harness.exe
              afl-fuzz -i afl/ocaml_fuzz/corpus -o afl/ocaml_fuzz/findings \
                -- ./_build/default/afl/ocaml_fuzz/harness.exe

   To see what a saved crash/queue file actually exercised, use ./decode.exe
   (see decode.ml) -- it shares this file's byte-layout parsing via
   fuzz_input.ml instead of re-deriving it. *)

module S = Xoshiro_simd.State

let bug fmt = Printf.ksprintf failwith fmt

(* The plain float/float_range functions promise the result stays within
   [min, max] (closed on both ends) for every input; they no longer promise
   the open interval -- either endpoint can leak through at extreme
   floating-point magnitudes, and that's their documented contract (see
   lib/xoshiro_simd.mli). The _strict siblings are what still promise the
   open interval, enforced by rejecting unsatisfiable inputs up front. *)

(* Rounds to the nearest float32, the way the float32 fills narrow their
   scale and offset. *)
let narrow_to_float32 x = Int32.float_of_bits (Int32.bits_of_float x)

(* The bulk range fills. Three checks:
   - the documented Invalid_argument contract (same conditions as float_range,
     plus the float32 narrowing checks in lib/rand_core.ml);
   - every element stays in the requested range;
   - every other available backend produces bit-identical output to Plain. This
     is the OCaml-level twin of afl/harness.c's differential check, but goes
     through the real min/max -> scale/offset derivation and the float32
     narrowing, where a tiny [max -. min] becomes a subnormal scale and picks
     the exact (non-prescaled) path in the backends.
   [kind] is the bigarray kind, [fill] the fill function under test, and
   [check_element] is the range invariant, which differs between the two
   widths (see the callers). *)
let check_bulk_range ~name ~invalid ~kind ~fill ~check_element ~n ~mn ~mx =
  let make backend =
    let state = S.init_deterministic ~backend 42L in
    let ba = Bigarray.Array1.create kind Bigarray.c_layout n in
    match fill state ba ~min:mn ~max:mx with
    | () ->
        if invalid then bug "%s: expected raise" name;
        Some ba
    | exception Invalid_argument _ ->
        if not invalid then bug "%s: unexpected raise" name;
        None
  in
  match make Xoshiro_simd.Plain with
  | None -> ()
  | Some plain ->
      for i = 0 to n - 1 do
        let v = Bigarray.Array1.get plain i in
        if not (check_element v) then
          bug "%s: element %d = %.17g out of range [%.17g,%.17g]" name i v mn mx
      done;
      List.iter
        (fun backend ->
          if not (Xoshiro_simd.backend_equal backend Xoshiro_simd.Plain) then
            match make backend with
            | None ->
                bug "%s: %s raised but Plain did not" name
                  (Xoshiro_simd.backend_to_string backend)
            | Some other ->
                for i = 0 to n - 1 do
                  (* Compare bits, not values: 0.0 vs -0.0 must differ, and
                     the cross-backend guarantee is bit-for-bit. A float32
                     element read back is an exactly-widened float. *)
                  if
                    Int64.bits_of_float (Bigarray.Array1.get plain i)
                    <> Int64.bits_of_float (Bigarray.Array1.get other i)
                  then
                    bug
                      "%s: backend %s differs from Plain at element %d \
                       (min=%.17g max=%.17g): %h vs %h"
                      name
                      (Xoshiro_simd.backend_to_string backend)
                      i mn mx
                      (Bigarray.Array1.get other i)
                      (Bigarray.Array1.get plain i)
                done)
        (Xoshiro_simd.available_backends ())

let run_one () =
  let data = Fuzz_input.read_all_bytes stdin in
  match Fuzz_input.parse data with
  | None -> ()
  | Some case -> (
      (* Fresh state per call, not shared across the persistent-mode batch:
         otherwise the same stdin bytes behave differently depending on how
         many prior iterations already advanced the bitstream, which makes
         results non-reproducible outside of afl-fuzz's exact replay order
         and tanks afl-fuzz's reported `stability` below 100%. *)
      let state = S.init_deterministic 42L in
      match case with
      | Fuzz_input.Int bound -> (
          let invalid = bound <= 0 || bound >= 1 lsl 30 in
          match S.int state bound with
          | v ->
              if invalid then bug "int: expected raise for bound=%d" bound;
              if v < 0 || v >= bound then bug "int: %d out of [0,%d)" v bound
          | exception Invalid_argument _ ->
              if not invalid then bug "int: unexpected raise for bound=%d" bound
          )
      | Fuzz_input.Int32 bound -> (
          let invalid = bound <= 0l in
          match S.int32 state bound with
          | v ->
              if invalid then bug "int32: expected raise";
              if v < 0l || v >= bound then bug "int32: out of range"
          | exception Invalid_argument _ ->
              if not invalid then bug "int32: unexpected raise")
      | Fuzz_input.Int64 bound -> (
          let invalid = bound <= 0L in
          match S.int64 state bound with
          | v ->
              if invalid then bug "int64: expected raise";
              if v < 0L || v >= bound then bug "int64: out of range"
          | exception Invalid_argument _ ->
              if not invalid then bug "int64: unexpected raise")
      | Fuzz_input.Full_int bound -> (
          let invalid = bound <= 0 in
          match S.full_int state bound with
          | v ->
              if invalid then bug "full_int: expected raise for bound=%d" bound;
              if v < 0 || v >= bound then
                bug "full_int: %d out of [0,%d)" v bound
          | exception Invalid_argument _ ->
              if not invalid then
                bug "full_int: unexpected raise for bound=%d" bound)
      | Fuzz_input.Int_in_range { min = mn; max = mx } -> (
          let invalid = mn > mx in
          match S.int_in_range state ~min:mn ~max:mx with
          | v ->
              if invalid then bug "int_in_range: expected raise";
              if v < mn || v > mx then bug "int_in_range: out of range"
          | exception Invalid_argument _ ->
              if not invalid then bug "int_in_range: unexpected raise")
      | Fuzz_input.Int32_in_range { min = mn; max = mx } -> (
          let invalid = mn > mx in
          match S.int32_in_range state ~min:mn ~max:mx with
          | v ->
              if invalid then bug "int32_in_range: expected raise";
              if v < mn || v > mx then bug "int32_in_range: out of range"
          | exception Invalid_argument _ ->
              if not invalid then bug "int32_in_range: unexpected raise")
      | Fuzz_input.Int64_in_range { min = mn; max = mx } -> (
          let invalid = mn > mx in
          match S.int64_in_range state ~min:mn ~max:mx with
          | v ->
              if invalid then bug "int64_in_range: expected raise";
              if v < mn || v > mx then bug "int64_in_range: out of range"
          | exception Invalid_argument _ ->
              if not invalid then bug "int64_in_range: unexpected raise")
      | Fuzz_input.Nativeint bound -> (
          let invalid = bound <= 0n in
          match S.nativeint state bound with
          | v ->
              if invalid then bug "nativeint: expected raise";
              if v < 0n || v >= bound then bug "nativeint: out of range"
          | exception Invalid_argument _ ->
              if not invalid then bug "nativeint: unexpected raise")
      | Fuzz_input.Nativeint_in_range { min = mn; max = mx } -> (
          let invalid = mn > mx in
          match S.nativeint_in_range state ~min:mn ~max:mx with
          | v ->
              if invalid then bug "nativeint_in_range: expected raise";
              if v < mn || v > mx then bug "nativeint_in_range: out of range"
          | exception Invalid_argument _ ->
              if not invalid then bug "nativeint_in_range: unexpected raise")
      | Fuzz_input.Get_bytes n -> (
          (* n is already clamped by Fuzz_input.parse so a bad allocation
             size can't turn this into a slow OOM rather than a fast,
             informative crash. *)
          let invalid = n < 0 in
          match S.get_insecure_bytes state n with
          | v ->
              if invalid then bug "get_bytes: expected raise for n=%d" n;
              if Bytes.length v <> n then
                bug "get_bytes: wrong length for n=%d" n
          | exception Invalid_argument _ ->
              if not invalid then bug "get_bytes: unexpected raise for n=%d" n)
      | Fuzz_input.Permutation n -> (
          let invalid = n < 0 in
          match S.permutation state n with
          | v ->
              if invalid then bug "permutation: expected raise for n=%d" n;
              if Array.length v <> n then
                bug "permutation: wrong length for n=%d" n;
              let seen = Array.make (max n 1) false in
              Array.iter
                (fun x ->
                  if x < 0 || x >= n || seen.(x) then
                    bug "permutation: not a valid permutation for n=%d" n;
                  seen.(x) <- true)
                v
          | exception Invalid_argument _ ->
              if not invalid then bug "permutation: unexpected raise for n=%d" n
          )
      | Fuzz_input.Float_range { min = mn; max = mx } -> (
          (* Relaxed contract: min/max may leak through exactly, so the only
             invariant that must always hold is the closed interval
             [mn, mx] -- not the open one. See float_range_strict below for
             the function that still promises the open interval. *)
          (* min = max = +/-infinity makes [mx -. mn] NaN, which is neither
             infinite nor an ordering violation, so it needs its own term:
             the library rejects it ("range is NaN"). *)
          let invalid =
            mn > mx || Float.is_nan mn || Float.is_nan mx
            || Float.is_infinite (mx -. mn)
            || Float.is_nan (mx -. mn)
          in
          match S.float_range state ~min:mn ~max:mx with
          | v ->
              if invalid then bug "float_range: expected raise";
              (* NaN compares false against everything, so the range test
                 below can't see it: check for it explicitly. *)
              if Float.is_nan v || v < mn || v > mx then
                bug "float_range: %.17g out of [%.17g,%.17g]" v mn mx
          | exception Invalid_argument _ ->
              if not invalid then bug "float_range: unexpected raise")
      | Fuzz_input.Float_range_strict { min = mn; max = mx } -> (
          let invalid =
            mn > mx || Float.is_nan mn || Float.is_nan mx
            || Float.is_infinite (mx -. mn)
            || mx = mn
            || mx = Float.succ mn
          in
          match S.float_range_strict state ~min:mn ~max:mx with
          | v ->
              if invalid then bug "float_range_strict: expected raise";
              if Float.is_nan v || v <= mn || v >= mx then
                bug
                  "float_range_strict: %.17g not strictly inside (%.17g,%.17g)"
                  v mn mx
          | exception Invalid_argument _ ->
              if not invalid then bug "float_range_strict: unexpected raise")
      | Fuzz_input.Float bound ->
          (* No precondition at all (pre-existing, out-of-scope behavior for
             NaN/infinite bounds), so only check the invariant when it's
             actually meaningful to do so. *)
          let v = S.float state bound in
          if Float.is_finite bound then
            let lo = Float.min 0. bound and hi = Float.max 0. bound in
            if Float.is_nan v || v < lo || v > hi then
              bug "float: %.17g out of [%.17g,%.17g] for bound=%.17g" v lo hi
                bound
      | Fuzz_input.Float_strict bound -> (
          let invalid =
            Float.is_nan bound || Float.is_infinite bound || bound = 0.
          in
          match S.float_strict state bound with
          | v ->
              if invalid then
                bug "float_strict: expected raise for bound=%.17g" bound;
              let lo = Float.min 0. bound and hi = Float.max 0. bound in
              let inside = v > lo && v < hi in
              let is_allowed_bound =
                Float.abs bound <= Float.min_float && v = bound
              in
              if Float.is_nan v || not (inside || is_allowed_bound) then
                bug "float_strict: %.17g not strictly inside (%.17g,%.17g)" v lo
                  hi
          | exception Invalid_argument _ ->
              if not invalid then
                bug "float_strict: unexpected raise for bound=%.17g" bound)
      | Fuzz_input.Fill_float64_range { min = mn; max = mx; n } ->
          (* As for float_range: min = max = +/-infinity gives a NaN span,
             which the library rejects but no comparison here would notice. *)
          let invalid =
            Float.is_nan mn || Float.is_nan mx
            || Float.abs (mx -. mn) > Float.max_float
            || Float.is_nan (mx -. mn)
            || mn > mx
          in
          check_bulk_range ~name:"fill_float64_range_bigarray" ~invalid
            ~kind:Bigarray.float64 ~fill:S.fill_float64_range_bigarray
            ~check_element:(fun v -> v >= mn && v <= mx)
            ~n ~mn ~mx
      | Fuzz_input.Fill_float32_range { min = mn; max = mx; n } ->
          let invalid =
            Float.is_nan mn || Float.is_nan mx
            || Float.abs (mx -. mn) > Float.max_float
            || mn > mx
            || Float.is_infinite (narrow_to_float32 (mx -. mn))
            || Float.is_infinite (narrow_to_float32 mn)
            || Float.is_infinite (narrow_to_float32 mx)
          in
          (* min and scale are each narrowed to float32 before use, so an
             element can leave [mn, mx] by the rounding of those and of the
             final result, a few float32 ulps of the larger magnitude; the
             absolute term covers subnormal results. This is a sanity bound
             (catches a wrong branch, a lost offset, NaN), not a tight one:
             the bit-exact check against Plain is the precise test. *)
          let tol =
            Float.ldexp (Float.abs mn +. Float.abs mx) (-20)
            +. Float.ldexp 1. (-140)
          in
          check_bulk_range ~name:"fill_float32_range_bigarray" ~invalid
            ~kind:Bigarray.float32 ~fill:S.fill_float32_range_bigarray
            ~check_element:(fun v -> v >= mn -. tol && v <= mx +. tol)
            ~n ~mn ~mx
      | Fuzz_input.Uniform_float ->
          let v = S.uniform_float state in
          if Float.is_nan v || v < 0. || v >= 1. then
            bug "uniform_float: %.17g not in [0,1)" v)

let () = AflPersistent.run run_one
