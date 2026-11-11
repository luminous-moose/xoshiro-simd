open Base
module Xs_S = Xoshiro_simd.State

(* Correctness story for Xoshiro_simd.State.int_lemire_native
   (lib/rand_core.ml) -- entirely from outside the library, since we must
   be able to test specific draws.

   Two local, test-only re-implementations:
   - [original_algorithm]: a verbatim copy of int_lemire_native's
     body -- the actual shipped algorithm, which computes the rejection
     threshold [t] once and caches it across retries -- parameterized over
     a [next_x] draw function instead of a live generator state, so it can
     be driven by controlled test input.
   - [attempt_step]/[attempt_algorithm]: an alternative "pure single
     attempt" shape (recomputes [t] on every retry instead of caching it),
     also driven by [next_x]. A pure [bound -> x -> int option] function is
     much easier to cross-check against an independent implementation than
     a retry loop tied to live generator state, which is why the actual
     reference comparison below (against lemire_ref_stubs.c, an
     independently-written C implementation) targets [attempt_step], not
     [original_algorithm] directly.

   The chain of evidence: [attempt_step] agrees with the independent C
   reference (below) => [attempt_step]-driven [attempt_algorithm] agrees
   with [original_algorithm] (further below) => [original_algorithm] is a
   verbatim transcription of the real, shipped
   int_lemire_native.

   Each draw is one raw 32-bit word (Lemire at L = 32): an [int32] here, as
   it is in the library's word buffer, so the whole 32-bit range can be
   written down on 32-bit hosts too. *)

(* Base hides this; attempt_step below needs it. *)
let ( mod ) = Stdlib.( mod )

module I32 = Stdlib.Int32

(* Copies of rand_core.ml's I64 and Int modules, so the verbatim copy below
   reads exactly like the shipped code. I64 is built on Stdlib's Int64 (Base's
   Int64.to_int returns an option); Int on Base's, which the rest of this file
   uses. *)
module I64 = struct
  include Stdlib.Int64

  external ( + ) : int64 -> int64 -> int64 = "%int64_add"
  external ( - ) : int64 -> int64 -> int64 = "%int64_sub"
  external ( * ) : int64 -> int64 -> int64 = "%int64_mul"
  external ( mod ) : int64 -> int64 -> int64 = "%int64_mod"
  external ( > ) : int64 -> int64 -> bool = "%greaterthan"
  external ( land ) : int64 -> int64 -> int64 = "%int64_and"
  external ( lsr ) : int64 -> int -> int64 = "%int64_lsr"
end

module Int = struct
  include Int

  external ( >= ) : int -> int -> bool = "%greaterequal"
  external ( land ) : int -> int -> int = "%andint"
  external ( mod ) : int -> int -> int = "%modint"
end

(* 2^30 - 1: the largest valid bound (exclusive upper limit is 2^30). *)
let max_int31 = 0x3fffffff

(* As in rand_core.ml. Only meaningful on 64-bit hosts, like everything that
   uses it here. *)
let mask32 = (1 lsl 32) - 1

(* Verbatim copy of int_lemire_native's body from rand_core.ml (the [else]
   branch after the bound check), with [bits32 state] replaced by [next_x ()]
   so it can be driven by controlled test input instead of a live generator,
   and the comment inside left out. Do not "clean up" or restructure this away
   from the shipped shape -- its value as a correctness witness depends on
   staying a literal transcription. *)
let original_algorithm ~next_x bound =
  let open I64 in
  let x = of_int32 (next_x ()) land 0xffff_ffffL in
  let m = x * of_int bound in
  let l = to_int (m land 0xffff_ffffL) in
  if Int.(l >= bound) then to_int (m lsr 32)
  else begin
    let two_32_minus_bound = Int.(-bound land mask32) in
    let t = Int.(two_32_minus_bound mod bound) in
    if l >= t then to_int (m lsr 32)
    else
      let rec loop () =
        let x = of_int32 (next_x ()) land 0xffff_ffffL in
        let m = x * of_int bound in
        if to_int (m land 0xffff_ffffL) >= t then to_int (m lsr 32) else loop ()
      in
      loop ()
  end

(* Alternative "pure single attempt" shape: same arithmetic, but
   recomputes [t] on every attempt instead of caching it across retries. *)
let attempt_step bound x =
  let x = I64.(of_int32 x land 0xffff_ffffL) in
  let m = I64.(x * of_int bound) in
  let l = I64.(to_int (m land 0xffff_ffffL)) in
  if l >= bound then Some I64.(to_int (m lsr 32))
  else
    let t = -bound land mask32 mod bound in
    if l >= t then Some I64.(to_int (m lsr 32)) else None

let rec attempt_algorithm ~next_x bound =
  match attempt_step bound (next_x ()) with
  | Some v -> v
  | None -> attempt_algorithm ~next_x bound

(* Small, midsized, near-power-of-two, and extreme bounds -- boundary
   conditions in the reduction (the l >= bound short-circuit, the
   rejection threshold t) tend to concentrate around powers of two and the
   ends of the valid range. The bounds just below 2^30 are where the
   rejection rate is highest. *)
let bounds_to_test =
  [
    1;
    2;
    3;
    7;
    8;
    100;
    101;
    127;
    128;
    129;
    1000;
    1023;
    1024;
    1025;
    12345;
    65535;
    65536;
    65537;
    1 lsl 20;
    (1 lsl 29) - 1;
    1 lsl 29;
    (1 lsl 29) + 1;
    715827883;
    max_int31 - 1;
    max_int31;
  ]

(* Raw words to try for every bound: the ends of the range, both sides of
   the int32 sign boundary (2^31), and words near the bound itself. *)
let edge_xs bound =
  [
    0l;
    1l;
    I32.of_int (bound - 1);
    I32.of_int bound;
    I32.of_int (bound + 1);
    I32.max_int;
    I32.min_int;
    -2l;
    -1l;
  ]

(* 2^32 - 1 - delta, as the int32 with those bits. *)
let near_top delta = I32.sub (-1l) (I32.of_int delta)

(* The words whose product with [bound] has a low half just above 0, i.e.
   the smallest x past each multiple of 2^32 / bound, and the words either
   side of it: the low half then sweeps the rejection boundary at t =
   2^32 mod bound. Only for the first [k_max] multiples. 64-bit only (k lsl
   32). *)
let threshold_xs ~k_max bound =
  List.concat_map
    (List.range 1 (Int.min k_max bound) ~stop:`inclusive)
    ~f:(fun k ->
      let x = ((k lsl 32) + bound - 1) / bound in
      List.filter_map
        [ x - 1; x; x + 1 ]
        ~f:(fun x ->
          if x >= 0 && x <= mask32 then Some (I32.of_int x) else None))

let random_x state = Random.State.int32_incl state I32.min_int I32.max_int

let show = function None -> "None" | Some v -> Int.to_string v

(* ---- attempt_step vs. the independent C reference (lemire_ref_stubs.c) ---- *)

let check_against_reference ~bound ~x =
  let expected = Lemire_ref.attempt ~bound ~x in
  let actual = attempt_step bound x in
  if not (Option.equal Int.equal expected actual) then
    failwith
      (Stdlib.Printf.sprintf
         "lemire attempt mismatch: bound=%d x=%lu reference=%s attempt_step=%s"
         bound x (show expected) (show actual))

(* attempt_step is a literal transcription of int_lemire_native's
   arithmetic (see the module doc comment above), which is only correct on
   64-bit hosts: the low half of the product, up to 2^32 - 1, has to fit in
   a native int after Int64.to_int, which only a 63-bit (64-bit host) int
   guarantees (see rand_core.ml's comment on int_lemire_native). *)

let%test_unit "attempt_step matches independent reference: dense small-x sweep"
    =
  if Stdlib.Sys.word_size = 64 then
    List.iter bounds_to_test ~f:(fun bound ->
        for x = 0 to 4999 do
          check_against_reference ~bound ~x:(I32.of_int x)
        done)

let%test_unit "attempt_step matches independent reference: x near 2^32" =
  if Stdlib.Sys.word_size = 64 then
    List.iter bounds_to_test ~f:(fun bound ->
        for delta = 0 to 4999 do
          check_against_reference ~bound ~x:(near_top delta)
        done)

let%test_unit "attempt_step matches independent reference: edge x values" =
  if Stdlib.Sys.word_size = 64 then
    List.iter bounds_to_test ~f:(fun bound ->
        List.iter (edge_xs bound) ~f:(fun x ->
            check_against_reference ~bound ~x))

let%test_unit
    "attempt_step matches independent reference: x at the rejection boundary" =
  if Stdlib.Sys.word_size = 64 then
    List.iter bounds_to_test ~f:(fun bound ->
        List.iter (threshold_xs ~k_max:200 bound) ~f:(fun x ->
            check_against_reference ~bound ~x))

let%test_unit "attempt_step matches independent reference: random sampling" =
  if Stdlib.Sys.word_size = 64 then begin
    let state = Random.State.make [| 0x1357_9bdf |] in
    for _ = 1 to 20000 do
      let bound = 1 + Random.State.int state max_int31 in
      check_against_reference ~bound ~x:(random_x state)
    done
  end

(* ---- Xoshiro_simd.for_testing_lemire_reduce32_wide vs. the independent C
   reference (lemire_ref_stubs.c) ----

   for_testing_lemire_reduce32_wide is the raw, retry-loop-free reduction
   step behind Xs_S.for_testing_int_lemire_c (the 32-bit-safe counterpart to
   int_lemire_native, and int_lemire_c's own algorithm -- see rand_core.ml
   and xoshiro_simd.mli; it's what [int] itself becomes on a 32-bit host).
   Compared directly against Lemire_ref.attempt, NOT attempt_step: this used
   to compare against attempt_step, but attempt_step is 64-bit only, same as
   int_lemire_native (see the module doc comment above and the gated tests
   just above) -- on real i386 hardware the 30-bit version's `x * bound`
   silently overflowed a 31-bit native int for plenty of the bounds under
   test, so this comparison was actually checking the correct wide-C result
   against attempt_step's garbage and reporting the mismatch as a wide_c
   bug. Lemire_ref.attempt and the wide-C path are both genuine uint64_t C
   arithmetic, safe and meaningful on any host width, so this comparison
   needs no gating -- correctness of the wide-C path matters most on the
   very hosts where attempt_step/int_lemire_native can't run at all. Only
   the rejection-boundary test is gated, because computing its words needs
   a 64-bit int. *)

let check_wide_c_against_reference ~bound ~x =
  let expected = Lemire_ref.attempt ~bound ~x in
  let actual = Xoshiro_simd.for_testing_lemire_reduce32_wide bound x in
  if not (Option.equal Int.equal expected actual) then
    failwith
      (Stdlib.Printf.sprintf
         "lemire_reduce32_wide mismatch: bound=%d x=%lu reference=%s wide_c=%s"
         bound x (show expected) (show actual))

let%test_unit
    "lemire_reduce32_wide matches independent reference: dense small-x sweep" =
  List.iter bounds_to_test ~f:(fun bound ->
      for x = 0 to 4999 do
        check_wide_c_against_reference ~bound ~x:(I32.of_int x)
      done)

let%test_unit "lemire_reduce32_wide matches independent reference: x near 2^32"
    =
  List.iter bounds_to_test ~f:(fun bound ->
      for delta = 0 to 4999 do
        check_wide_c_against_reference ~bound ~x:(near_top delta)
      done)

let%test_unit
    "lemire_reduce32_wide matches independent reference: edge x values" =
  List.iter bounds_to_test ~f:(fun bound ->
      List.iter (edge_xs bound) ~f:(fun x ->
          check_wide_c_against_reference ~bound ~x))

let%test_unit
    "lemire_reduce32_wide matches independent reference: x at the rejection \
     boundary" =
  if Stdlib.Sys.word_size = 64 then
    List.iter bounds_to_test ~f:(fun bound ->
        List.iter (threshold_xs ~k_max:200 bound) ~f:(fun x ->
            check_wide_c_against_reference ~bound ~x))

let%test_unit
    "lemire_reduce32_wide matches independent reference: random sampling" =
  (* The seed must fit in a native int on any host, including 32-bit ones,
     since this test (unlike most others in this file) deliberately isn't
     gated to 64-bit -- 0x7766_5544 didn't (exceeds 32-bit max_int), only
     ever compiled here because nothing had run this file's tests on a
     32-bit host before. *)
  let state = Random.State.make [| 0x1766_5544 |] in
  for _ = 1 to 20000 do
    (* max_int31 is the largest bound Base.Random.State.int accepts on 32-bit
       hosts, so this leaves out bound = max_int31 itself, which
       bounds_to_test covers. *)
    let bound = 1 + Random.State.int state max_int31 in
    check_wide_c_against_reference ~bound ~x:(random_x state)
  done

(* ---- Xs_S.for_testing_int_lemire_c: range/termination, live generator ----
   The reduction step itself is already proven correct above; this just
   confirms the retry loop and bits32 wiring around it hold to
   int_lemire_native's own contract, and stays in range on any host width
   (deliberately not gated the way the int_lemire_native-based tests
   elsewhere in this file are). *)

let hist_seed = 0x3344_5566_7788_99aaL

let%test_unit "int_lemire_c small/large bounds terminate and stay in range" =
  let state = Xs_S.init_deterministic hist_seed in
  List.iter [ 1; 2; 3; 7; 100; 0x3FFFFFFF ] ~f:(fun bound ->
      for _ = 1 to 2000 do
        let r = Xs_S.for_testing_int_lemire_c state bound in
        assert (r >= 0 && r < bound)
      done)

let%test "int_lemire_c raises on invalid bound (<= 0)" =
  let state = Xs_S.init () in
  (try
     ignore (Xs_S.for_testing_int_lemire_c state 0);
     false
   with Invalid_argument _ -> true)
  &&
    try
      ignore (Xs_S.for_testing_int_lemire_c state (-1));
      false
    with Invalid_argument _ -> true

(* 0x40000000 (2^30, the too-large-bound boundary) doesn't fit in a native
   int on a 32-bit host at all -- max_int there is 2^30 - 1, the same
   representability limit as test_numeric.ml's "full_int > 30 bit range"
   tests. Not a platform-dependent semantic difference: the bound < 2^30
   contract itself is identical on every host, only this specific literal's
   representability isn't, so this check is gated rather than moved into
   the unconditional test above. *)
let%test_unit "int_lemire_c raises on invalid bound (too large)" =
  if Stdlib.Sys.word_size = 64 then
    let state = Xs_S.init () in
    assert (
      try
        ignore (Xs_S.for_testing_int_lemire_c state 0x40000000);
        false
      with Invalid_argument _ -> true)

(* ---- int_lemire_c matches int_lemire_native bit-for-bit on 64-bit hosts:
   both should implement the identical algorithm end to end (bits32 draw +
   reduction), differing only in the reduction's arithmetic, so given the
   same seed and the same bound sequence they must produce identical output
   wherever int_lemire_native itself is safe to run. int_lemire_native has
   no for_testing_ alias of its own -- on a 64-bit host, [Xs_S.int] already
   IS int_lemire_native (see rand_core.ml), so it stands in directly for the
   native side of this comparison. Gated on word size for the same reason
   the other int_lemire_native-based tests in this file are -- this only
   proves agreement on the width where both sides can run; the wide-C path's
   correctness on 32-bit hosts is established independently above, via the
   C reference, not via this test.

   Runs bounds_to_test (including the power-of-two bounds, where t = 0 and
   the retry/reject path is unreachable) before the random-bound sweep --
   x itself can't be injected here the way attempt_step/wide_c tests do it,
   since both sides draw it from their own live generator, so bound is the
   only axis this test can exercise edge cases on. *)

let%test_unit "int_lemire_c matches int (int_lemire_native) on 64-bit hosts" =
  if Stdlib.Sys.word_size = 64 then begin
    let seed = 0x0f1e_2d3c_4b5a_6978L in
    let state_a = Xs_S.init_deterministic seed in
    let state_b = Xs_S.init_deterministic seed in
    let check bound =
      let a = Xs_S.int state_a bound in
      let b = Xs_S.for_testing_int_lemire_c state_b bound in
      if a <> b then
        failwith
          (Stdlib.Printf.sprintf
             "int_lemire_native/int_lemire_c mismatch: bound=%d native=%d \
              wide_c=%d"
             bound a b)
    in
    (* bounds_to_test first, so the power-of-two (zero-rejection-threshold)
       and near-power-of-two bounds get many draws each against both live
       generators, not just whatever bound a single random draw happens to
       land on below. *)
    List.iter bounds_to_test ~f:(fun bound ->
        for _ = 1 to 2000 do
          check bound
        done);
    (* Must still fit in a native int on any host despite this whole branch
       being 64-bit-only at runtime: the compiler parses/types the literal
       for whichever host it's building for regardless of which branch of a
       runtime [if] it sits in -- 0x8899_aabb didn't (exceeds 32-bit
       max_int), and failed to compile on 32 bit hosts as a result. *)
    let rand_state = Random.State.make [| 0x1899_aabb |] in
    for _ = 1 to 20000 do
      let bound = 1 + Random.State.int rand_state max_int31 in
      check bound
    done
  end

(* ---- original_algorithm vs. attempt_algorithm ---- *)

(* Both algorithms driven by the same materialized array of draws (rather
   than two closures sharing one mutable RNG) so that however many draws
   each one ends up consuming, they see an identical sequence -- required
   for the comparison to isolate a difference in the *decision* (accept
   vs. reject) rather than an artifact of the two calls being interleaved
   against a single shared draw source. *)
let make_next_x_from_array arr =
  let idx = ref 0 in
  fun () ->
    let v = arr.(!idx) in
    Int.incr idx;
    v

(* 1 (the deliberately-chosen first draw under test) + 63 random-tail
   draws to cover any retries. The rejection probability is (2^32 mod
   bound) / 2^32 < bound / 2^32 < 1/4, so the chance of 64 consecutive
   draws all rejecting is below 0.25^64 -- for all practical purposes
   impossible to hit across even 20000+ test cases. *)
let tail_length = 63

let xs_for ~rand_state ~first =
  Array.append [| first |]
    (Array.init tail_length ~f:(fun _ -> random_x rand_state))

let check_algorithms_equivalent ~bound ~xs =
  let a = original_algorithm ~next_x:(make_next_x_from_array xs) bound in
  let b = attempt_algorithm ~next_x:(make_next_x_from_array xs) bound in
  if a <> b then
    failwith
      (Stdlib.Printf.sprintf
         "original_algorithm/attempt_algorithm mismatch: bound=%d original=%d \
          attempt=%d"
         bound a b)

(* Both original_algorithm and attempt_algorithm run int_lemire_native's
   arithmetic -- 64-bit-only, same reasoning as above. *)

let%test_unit
    "original_algorithm matches attempt_algorithm: dense small-x sweep" =
  if Stdlib.Sys.word_size = 64 then begin
    let rand_state = Random.State.make [| 0x2468_ace0 |] in
    List.iter bounds_to_test ~f:(fun bound ->
        for x = 0 to 999 do
          check_algorithms_equivalent ~bound
            ~xs:(xs_for ~rand_state ~first:(I32.of_int x))
        done)
  end

let%test_unit "original_algorithm matches attempt_algorithm: x near 2^32" =
  if Stdlib.Sys.word_size = 64 then begin
    let rand_state = Random.State.make [| 0x1122_3344 |] in
    List.iter bounds_to_test ~f:(fun bound ->
        for delta = 0 to 999 do
          check_algorithms_equivalent ~bound
            ~xs:(xs_for ~rand_state ~first:(near_top delta))
        done)
  end

let%test_unit "original_algorithm matches attempt_algorithm: edge x values" =
  if Stdlib.Sys.word_size = 64 then begin
    let rand_state = Random.State.make [| 0x1566_7788 |] in
    List.iter bounds_to_test ~f:(fun bound ->
        List.iter (edge_xs bound) ~f:(fun x ->
            check_algorithms_equivalent ~bound ~xs:(xs_for ~rand_state ~first:x)))
  end

let%test_unit
    "original_algorithm matches attempt_algorithm: x at the rejection boundary"
    =
  if Stdlib.Sys.word_size = 64 then begin
    let rand_state = Random.State.make [| 0x0bad_cafe |] in
    List.iter bounds_to_test ~f:(fun bound ->
        List.iter (threshold_xs ~k_max:200 bound) ~f:(fun x ->
            check_algorithms_equivalent ~bound ~xs:(xs_for ~rand_state ~first:x)))
  end

let%test_unit "original_algorithm matches attempt_algorithm: random sampling" =
  if Stdlib.Sys.word_size = 64 then begin
    let rand_state = Random.State.make [| 0x2199_abcc |] in
    for _ = 1 to 20000 do
      let bound = 1 + Random.State.int rand_state max_int31 in
      let first = random_x rand_state in
      check_algorithms_equivalent ~bound ~xs:(xs_for ~rand_state ~first)
    done
  end
