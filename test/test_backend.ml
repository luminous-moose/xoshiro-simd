open Base
open! Stdio
open! Stdlib.Printf
module Xs_S = Xoshiro_simd.State
module Xs = Xoshiro_simd

let seed = 42L

(* --- backend availability and selection plumbing --- *)

let%test_unit "Plain always in available_backends" =
  let vs = Xs.available_backends () in
  assert (List.mem vs Xs.Plain ~equal:Xs.backend_equal)

let%test_unit "state_backend reflects actual backend in use" =
  let state = Xs_S.init_deterministic ~backend:Xs.Plain seed in
  assert (Xs.backend_equal (Xs_S.state_backend state) Xs.Plain)

(* --- bits64 cross-backend agreement --- *)

let%test_unit "all available backends produce identical output to Plain" =
  let backends = Xs.available_backends () in
  List.iter backends ~f:(fun v ->
      let plain = Xs_S.init_deterministic ~backend:Xs.Plain seed in
      let other = Xs_S.init_deterministic ~backend:v seed in
      for _ = 1 to 20_000 do
        let a = Xs_S.bits64 plain in
        let b = Xs_S.bits64 other in
        assert (Int64.equal a b)
      done)

(* --- fill_float32_bigarray cross-backend agreement --- *)

(* The fill_* functions dispatch through the state's backend (see the
   xs256_fill_*_fn pointers in xs256_stubs.c), so this exercises the real
   SSE2/AVX2/NEON/AVX512FVL fill kernels against Plain -- same shape as
   "all available backends produce identical output to Plain" above. *)
let%test_unit
    "all available backends produce identical fill_float32_bigarray output to \
     Plain" =
  let backends = Xs.available_backends () in
  (* Sweep every length, not just one fixed n: a backend-specific remainder-
     handling bug (wrong block size, off-by-one, etc.) may only manifest at
     particular residues mod the 8-float32 logical block, and a single fixed
     n can miss it or only catch it by chance. Same max_len as the prefix-
     stability sweep in test_primitives.ml (3 full periods of the block). *)
  let max_len = 24 in
  List.iter backends ~f:(fun v ->
      for n = 0 to max_len do
        let plain = Xs_S.init_deterministic ~backend:Xs.Plain seed in
        let other = Xs_S.init_deterministic ~backend:v seed in
        let plain_ba =
          Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n
        in
        let other_ba =
          Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n
        in
        Xs_S.fill_float32_bigarray plain plain_ba;
        Xs_S.fill_float32_bigarray other other_ba;
        for i = 0 to n - 1 do
          assert (
            Float.equal
              (Bigarray.Array1.get plain_ba i)
              (Bigarray.Array1.get other_ba i))
        done
      done)

(* Same fill, but with a single state reused across every size instead of a
   fresh one each time. The test above resets per iteration, so it can never
   catch a bug where a backend's state is left wrong after a fill (e.g. the
   tail-buffer/remainder path silently leaving the lane state advanced by the
   wrong number of blocks) -- every call there is a first call on that state.
   Here each size's fill continues from wherever the previous size's fill
   left off, so a state left wrong by any earlier size surfaces as a mismatch
   at some later size. *)
let%test_unit
    "all available backends produce identical fill_float32_bigarray output to \
     Plain without resetting state between sizes" =
  let backends = Xs.available_backends () in
  let max_len = 24 in
  List.iter backends ~f:(fun v ->
      let plain = Xs_S.init_deterministic ~backend:Xs.Plain seed in
      let other = Xs_S.init_deterministic ~backend:v seed in
      for n = 0 to max_len do
        let plain_ba =
          Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n
        in
        let other_ba =
          Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n
        in
        Xs_S.fill_float32_bigarray plain plain_ba;
        Xs_S.fill_float32_bigarray other other_ba;
        for i = 0 to n - 1 do
          assert (
            Float.equal
              (Bigarray.Array1.get plain_ba i)
              (Bigarray.Array1.get other_ba i))
        done
      done)

(* A single very large fill. 10,000 elements is 1,250 periods of the 8-float32
   logical block. *)
let%test_unit
    "all available backends produce identical fill_float32_bigarray output to \
     Plain at 10,000 elements" =
  let backends = Xs.available_backends () in
  let n = 10_000 in
  List.iter backends ~f:(fun v ->
      let plain = Xs_S.init_deterministic ~backend:Xs.Plain seed in
      let other = Xs_S.init_deterministic ~backend:v seed in
      let plain_ba =
        Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n
      in
      let other_ba =
        Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n
      in
      Xs_S.fill_float32_bigarray plain plain_ba;
      Xs_S.fill_float32_bigarray other other_ba;
      for i = 0 to n - 1 do
        assert (
          Float.equal
            (Bigarray.Array1.get plain_ba i)
            (Bigarray.Array1.get other_ba i))
      done)

(* --- fill_float64_bigarray cross-backend agreement --- *)

let%test_unit
    "all available backends produce identical fill_float64_bigarray output to \
     Plain" =
  let backends = Xs.available_backends () in
  let max_len =
    16
    (* 4 full periods of the 4-double logical block *)
  in
  List.iter backends ~f:(fun v ->
      for n = 0 to max_len do
        let plain = Xs_S.init_deterministic ~backend:Xs.Plain seed in
        let other = Xs_S.init_deterministic ~backend:v seed in
        let plain_ba =
          Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n
        in
        let other_ba =
          Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n
        in
        Xs_S.fill_float64_bigarray plain plain_ba;
        Xs_S.fill_float64_bigarray other other_ba;
        for i = 0 to n - 1 do
          assert (
            Float.equal
              (Bigarray.Array1.get plain_ba i)
              (Bigarray.Array1.get other_ba i))
        done
      done)

(* Same fill, but with a single state reused across every size instead of a
   fresh one each time. *)
let%test_unit
    "all available backends produce identical fill_float64_bigarray output to \
     Plain without resetting state between sizes" =
  let backends = Xs.available_backends () in
  let max_len =
    16
    (* 4 full periods of the 4-double logical block *)
  in
  List.iter backends ~f:(fun v ->
      let plain = Xs_S.init_deterministic ~backend:Xs.Plain seed in
      let other = Xs_S.init_deterministic ~backend:v seed in
      for n = 0 to max_len do
        let plain_ba =
          Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n
        in
        let other_ba =
          Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n
        in
        Xs_S.fill_float64_bigarray plain plain_ba;
        Xs_S.fill_float64_bigarray other other_ba;
        for i = 0 to n - 1 do
          assert (
            Float.equal
              (Bigarray.Array1.get plain_ba i)
              (Bigarray.Array1.get other_ba i))
        done
      done)

(* A single very large fill: 10,000 elements is 2,500 periods of the
   4-double logical block, far beyond anything the sweeps above reach, so
   this is a stress test for anything that only surfaces deep into a long
   fill (loop-counter issues, accumulated drift, etc.), on one persistent
   state rather than many short-lived ones. *)
let%test_unit
    "all available backends produce identical fill_float64_bigarray output to \
     Plain at 10,000 elements" =
  let backends = Xs.available_backends () in
  let n = 10_000 in
  List.iter backends ~f:(fun v ->
      let plain = Xs_S.init_deterministic ~backend:Xs.Plain seed in
      let other = Xs_S.init_deterministic ~backend:v seed in
      let plain_ba =
        Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n
      in
      let other_ba =
        Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n
      in
      Xs_S.fill_float64_bigarray plain plain_ba;
      Xs_S.fill_float64_bigarray other other_ba;
      for i = 0 to n - 1 do
        assert (
          Float.equal
            (Bigarray.Array1.get plain_ba i)
            (Bigarray.Array1.get other_ba i))
      done)

(* --- fill_float32_range_bigarray cross-backend agreement --- *)

let%test_unit
    "all available backends produce identical fill_float32_range_bigarray \
     output to Plain" =
  let backends = Xs.available_backends () in
  let max_len =
    24
    (* 3 full periods of the 8-float32 logical block *)
  in
  List.iter backends ~f:(fun v ->
      for n = 0 to max_len do
        let plain = Xs_S.init_deterministic ~backend:Xs.Plain seed in
        let other = Xs_S.init_deterministic ~backend:v seed in
        let plain_ba =
          Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n
        in
        let other_ba =
          Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n
        in
        Xs_S.fill_float32_range_bigarray plain plain_ba ~min:(-9000.) ~max:9000.;
        Xs_S.fill_float32_range_bigarray other other_ba ~min:(-9000.) ~max:9000.;
        for i = 0 to n - 1 do
          assert (
            Float.equal
              (Bigarray.Array1.get plain_ba i)
              (Bigarray.Array1.get other_ba i))
        done
      done)

(* Same fill, but with a single state reused across every size instead of a
   fresh one each time. *)
let%test_unit
    "all available backends produce identical fill_float32_range_bigarray \
     output to Plain without resetting state between sizes" =
  let backends = Xs.available_backends () in
  let max_len =
    24
    (* 3 full periods of the 8-float32 logical block *)
  in
  List.iter backends ~f:(fun v ->
      let plain = Xs_S.init_deterministic ~backend:Xs.Plain seed in
      let other = Xs_S.init_deterministic ~backend:v seed in
      for n = 0 to max_len do
        let plain_ba =
          Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n
        in
        let other_ba =
          Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n
        in
        Xs_S.fill_float32_range_bigarray plain plain_ba ~min:(-9000.) ~max:9000.;
        Xs_S.fill_float32_range_bigarray other other_ba ~min:(-9000.) ~max:9000.;
        for i = 0 to n - 1 do
          assert (
            Float.equal
              (Bigarray.Array1.get plain_ba i)
              (Bigarray.Array1.get other_ba i))
        done
      done)

let%test_unit
    "all available backends produce identical fill_float32_range_bigarray \
     output to Plain at 10,000 elements" =
  let backends = Xs.available_backends () in
  let n = 10_000 in
  List.iter backends ~f:(fun v ->
      let plain = Xs_S.init_deterministic ~backend:Xs.Plain seed in
      let other = Xs_S.init_deterministic ~backend:v seed in
      let plain_ba =
        Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n
      in
      let other_ba =
        Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n
      in
      Xs_S.fill_float32_range_bigarray plain plain_ba ~min:(-9000.) ~max:9000.;
      Xs_S.fill_float32_range_bigarray other other_ba ~min:(-9000.) ~max:9000.;
      for i = 0 to n - 1 do
        assert (
          Float.equal
            (Bigarray.Array1.get plain_ba i)
            (Bigarray.Array1.get other_ba i))
      done)

(* Tiny float32 ranges: the same idea as the float64 test below. The canonical
   definition is (u24 * 2^-24) * scale + offset, and a backend that computes
   u24 * (scale * 2^-24) instead is only equal to it while scale * 2^-24 is
   exact in float32, i.e. |scale| >= 2^-102 (about 1.97e-31; 2^-126 is the
   smallest normal float32, and scale has 24 bits). Below that the prescaled
   scale is subnormal and loses its low bits.

   The scale is passed as a double and narrowed to float32 inside the fill, so
   every scale here is first rounded to float32; otherwise the narrowing would
   move it off the value the comments below describe. As in the float64 test,
   the offset is 0 for the one-sided ranges, since a large one would swallow the
   tiny values. The symmetric ranges then check that a tiny offset is applied
   the same way too. *)
let%test_unit
    "all available backends produce identical fill_float32_range_bigarray \
     output to Plain for tiny ranges" =
  let to_float32 x = Int32.float_of_bits (Int32.bits_of_float x) in
  let threshold = Float.ldexp 1. (-102) in
  let scales =
    [
      (* Comfortably above the threshold, where prescaling is exact, and then
         well below it but still normal. *)
      to_float32 1e-30;
      to_float32 1e-32;
      to_float32 1e-36;
      (* The smallest normal float32 and a full-mantissa one just above it. *)
      Float.ldexp 1. (-126);
      Float.ldexp (Float.of_int 0xffffff) (-125);
      (* The scale is itself subnormal. *)
      to_float32 1e-40;
      to_float32 1e-44;
      Float.ldexp 3. (-149);
      Float.ldexp 1. (-149);
      (* Around 2^-102, where scale * 2^-24 stops being exact. The first has
         all 24 mantissa bits set and is the largest float32 below the
         threshold: prescaled it is subnormal and rounds to 2^-126, which is
         not just off in the last place, so it must take the exact path. A
         threshold that is too low (say 2^-103) sends it down the prescaled path
         and fails here. A threshold that is too high, or > instead of >=, only
         costs speed: the exact path is right for every scale, so nothing can
         detect that. *)
      Float.ldexp (Float.of_int 0xffffff) (-126);
      threshold;
      Float.ldexp (Float.of_int ((1 lsl 23) + 1)) (-125);
      Float.ldexp 1.5 (-102);
    ]
  in
  let check v ~min ~max n =
    let plain = Xs_S.init_deterministic ~backend:Xs.Plain seed in
    let other = Xs_S.init_deterministic ~backend:v seed in
    let plain_ba =
      Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n
    in
    let other_ba =
      Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n
    in
    Xs_S.fill_float32_range_bigarray plain plain_ba ~min ~max;
    Xs_S.fill_float32_range_bigarray other other_ba ~min ~max;
    for i = 0 to n - 1 do
      assert (
        Float.equal
          (Bigarray.Array1.get plain_ba i)
          (Bigarray.Array1.get other_ba i))
    done
  in
  List.iter (Xs.available_backends ()) ~f:(fun v ->
      List.iter scales ~f:(fun scale ->
          for n = 0 to 24 do
            check v ~min:0. ~max:scale n;
            (* A tiny range centered on 0. For the subnormal scales half of the
               scale is rounded when narrowed, but identically in both backends. *)
            check v ~min:(-.scale /. 2.) ~max:(scale /. 2.) n
          done))

(* Cancellation to a subnormal.

   The float32 range fill's prescale-vs-exact choice -- the branch shared by
   every backend's "common" kernel (SSE2, AVX2, AVX512FVL, NEON) -- looks
   only at `scale`: once |scale| clears the threshold (2^-102), folding
   XS256_FLOAT_SCALE into `scale` ahead of time (one multiply instead of
   two for every draw) is taken as safe regardless of `offset`. That's fine for
   every backend except 32-bit ARM, where NEON hardware flushes subnormal
   *results* (not inputs) to zero. It is possible for a nonzero offset to
   produce results that cancel to subnormals despite a scale above the
   threshold, and 32-bit NEON would return them as zero, so that platform has to
   take the value of the offset into account as well when deciding to branch,
   and it has to branch to the plain-C code to preserve those subnormals. This
   test ensures that that is done correctly, by carefully constructing an offset
   that creates such a cancellation.

   Landmarks on the magnitude line, smallest to largest:

     0 < 2^-149 (smallest subnormal float32) < 2^-126 (smallest normal) <
     p < below = 2^-103 < scale (2^-102, 1.5x or 1.75x it -- just above the
     prescale threshold, so comfortably normal)

   First, draw 32 ordinary Plain values from [0, scale) with offset 0, so
   nothing is cancelled yet -- this just exposes the raw quantization steps
   at this scale. Search those 32 for one, `p`, that lands below `below`.
   `below` (2^-103) is exactly half of the smallest `scale` tried (2^-102),
   down to about 0.29x the largest (1.75x2^-102), so each draw's odds of
   landing under it are `below / scale` -- 0.5 down to ~0.29 depending on
   which scale is running, not a rare event. `p` can be any draw whose u24
   happens to fall under that threshold, not necessarily the single smallest
   nonzero step -- it's still normal here (the fill's granularity at this scale
   bottoms out exactly at 2^-126) but small next to scale.

   Take `ulp` as p's own float32 spacing -- the gap between p and the next
   representable float32 below it, i.e. the weight of p's lowest mantissa
   bit (works out somewhere between 2^-149 and 2^-127 depending on exactly
   how small p is, but always itself subnormal-sized).

   Build `offset` as -(p - ulp): one ulp short of exactly cancelling p. Redraw
   at (offset, offset + scale) -- same seed, same index, since the raw bits
   drawn don't depend on min/max -- and the sum at that index lands exactly
   on `ulp`: below 2^-126, i.e. subnormal, by construction rather than luck.
   p, offset, and offset + scale are all normal-sized (offset is just -p;
   offset + scale is just under scale); it's only the sum, after this
   near-total cancellation, that's forced down into subnormal territory.

   The two asserts right after the second fill check that the construction
   actually landed where intended -- sum equals ulp, and sum is a nonzero
   value under 2^-126 -- so the test can't pass vacuously if it didn't. Only
   then does it compare every available backend's output over the whole
   block against Plain at this (offset, offset + scale) range. Scale and
   offset are exact in a double, so max -. min gives the scale back exactly. *)
let%expect_test
    "all available backends produce identical fill_float32_range_bigarray \
     output to Plain when scaled value and offset cancel to a subnormal" =
  let seed = 123456L in
  let n = 32 in
  let print_float_and_bits d =
    let f = Float_fmt.sprintf_g 9 d in
    printf "%15s " f;
    let bits = Stdlib.Int32.bits_of_float d in
    printf "0x%08lx " bits;
    if Float.( <> ) d 0. && Float.( < ) (Float.abs d) (Float.ldexp 1. (-126))
    then printf "subnormal";
    printf "\n"
  in
  let make backend min max =
    let state = Xs_S.init_deterministic ~backend seed in
    let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n in
    Xs_S.fill_float32_range_bigarray state ba ~min ~max;
    ba
  in
  let below = Float.ldexp 1. (-103) in
  let run scale =
    let unshifted = make Xs.Plain 0. scale in
    let j =
      match
        List.find (List.range 0 n) ~f:(fun i ->
            let p = Bigarray.Array1.get unshifted i in
            Float.( > ) p 0. && Float.( < ) p below)
      with
      | Some j -> j
      | None -> failwith "no suitable output below 2^-103 in the first 32"
    in
    let p = Bigarray.Array1.get unshifted j in
    let _, e = Float.frexp p in
    let ulp = Float.ldexp 1. (e - 24) in
    let offset = -.(p -. ulp) in
    let plain = make Xs.Plain offset (offset +. scale) in
    let sum = Bigarray.Array1.get plain j in
    assert (Float.equal sum ulp);
    (* An element read back is a double, where a subnormal float32 is an
       ordinary number: compare against the smallest normal float32. *)
    assert (Float.( > ) sum 0. && Float.( < ) sum (Float.ldexp 1. (-126)));
    List.iter (Xs.available_backends ()) ~f:(fun v ->
        let other = make v offset (offset +. scale) in
        for i = 0 to n - 1 do
          assert (
            Float.equal
              (Bigarray.Array1.get plain i)
              (Bigarray.Array1.get other i))
        done);
    (* Unscaled by offset -- just the raw quantization steps at this scale,
       the "unshifted" array the search above draws `p` from. *)
    printf "scale: ";
    print_float_and_bits scale;
    print_endline "";
    for i = 0 to n - 1 do
      print_float_and_bits (Bigarray.Array1.get unshifted i)
    done;
    print_endline "";
    (* Same draw, offset applied: index j (where `p` was) now lands on `ulp`,
       a subnormal, by the cancellation this test is about. *)
    printf "offset: ";
    print_float_and_bits offset;
    print_endline "";
    for i = 0 to n - 1 do
      print_float_and_bits (Bigarray.Array1.get plain i)
    done
  in
  run (Float.ldexp 1. (-102));
  [%expect
    {|
    scale:  1.97215226e-31 0x0c800000

     1.42199657e-31 0x0c3895f1
     1.87223078e-31 0x0c73078a
      1.8732176e-31 0x0c732855
     9.05210231e-32 0x0beb018a
     1.34518531e-31 0x0c2e9d73
     1.21371026e-31 0x0c1d8c71
     1.97369381e-32 0x0accf5c8
     1.55626824e-31 0x0c4a03e2
     1.30458526e-31 0x0c295848
     1.70612931e-31 0x0c5d77df
     1.79419312e-31 0x0c68e64b
     1.50744667e-31 0x0c43ad82
     3.92147149e-32 0x0b4b9d4c
     2.84414385e-33 0x096c4840
     1.91332712e-31 0x0c785d33
     9.45076887e-32 0x0bf55b22
     1.18884021e-31 0x0c1a51fe
     7.02888845e-32 0x0bb67afa
     7.49879467e-32 0x0bc2ae0a
     1.07390989e-31 0x0c0b66c7
     1.61051789e-31 0x0c510ea3
     1.83357676e-31 0x0c6e030a
     1.46953157e-31 0x0c3ec190
     7.93445639e-32 0x0bcdfd82
     4.98232693e-32 0x0b81593e
     9.96720468e-33 0x0a4f02c0
      1.4804438e-31 0x0c402c2f
      2.5073318e-32 0x0b023028
     1.20099553e-31 0x0c1be5ec
      1.7926377e-31 0x0c68b29b
     7.62877026e-33 0x0a1e7180
      1.0949349e-31 0x0c0e2174

    offset: -9.05210172e-32 0x8beb0189

     5.16786402e-32 0x0b862a59
     9.67020604e-32 0x0bfb0d8b
     9.68007432e-32 0x0bfb4f21
     5.87747175e-39 0x00400000 subnormal
     4.39975134e-32 0x0b6472ba
     3.08500088e-32 0x0b202eb2
    -7.07840792e-32 0x8bb7c417
     6.51058067e-32 0x0ba9063b
     3.99375087e-32 0x0b4f5e0e
     8.00919138e-32 0x0bcfee35
     8.88982943e-32 0x0be6cb0d
     6.02236495e-32 0x0b9c597b
    -5.13063023e-32 0x8b8532e3
    -8.76768734e-32 0x8be39f47
     1.00811689e-31 0x0c02dc6e
     3.98667146e-33 0x09a59990
     2.83630036e-32 0x0b1344e6
    -2.02321327e-32 0x8ad21a3c
    -1.55330705e-32 0x8aa14dfc
      1.6869972e-32 0x0aaf3014
     7.05307719e-32 0x0bb71bbd
     9.28366588e-32 0x0bf1048b
     5.64321395e-32 0x0b928197
    -1.11764534e-32 0x8a682038
    -4.06977479e-32 0x8b535096
    -8.05538126e-32 0x8bd12131
     5.75233627e-32 0x0b9556d5
    -6.54476992e-32 0x8ba9e975
     2.95785353e-32 0x0b19949e
     8.87427529e-32 0x0be663ad
     -8.2892247e-32 0x8bd73359
     1.89724729e-32 0x0ac5057c
    |}];
  run (Float.ldexp 1.5 (-102));
  [%expect
    {|
    scale:  2.95822839e-31 0x0cc00000

     2.13299492e-31 0x0c8a7075
     2.80834628e-31 0x0cb645a8
     2.80982646e-31 0x0cb65e40
     1.35781541e-31 0x0c304128
      2.0177779e-31 0x0c82f616
     1.82056545e-31 0x0c6c52aa
     2.96054071e-32 0x0b19b856
     2.33440248e-31 0x0c9782ea
     1.95687789e-31 0x0c7e046c
     2.55919391e-31 0x0ca619e7
     2.69128961e-31 0x0caeacb8
     2.26117012e-31 0x0c92c222
     5.88220723e-32 0x0b98b5f9
     4.26621577e-33 0x09b13630
     2.86999062e-31 0x0cba45e6
     1.41761539e-31 0x0c38045a
     1.78326031e-31 0x0c677afd
     1.05433333e-31 0x0c08dc3c
     1.12481926e-31 0x0c120288
     1.61086478e-31 0x0c511a2a
     2.41577678e-31 0x0c9ccafa
     2.75036526e-31 0x0cb28248
     2.20429735e-31 0x0c8f112c
     1.19016852e-31 0x0c1a7e22
     7.47349039e-32 0x0bc205dd
      1.4950807e-32 0x0a9b4210
     2.22066564e-31 0x0c902123
      3.7609977e-32 0x0b43483c
     1.80149329e-31 0x0c69d8e2
     2.68895649e-31 0x0cae85f4
     1.14431554e-32 0x0a6daa40
     1.64240235e-31 0x0c55322e

    offset: -2.96054042e-32 0x8b19b855

     1.83694091e-31 0x0c6e72d5
     2.51229215e-31 0x0ca30e9d
     2.51377233e-31 0x0ca32735
     1.06176139e-31 0x0c09d313
     1.72172389e-31 0x0c5f7e17
     1.52451144e-31 0x0c45e495
     2.93873588e-39 0x00200000 subnormal
     2.03834835e-31 0x0c844bdf
     1.66082388e-31 0x0c579657
     2.26313978e-31 0x0c92e2dc
     2.39523548e-31 0x0c9b75ad
     1.96511611e-31 0x0c7f162f
     2.92166682e-32 0x0b17b39d
    -2.53391884e-32 0x8b03918f
     2.57393649e-31 0x0ca70edb
     1.12156138e-31 0x0c119645
      1.4872063e-31 0x0c410ce8
     7.58279315e-32 0x0bc4dc4e
     8.28765247e-32 0x0bd728e6
     1.31481077e-31 0x0c2aac15
     2.11972265e-31 0x0c8993ef
     2.45431113e-31 0x0c9f4b3d
     1.90824334e-31 0x0c77b443
     8.94114505e-32 0x0be8201a
     4.51294998e-32 0x0b6a5365
    -1.46545971e-32 0x8a982e9a
     1.92461163e-31 0x0c79d431
     8.00457286e-33 0x0a263f9c
     1.50543928e-31 0x0c436acd
     2.39290236e-31 0x0c9b4ee9
    -1.81622488e-32 0x8abc9b8a
     1.34634834e-31 0x0c2ec419
    |}];
  run (Float.ldexp 1.75 (-102));
  [%expect
    {|
    scale:  3.45126646e-31 0x0ce00000

     2.48849403e-31 0x0ca18333
     3.27640392e-31 0x0cd4a699
     3.27813072e-31 0x0cd4c34a
     1.58411793e-31 0x0c4da159
     2.35407437e-31 0x0c98c9c5
     2.12399298e-31 0x0c89dae3
     3.45396416e-32 0x0b33570f
     2.72346948e-31 0x0cb0c366
      2.2830242e-31 0x0c942d3f
     2.98572626e-31 0x0cc1c8e3
     3.13983804e-31 0x0ccbc982
     2.63803173e-31 0x0cab37d2
     6.86257481e-32 0x0bb229a2
     4.97725173e-33 0x09cebf38
     3.34832254e-31 0x0cd9518d
     1.65388458e-31 0x0c56afbe
     2.08047031e-31 0x0c8707be
     1.23005551e-31 0x0c1fab9b
      1.3122891e-31 0x0c2a5849
     1.87934228e-31 0x0c73f3dc
      2.8184064e-31 0x0cb6eccf
     3.20875939e-31 0x0cd042a9
     2.57168024e-31 0x0ca6e95e
      1.3885299e-31 0x0c343dd2
     8.71907183e-32 0x0be25c2c
     1.74426082e-32 0x0ab52268
     2.59077662e-31 0x0ca826a9
     4.38783065e-32 0x0b63d446
     2.10174205e-31 0x0c88692e
     3.13711607e-31 0x0ccb9c48
      1.3350348e-32 0x0a8aa350
     1.91613608e-31 0x0c78ba8b

    offset: -3.45396387e-32 0x8b33570e

     2.14309759e-31 0x0c8b1851
     2.93100747e-31 0x0cbe3bb7
     2.93273427e-31 0x0cbe5868
     1.23872161e-31 0x0c20cb96
     2.00867793e-31 0x0c825ee3
     1.77859654e-31 0x0c66e002
     2.93873588e-39 0x00200000 subnormal
     2.37807303e-31 0x0c9a5884
     1.93762776e-31 0x0c7b84ba
     2.64032982e-31 0x0cab5e01
     2.79444159e-31 0x0cb55ea0
     2.29263528e-31 0x0c94ccf0
     3.40861094e-32 0x0b30fc36
    -2.95623869e-32 0x8b197f27
      3.0029261e-31 0x0cc2e6ab
     1.30848814e-31 0x0c29d9fa
     1.73507386e-31 0x0c6139b8
     8.84659122e-32 0x0be5abaf
      9.6689271e-32 0x0bfb050b
     1.53394584e-31 0x0c471e18
     2.47300995e-31 0x0ca081ed
     2.86336294e-31 0x0cb9d7c7
      2.2262838e-31 0x0c907e7c
     1.04313345e-31 0x0c07680e
     5.26510796e-32 0x0b88b0a5
    -1.70970305e-32 0x8ab18bb4
     2.24538017e-31 0x0c91bbc7
     9.33866785e-33 0x0a41f4e0
     1.75634561e-31 0x0c63fc98
     2.79171962e-31 0x0cb53166
    -2.11892907e-32 0x8adc0acc
     1.57073975e-31 0x0c4be4c8
    |}]

(* --- fill_float64_range_bigarray cross-backend agreement --- *)

let%test_unit
    "all available backends produce identical fill_float64_range_bigarray \
     output to Plain" =
  let backends = Xs.available_backends () in
  let max_len =
    16
    (* 4 full periods of the 4-double logical block *)
  in
  List.iter backends ~f:(fun v ->
      for n = 0 to max_len do
        let plain = Xs_S.init_deterministic ~backend:Xs.Plain seed in
        let other = Xs_S.init_deterministic ~backend:v seed in
        let plain_ba =
          Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n
        in
        let other_ba =
          Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n
        in
        Xs_S.fill_float64_range_bigarray plain plain_ba ~min:(-9000.) ~max:9000.;
        Xs_S.fill_float64_range_bigarray other other_ba ~min:(-9000.) ~max:9000.;
        for i = 0 to n - 1 do
          assert (
            Float.equal
              (Bigarray.Array1.get plain_ba i)
              (Bigarray.Array1.get other_ba i))
        done
      done)

(* Same fill, but with a single state reused across every size instead of a
   fresh one each time. *)
let%test_unit
    "all available backends produce identical fill_float64_range_bigarray \
     output to Plain without resetting state between sizes" =
  let backends = Xs.available_backends () in
  let max_len =
    16
    (* 4 full periods of the 4-double logical block *)
  in
  List.iter backends ~f:(fun v ->
      let plain = Xs_S.init_deterministic ~backend:Xs.Plain seed in
      let other = Xs_S.init_deterministic ~backend:v seed in
      for n = 0 to max_len do
        let plain_ba =
          Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n
        in
        let other_ba =
          Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n
        in
        Xs_S.fill_float64_range_bigarray plain plain_ba ~min:(-9000.) ~max:9000.;
        Xs_S.fill_float64_range_bigarray other other_ba ~min:(-9000.) ~max:9000.;
        for i = 0 to n - 1 do
          assert (
            Float.equal
              (Bigarray.Array1.get plain_ba i)
              (Bigarray.Array1.get other_ba i))
        done
      done)

(* A single very large fill. *)
let%test_unit
    "all available backends produce identical fill_float64_range_bigarray \
     output to Plain at 10,000 elements" =
  let backends = Xs.available_backends () in
  let n = 10_000 in
  List.iter backends ~f:(fun v ->
      let plain = Xs_S.init_deterministic ~backend:Xs.Plain seed in
      let other = Xs_S.init_deterministic ~backend:v seed in
      let plain_ba =
        Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n
      in
      let other_ba =
        Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n
      in
      Xs_S.fill_float64_range_bigarray plain plain_ba ~min:(-9000.) ~max:9000.;
      Xs_S.fill_float64_range_bigarray other other_ba ~min:(-9000.) ~max:9000.;
      for i = 0 to n - 1 do
        assert (
          Float.equal
            (Bigarray.Array1.get plain_ba i)
            (Bigarray.Array1.get other_ba i))
      done)

(* Tiny ranges. The canonical definition of a float64 range fill is
   (m * 2^-53) * scale + offset, with m the 53 random bits, so that Plain, SSE2,
   AVX2 and NEON64 (which get m * 2^-53 directly) can all compute it. A backend
   that instead computes m * (scale * 2^-53) is only equal to it while
   scale * 2^-53 is exact, i.e. |scale| >= 2^-969 (about 2e-292). Below that
   the prescaled scale is subnormal and loses bits, so the output differs. A
   backend that prescales has to fall back to the exact form below that
   threshold (AVX512FVL does), and this fails if it doesn't. The offset has to
   be 0 here: with a large one these tiny values would vanish in the final add
   and hide the difference. *)
let%test_unit
    "all available backends produce identical fill_float64_range_bigarray \
     output to Plain for tiny ranges" =
  let threshold = Float.ldexp 1. (-969) in
  let scales =
    [
      1e-300;
      1e-305;
      1e-310;
      3e-320;
      (* Around 2^-969, where scale * 2^-53 stops being exact: the one just
         below it has its lowest mantissa bit set, so it must take the exact
         path. A threshold that is too low (say 2^-970) sends it down the
         prescaled path and fails here. A threshold that is too high, or >
         instead of >=, only costs speed: the exact path is right for every
         scale, so nothing can detect that. *)
      Float.one_ulp `Down threshold;
      threshold;
      Float.ldexp 1.5 (-969);
    ]
  in
  List.iter (Xs.available_backends ()) ~f:(fun v ->
      List.iter scales ~f:(fun scale ->
          for n = 0 to 16 do
            let plain = Xs_S.init_deterministic ~backend:Xs.Plain seed in
            let other = Xs_S.init_deterministic ~backend:v seed in
            let plain_ba =
              Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n
            in
            let other_ba =
              Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n
            in
            Xs_S.fill_float64_range_bigarray plain plain_ba ~min:0. ~max:scale;
            Xs_S.fill_float64_range_bigarray other other_ba ~min:0. ~max:scale;
            for i = 0 to n - 1 do
              assert (
                Float.equal
                  (Bigarray.Array1.get plain_ba i)
                  (Bigarray.Array1.get other_ba i))
            done
          done))

(* --- Byte fills cross-backend agreement --- *)

(* The byte fills (get_insecure_bytes / fill_insecure_bytes /
   fill_insecure_bigarray) all go through the backend's fill_blocks function,
   which none of the tests above exercise: they only cover the scalar block
   function (bits64) and the float fills. Sweep every length from 0 to 127 so
   every remainder mod the 32-byte block is hit with zero to three whole blocks
   before it. *)
let%test_unit
    "all available backends produce identical get_insecure_bytes to Plain" =
  List.iter (Xs.available_backends ()) ~f:(fun v ->
      for n = 0 to 127 do
        let plain = Xs_S.init_deterministic ~backend:Xs.Plain seed in
        let other = Xs_S.init_deterministic ~backend:v seed in
        assert (
          Bytes.equal
            (Xs_S.get_insecure_bytes plain n)
            (Xs_S.get_insecure_bytes other n));
        (* The state must have advanced by the same number of blocks: a
           follow-up byte draw and a float fill (which reads the lanes directly,
           bypassing the bitstream cache) must both still agree. *)
        assert (
          Bytes.equal
            (Xs_S.get_insecure_bytes plain 40)
            (Xs_S.get_insecure_bytes other 40));
        let float_ba () =
          Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout 8
        in
        let plain_ba = float_ba () in
        let other_ba = float_ba () in
        Xs_S.fill_float64_bigarray plain plain_ba;
        Xs_S.fill_float64_bigarray other other_ba;
        for i = 0 to 7 do
          assert (
            Float.equal
              (Bigarray.Array1.get plain_ba i)
              (Bigarray.Array1.get other_ba i))
        done
      done)

let%test_unit
    "all available backends: fill_insecure_bytes matches Plain and leaves the \
     rest of the buffer untouched" =
  let pad = 40 in
  List.iter (Xs.available_backends ()) ~f:(fun v ->
      for n = 0 to 127 do
        let expected =
          Xs_S.get_insecure_bytes
            (Xs_S.init_deterministic ~backend:Xs.Plain seed)
            n
        in
        let state = Xs_S.init_deterministic ~backend:v seed in
        let buf = Bytes.make (n + pad) '\xAA' in
        Xs_S.fill_insecure_bytes state buf n;
        assert (Bytes.equal (Bytes.sub buf ~pos:0 ~len:n) expected);
        for i = n to n + pad - 1 do
          assert (Char.equal (Bytes.get buf i) '\xAA')
        done
      done)

(* Bigarray data can start at any address: an Array1.sub view of a byte array
   at an odd offset hands the C side a destination with no alignment at all.
   This can fault on 32-bit arm if not handled correctly. Fill views at every
   offset 0-15 and check they match an aligned fill, and that the bytes on
   either side of the view were not touched. *)
let%test_unit
    "fill_insecure_bigarray into misaligned sub-views matches an aligned fill \
     and stays in bounds" =
  let sentinel = 0xAA in
  let pad = 16 in
  let create len =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout len
  in
  List.iter (Xs.available_backends ()) ~f:(fun v ->
      List.iter [ 0; 1; 7; 31; 32; 33; 63; 64; 65; 100 ] ~f:(fun n ->
          let reference = create n in
          Xs_S.fill_insecure_bigarray
            (Xs_S.init_deterministic ~backend:Xs.Plain seed)
            reference;
          for off = 0 to 15 do
            let parent = create (off + n + pad) in
            Bigarray.Array1.fill parent sentinel;
            let view = Bigarray.Array1.sub parent off n in
            Xs_S.fill_insecure_bigarray
              (Xs_S.init_deterministic ~backend:v seed)
              view;
            for i = 0 to off - 1 do
              assert (Bigarray.Array1.get parent i = sentinel)
            done;
            for i = 0 to n - 1 do
              assert (
                Bigarray.Array1.get view i = Bigarray.Array1.get reference i)
            done;
            for i = off + n to off + n + pad - 1 do
              assert (Bigarray.Array1.get parent i = sentinel)
            done
          done))

(* --- split cross-backend agreement --- *)

(* split draws the parent's own get_insecure_bytes output and feeds it
   through mix_entropy/init_string -- the draw itself is already covered by the
   byte-fill agreement above, but this is the only dedicated cross-backend check
   for split itself. *)
let%test_unit
    "all available backends produce an identical split result to Plain" =
  List.iter (Xs.available_backends ()) ~f:(fun v ->
      let plain_child =
        Xs_S.split (Xs_S.init_deterministic ~backend:Xs.Plain seed)
      in
      let other_child = Xs_S.split (Xs_S.init_deterministic ~backend:v seed) in
      let plain_dump = Xs_S.for_testing_raw_dump plain_child in
      let other_dump = Xs_S.for_testing_raw_dump other_child in
      assert (Array.equal Int64.equal plain_dump.s other_dump.s);
      for _ = 1 to 1000 do
        assert (Int64.equal (Xs_S.bits64 plain_child) (Xs_S.bits64 other_child))
      done)

(* --- split inherits its parent's backend --- *)

(* The cross-backend test above only compares output, which is identical on
   every backend, so it would pass even if split always built a Plain child.
   This checks the backend itself: State.split must keep the parent's, and the
   module-level split must keep the domain generator's, which
   init_deterministic ~backend sets. That replaces the domain generator, so it is
   put back to Auto afterward: the arch tests below read the domain generator's
   backend. *)
let%test_unit "split uses the same backend as its parent" =
  Exn.protect
    ~f:(fun () ->
      List.iter (Xs.available_backends ()) ~f:(fun v ->
          let parent = Xs_S.init_deterministic ~backend:v seed in
          assert (Xs.backend_equal (Xs_S.state_backend (Xs_S.split parent)) v);
          Xs.init_deterministic ~backend:v seed;
          assert (Xs.backend_equal (Xs_S.state_backend (Xs.split ())) v)))
    ~finally:(fun () -> Xs.self_init ())

(* --- Platform/arch dispatch guarantees --- *)

let%test "amd64 always supports SSE2" =
  if String.(Config.architecture = "amd64") then
    let backends = Xs.available_backends () in
    List.mem backends Xs.Sse2 ~equal:Xs.backend_equal
  else true

let%test "arm64 always supports NEON" =
  if String.(Config.architecture = "arm64") then
    let backends = Xs.available_backends () in
    List.mem backends Xs.Neon ~equal:Xs.backend_equal
  else true

let%test "amd64 backend chooses better than plain" =
  if String.(Config.architecture = "amd64") then
    let state = Xs.split () in
    let backend = Xs_S.state_backend state in
    not @@ Xs.backend_equal backend Plain
  else true

let%test "arm64 backend chooses better than plain" =
  if String.(Config.architecture = "arm64") then
    let state = Xs.split () in
    let backend = Xs_S.state_backend state in
    not @@ Xs.backend_equal backend Plain
  else true

(* --- set_backend_limit --- *)

(* set_backend_limit is process-global and sticky, so every test below restores
   it to "no restriction" via Exn.protect's
   ~finally, even on assertion failure -- otherwise a leftover restrictive
   limit from one test would silently corrupt every later test in this
   process that relies on AUTO picking the most capable backend. The
   first entry of [available_backends ()] is always what a limit of 0 (the
   default) would pick, since that function walks backend_table in the same
   order AUTO's search does. This deliberately doesn't use
   [set_backend_limit Auto], so a regression in that path fails only its own
   test below instead of leaving a limit behind for every later test. *)
let reset_backend_limit () =
  Xs.set_backend_limit (List.hd_exn (Xs.available_backends ()))

let%test_unit
    "set_backend_limit restricts AUTO to no more than the given backend" =
  Exn.protect ~finally:reset_backend_limit ~f:(fun () ->
      let least_capable = List.last_exn (Xs.available_backends ()) in
      Xs.set_backend_limit least_capable;
      let state = Xs_S.init ~backend:Xs.Auto () in
      assert (Xs.backend_equal (Xs_S.state_backend state) least_capable))

let%test_unit
    "set_backend_limit is silently ignored for an backend not present on this \
     build/CPU" =
  Exn.protect ~finally:reset_backend_limit ~f:(fun () ->
      let unavailable =
        if String.(Config.architecture = "amd64") then Some Xs.Neon
        else if String.(Config.architecture = "arm64") then Some Xs.Avx2
        else None
      in
      match unavailable with
      | None -> ()
      | Some backend ->
          reset_backend_limit ();
          let before = Xs_S.state_backend (Xs_S.init ~backend:Xs.Auto ()) in
          Xs.set_backend_limit backend;
          let after = Xs_S.state_backend (Xs_S.init ~backend:Xs.Auto ()) in
          assert (Xs.backend_equal before after))

(* On a host where Plain is the only backend, most and least capable are the
   same and this can't fail, but every SIMD host exercises it. *)
let%test_unit "set_backend_limit Auto removes a previously set limit" =
  Exn.protect ~finally:reset_backend_limit ~f:(fun () ->
      let backends = Xs.available_backends () in
      let most_capable = List.hd_exn backends in
      let least_capable = List.last_exn backends in
      Xs.set_backend_limit least_capable;
      Xs.set_backend_limit Xs.Auto;
      let state = Xs_S.init ~backend:Xs.Auto () in
      assert (Xs.backend_equal (Xs_S.state_backend state) most_capable))

let%test_unit "set_backend_limit does not affect an explicit ~backend request" =
  Exn.protect ~finally:reset_backend_limit ~f:(fun () ->
      Xs.set_backend_limit Xs.Plain;
      List.iter (Xs.available_backends ()) ~f:(fun v ->
          let state = Xs_S.init_deterministic ~backend:v seed in
          assert (Xs.backend_equal (Xs_S.state_backend state) v)))

(* --- Requesting a backend that can't be used --- *)

(* The SIMD backends lib/dune compiles in for this architecture. Plain is always
   compiled and Auto always resolves, so neither is listed. *)
let compiled_simd_backends =
  match Config.architecture with
  | "amd64" | "i386" -> [ Xs.Sse2; Xs.Avx2; Xs.Avx512fvl ]
  | "arm64" | "arm" -> [ Xs.Neon ]
  | _ -> []

(* A backend that is never compiled into this build. *)
let not_compiled_backend =
  match Config.architecture with "arm64" | "arm" -> Xs.Avx2 | _ -> Xs.Neon

(* Every function that takes ~backend, each called so that the backend is the
   only thing wrong with the arguments (init_int64array raises on an empty array
   before it looks at the backend). *)
let init_calls backend =
  [
    ("State.init", fun () -> ignore (Xs_S.init ~backend ()));
    ( "State.init_deterministic",
      fun () -> ignore (Xs_S.init_deterministic ~backend seed) );
    ( "State.init_int64array",
      fun () -> ignore (Xs_S.init_int64array ~backend [| seed |]) );
    ("State.init_string", fun () -> ignore (Xs_S.init_string ~backend "seed"));
    ("self_init", fun () -> Xs.self_init ~backend ());
    ("init_deterministic", fun () -> Xs.init_deterministic ~backend seed);
    ("init_int64array", fun () -> Xs.init_int64array ~backend [| seed |]);
    ("init_string", fun () -> Xs.init_string ~backend "seed");
  ]

(* Checks that every init function raises [Invalid_argument expected] for
   [backend], and that the module-level ones leave the domain generator's
   backend as it was. If a call wrongly succeeds, the finally puts the domain
   generator back to Auto. *)
let check_init_raises backend ~expected =
  Exn.protect
    ~finally:(fun () -> Xs.self_init ())
    ~f:(fun () ->
      let domain_backend () = Xs_S.state_backend (Xs.split ()) in
      let before = domain_backend () in
      List.iter (init_calls backend) ~f:(fun (name, call) ->
          match call () with
          | () ->
              failwith
                (sprintf "%s ~backend:%s did not raise" name
                   (Xs.backend_to_string backend))
          | exception Invalid_argument msg ->
              if not (String.equal msg expected) then
                failwith
                  (sprintf "%s ~backend:%s raised %S, expected %S" name
                     (Xs.backend_to_string backend)
                     msg expected));
      assert (Xs.backend_equal (domain_backend ()) before))

let%test_unit "every init function raises for a backend that is not compiled" =
  check_init_raises not_compiled_backend
    ~expected:"Xoshiro_simd: backend is not compiled"

(* Only runs on a CPU missing one of the compiled SIMD backends, e.g. AVX512FVL
   on an i7-8550U or NEON on an ARM32 board without it; elsewhere there is
   nothing to request and the test passes trivially. *)
let%test_unit
    "every init function raises for a backend that is compiled but not \
     supported" =
  let available = Xs.available_backends () in
  List.filter compiled_simd_backends ~f:(fun backend ->
      not (List.mem available backend ~equal:Xs.backend_equal))
  |> List.iter ~f:(fun backend ->
      check_init_raises backend
        ~expected:"Xoshiro_simd: backend is not supported")

(* --- self_init --- *)

(* self_init replaces the domain generator, so these put it back to Auto
   afterward with another self_init, and the limit back to "no restriction":
   the arch tests above read the domain generator's backend. *)
let%test_unit "self_init ~backend gives the domain generator that backend" =
  Exn.protect
    ~f:(fun () ->
      List.iter (Xs.available_backends ()) ~f:(fun v ->
          Xs.self_init ~backend:v ();
          assert (Xs.backend_equal (Xs_S.state_backend (Xs.split ())) v)))
    ~finally:(fun () -> Xs.self_init ())

let%test_unit
    "self_init re-resolves Auto against set_backend_limit, reseed keeps the \
     backend" =
  Exn.protect
    ~f:(fun () ->
      let backends = Xs.available_backends () in
      let most_capable = List.hd_exn backends in
      let least_capable = List.last_exn backends in
      let domain_backend () = Xs_S.state_backend (Xs.split ()) in
      reset_backend_limit ();
      Xs.self_init ();
      assert (Xs.backend_equal (domain_backend ()) most_capable);
      Xs.set_backend_limit least_capable;
      (* The limit only matters when Auto is resolved, and reseed does not
         resolve it again: the existing state keeps its backend. *)
      Xs.reseed ();
      assert (Xs.backend_equal (domain_backend ()) most_capable);
      Xs.self_init ();
      assert (Xs.backend_equal (domain_backend ()) least_capable))
    ~finally:(fun () ->
      reset_backend_limit ();
      Xs.self_init ())
