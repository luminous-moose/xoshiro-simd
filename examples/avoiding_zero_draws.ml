(* Xoshiro_simd.uniform_float and Xoshiro_simd.float are able to return exactly
   0, and float_range is able to return its exact minimum. For most uses this is
   fine, but something that pipes the draw into log () or divides by it must
   have 0 excluded. Xoshiro_simd provides float_strict and float_range_strict
   for these purposes, and it can be done without rejection sampling and in bulk
   with float_range and the fill_floatXX_range_bigarray functions. *)

module Xs = Xoshiro_simd

let bound = Float.(succ @@ succ @@ succ @@ succ 0.)

let float_strict_example () =
  Xs.init_string "avoiding zero draws example";

  Printf.printf
    "float_strict 1.0, 10 draws, every one strictly between 0 and 1:\n";
  for _ = 1 to 10 do
    let x = Xs.float_strict 1.0 in
    assert (x > 0.0 && x < 1.0);
    Printf.printf "  %.17g  (log = %.6f)\n" x (log x)
  done

let float_tiny_range_example () =
  (* For demonstration, do draws at subnormal ranges so that both 0.0 and the
     bound show up, since at 1.0 a 0 draw has a probability of 2^-53. *)
  Printf.printf "\nfloat %.17g, 15 draws:\n" bound;
  for _ = 1 to 15 do
    let x = Xs.float bound in
    Printf.printf "  %.17g\n" x
  done

let float_strict_tiny_range_example () =
  (* float_strict avoids all the 0 draws of the above by rejection sampling. *)
  Printf.printf "\nfloat_strict %.17g, 15 draws:\n" bound;
  for _ = 1 to 15 do
    let x = Xs.float_strict bound in
    assert (x > 0.0 && x <= bound);
    Printf.printf "  %.17g\n" x
  done

let float_range_example () =
  (* For draws strictly between bounds with no rejection sampling, we use
     float_range with bounds chosen to be the minimum and maximum values that we
     will accept. There is also float_range_strict with rejection sampling, not
     demonstrated here. *)
  let min = Float.succ 0. in
  let bound_pred = Float.pred bound in
  Printf.printf "\nfloat_range ~min:%.17g ~max:%.17g, 15 draws:\n" min
    bound_pred;
  for _ = 1 to 15 do
    let x = Xs.float_range ~min ~max:bound_pred in
    assert (x > 0.0 && x < bound);
    Printf.printf "  %.17g\n" x
  done

let bulk_fill_range_example () =
  (* For bulk fills, this works too, and is the only way to avoid zero draws
     there. *)
  let min = Float.succ 0. in
  let bound_pred = Float.pred bound in
  Printf.printf
    "\nfill_float64_range_bigarray ~min:%.17g ~max:%.17g, 15 draws:\n" min
    bound_pred;
  let ranged = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout 15 in
  Xs.fill_float64_range_bigarray ranged ~min ~max:bound_pred;
  for i = 0 to 14 do
    let x = Bigarray.Array1.get ranged i in
    assert (x > 0.0 && x < bound);
    Printf.printf "  %.17g\n" x
  done

let invalid_arg_example () =
  (* Demonstrate where these functions will raise: float_strict raises if passed
     0 as a bound, and float_range_strict raises if there are no numbers
     strictly between min and max. *)
  (try
     let x = Xs.float_strict 0.0 in
     Printf.printf "\nfloat_strict 0.0 = %.17g (did not raise)\n" x
   with Invalid_argument msg ->
     Printf.printf "\nfloat_strict 0.0 raised: %s\n" msg);
  try
    let x = Xs.float_range_strict ~min:1.0 ~max:(Float.succ 1.0) in
    Printf.printf
      "float_range_strict of adjacent floats = %.17g (did not raise)\n" x
  with Invalid_argument msg ->
    Printf.printf "float_range_strict of adjacent floats raised: %s\n" msg

let () =
  float_strict_example ();
  float_tiny_range_example ();
  float_strict_tiny_range_example ();
  float_range_example ();
  bulk_fill_range_example ();
  invalid_arg_example ()
