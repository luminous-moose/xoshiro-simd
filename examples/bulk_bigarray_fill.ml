(* Bulk Bigarray fills generate directly into a destination array and are
   several times the speed of creating or filling a Bigarray an element at a
   time. They are similar in concept to numpy's random(size=N), and like that
   function, can do ranged fills. This is an uncommon feature that I wanted to
   demonstrate, and can be used for things like ML weight init, Monte Carlo
   sampling, and noise generation. *)

module Xs = Xoshiro_simd

let print_sample name (ba : (float, 'b, 'c) Bigarray.Array1.t) n =
  Printf.printf "%s (first %d of %d): %s\n" name n (Bigarray.Array1.dim ba)
    (String.concat " "
       (List.init n (fun i -> Printf.sprintf "%.6f" (Bigarray.Array1.get ba i))))

let () =
  let uniform =
    Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout 1_000_000
  in
  Xs.fill_float64_bigarray uniform;
  print_sample "float64 uniform [0, 1)" uniform 5;

  let ranged =
    Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout 1_000_000
  in
  Xs.fill_float64_range_bigarray ranged ~min:(-1.0) ~max:1.0;
  print_sample "float64 ranged [-1, 1)" ranged 5;

  let f32 =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 1_000_000
  in
  Xs.fill_float32_bigarray f32;
  print_sample "float32 uniform [0, 1)" f32 5;

  let f32_ranged =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 1_000_000
  in
  Xs.fill_float32_range_bigarray f32_ranged ~min:(-1.0) ~max:1.0;
  print_sample "float32 ranged [-1, 1)" f32_ranged 5
