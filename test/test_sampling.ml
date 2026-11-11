open Base
open! Stdio
open! Stdlib.Printf
module List = List_extra
module S = Xoshiro_simd.State
module Xs_S = S

let seed = 1042L

let printbuckets buckets sum =
  for i = 0 to Array.length buckets - 1 do
    printf "%d\n" buckets.(i)
  done;
  let sum' = Array.fold buckets ~init:0 ~f:(fun acc x -> acc + x) in
  assert (sum = sum')

(* --- choice --- *)

let%test "choice raises on empty array" =
  try
    let state = S.init () in
    ignore (S.choice state [||]);
    false
  with Invalid_argument _ -> true

let%test_unit "choice single-element array always returns that element" =
  let state = S.init_deterministic seed in
  for _ = 1 to 100 do
    assert (String.equal "only" (S.choice state [| "only" |]))
  done

let%expect_test "choice" =
  let state = S.init_deterministic seed in
  let arr = [| "alpha"; "beta"; "gamma"; "delta"; "epsilon" |] in
  for _ = 1 to 30 do
    printf "%s\n" (S.choice state arr)
  done;
  [%expect
    {|
    alpha
    delta
    alpha
    epsilon
    alpha
    beta
    alpha
    gamma
    epsilon
    beta
    epsilon
    alpha
    epsilon
    epsilon
    gamma
    alpha
    gamma
    epsilon
    gamma
    gamma
    alpha
    epsilon
    alpha
    epsilon
    alpha
    epsilon
    gamma
    beta
    gamma
    delta
    |}]

(* --- choice_opt --- *)

let%test_unit "choice_opt returns None on empty array" =
  let state = S.init () in
  assert (Option.is_none (S.choice_opt state [||]))

let%test_unit
    "choice_opt single-element array always returns Some of that element" =
  let state = S.init_deterministic seed in
  for _ = 1 to 100 do
    assert (
      Option.equal String.equal (Some "only") (S.choice_opt state [| "only" |]))
  done

let%expect_test "choice_opt" =
  let state = S.init_deterministic seed in
  let arr = [| "alpha"; "beta"; "gamma"; "delta"; "epsilon" |] in
  for _ = 1 to 10 do
    match S.choice_opt state arr with
    | None -> printf "None\n"
    | Some s -> printf "Some %s\n" s
  done;
  printf "empty: %b\n" (Option.is_none (S.choice_opt state [||]));
  [%expect
    {|
    Some alpha
    Some delta
    Some alpha
    Some epsilon
    Some alpha
    Some beta
    Some alpha
    Some gamma
    Some epsilon
    Some beta
    empty: true
    |}]

(* --- choice histogram --- *)

let%expect_test "choice hist" =
  let state = Xs_S.init_deterministic seed in
  let primes = [| 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31 |] in
  let buckets = Array.init 32 ~f:(fun _ -> 0) in
  for _ = 1 to 10000 do
    let i = Xs_S.choice state primes in
    buckets.(i) <- buckets.(i) + 1
  done;
  printbuckets buckets 10000;
  [%expect
    {|
    0
    0
    908
    921
    0
    877
    0
    941
    0
    0
    0
    897
    0
    954
    0
    0
    0
    907
    0
    906
    0
    0
    0
    901
    0
    0
    0
    0
    0
    905
    0
    883
    |}]

(* --- permutation --- *)

let%test_unit "permutation 0 returns empty array" =
  let state = S.init_deterministic seed in
  assert (Array.is_empty (S.permutation state 0))

let%test_unit "permutation 1 returns [|0|]" =
  let state = S.init_deterministic seed in
  assert (Array.equal Int.equal (S.permutation state 1) [| 0 |])

let%test_unit "permutation is a permutation of 0..n-1" =
  let state = S.init_deterministic seed in
  let n = 30 in
  for _ = 1 to 20 do
    let arr = S.permutation state n in
    let sorted = Array.copy arr in
    Array.sort sorted ~compare:Int.compare;
    assert (Array.equal Int.equal sorted (Array.init n ~f:(fun i -> i)))
  done

(* The size check has to come before the array is built: checking afterwards
   (as shuffle does) still raises, but only after allocating and filling an
   8 GiB array, or running out of memory first. So this checks the allocation,
   not just the exception. 64-bit only: on 32-bit, 2^30 doesn't fit in an int.
   If this regresses, the test itself will try that 8 GiB allocation. *)
let%test_unit
    ("permutation rejects n > 2^30 - 1 before allocating"
     [@tags "64-bits-only"]) =
  let state = S.init_deterministic seed in
  let too_large = 1 lsl 30 in
  let before = Stdlib.Gc.allocated_bytes () in
  let raised =
    try
      ignore (S.permutation state too_large);
      false
    with Invalid_argument _ -> true
  in
  let allocated = Stdlib.Gc.allocated_bytes () -. before in
  assert raised;
  assert (Float.(allocated < 1e6))

let%expect_test "permutation" =
  let state = S.init_deterministic seed in
  for _ = 1 to 10 do
    let arr = S.permutation state 10 in
    Array.iter arr ~f:(printf "%d ");
    printf "\n"
  done;
  [%expect
    {|
    0 7 3 9 4 6 5 8 2 1
    3 8 2 9 1 7 6 4 0 5
    5 4 2 8 1 9 6 0 7 3
    2 5 7 4 1 9 0 3 8 6
    1 8 4 6 0 9 2 7 5 3
    8 4 9 6 7 1 0 5 2 3
    4 8 6 9 7 0 5 1 3 2
    4 1 0 6 3 5 9 7 8 2
    0 9 8 3 6 4 5 7 1 2
    2 0 6 9 5 8 1 7 3 4
    |}]

(* --- reservoir_sample --- *)

let%test "reservoir_sample raises on negative k" =
  try
    let state = S.init () in
    ignore (S.reservoir_sample state [| 1; 2; 3 |] (-1));
    false
  with Invalid_argument _ -> true

let%test "reservoir_sample raises when k exceeds array length" =
  try
    let state = S.init () in
    ignore (S.reservoir_sample state [| 1; 2; 3 |] 4);
    false
  with Invalid_argument _ -> true

let%test_unit "reservoir_sample k=0 returns empty array" =
  let state = S.init_deterministic seed in
  assert (Array.is_empty (S.reservoir_sample state [| 1; 2; 3 |] 0))

let%test_unit "reservoir_sample k=n contains all input elements" =
  let state = S.init_deterministic seed in
  let input = [| 10; 20; 30; 40; 50 |] in
  for _ = 1 to 20 do
    let out = S.reservoir_sample state input 5 in
    let sorted_in = Array.copy input in
    let sorted_out = Array.copy out in
    Array.sort sorted_in ~compare:Int.compare;
    Array.sort sorted_out ~compare:Int.compare;
    assert (Array.equal Int.equal sorted_in sorted_out)
  done

let%test_unit "reservoir_sample output is a subset with no duplicates" =
  let state = S.init_deterministic seed in
  let input = Array.init 20 ~f:(fun i -> i * 10) in
  for _ = 1 to 50 do
    let k = 7 in
    let out = S.reservoir_sample state input k in
    assert (Array.length out = k);
    let input_set = Hash_set.of_list (module Int) (Array.to_list input) in
    assert (Array.for_all out ~f:(Hash_set.mem input_set));
    let out_set = Hash_set.of_list (module Int) (Array.to_list out) in
    assert (Hash_set.length out_set = k)
  done

let%expect_test "reservoir_sample" =
  let state = S.init_deterministic seed in
  let input = [| "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" |] in
  for _ = 1 to 5 do
    let out = S.reservoir_sample state input 4 in
    Array.iter out ~f:(printf "%s ");
    printf "\n"
  done;
  [%expect
    {|
    a f c h
    a d c f
    g d a b
    h g e d
    e h f c
    |}]

(* --- weighted_bool --- *)

let%test_unit "weighted_bool p=0.0 always returns false" =
  let state = S.init_deterministic seed in
  for _ = 1 to 1000 do
    assert (not (S.weighted_bool state 0.0))
  done

let%test_unit "weighted_bool p=1.0 always returns true" =
  let state = S.init_deterministic seed in
  for _ = 1 to 1000 do
    assert (S.weighted_bool state 1.0)
  done

let%expect_test "weighted_bool p=0.25" =
  let state = S.init_deterministic seed in
  for _ = 1 to 30 do
    printf "%b\n" (S.weighted_bool state 0.25)
  done;
  [%expect
    {|
    true
    true
    false
    true
    false
    false
    true
    false
    true
    true
    false
    false
    false
    false
    false
    true
    false
    false
    false
    true
    false
    false
    false
    false
    false
    false
    false
    false
    false
    true
    |}]

let%expect_test "weighted_bool p=0.5" =
  let state = S.init_deterministic seed in
  for _ = 1 to 30 do
    printf "%b\n" (S.weighted_bool state 0.5)
  done;
  [%expect
    {|
    true
    true
    false
    true
    true
    false
    true
    false
    true
    true
    false
    false
    false
    false
    true
    true
    false
    false
    false
    true
    true
    true
    false
    true
    false
    false
    false
    false
    true
    true
    |}]

(* --- weighted_choice_linear_float --- *)

let%test "weighted_choice_linear_float raises on empty array" =
  try
    ignore (S.weighted_choice_linear_float (S.init ()) [||]);
    false
  with Invalid_argument _ -> true

let%test "weighted_choice_linear_float raises on negative weight" =
  try
    ignore (S.weighted_choice_linear_float (S.init ()) [| ("a", -1.) |]);
    false
  with Invalid_argument _ -> true

let%test "weighted_choice_linear_float raises on NaN weight" =
  try
    ignore (S.weighted_choice_linear_float (S.init ()) [| ("a", Float.nan) |]);
    false
  with Invalid_argument _ -> true

let%test "weighted_choice_linear_float raises on infinite weight" =
  try
    ignore
      (S.weighted_choice_linear_float (S.init ()) [| ("a", Float.infinity) |]);
    false
  with Invalid_argument _ -> true

let%test "weighted_choice_linear_float raises when all weights are zero" =
  try
    ignore
      (S.weighted_choice_linear_float (S.init ()) [| ("a", 0.); ("b", 0.) |]);
    false
  with Invalid_argument _ -> true

let%test_unit "weighted_choice_linear_float single element always returned" =
  let state = S.init_deterministic seed in
  for _ = 1 to 100 do
    assert (
      String.equal "only"
        (S.weighted_choice_linear_float state [| ("only", 42.) |]))
  done

let%test_unit "weighted_choice_linear_float never selects zero-weight elements"
    =
  let state = S.init_deterministic seed in
  let arr = [| ("never", 0.); ("always", 1.); ("never2", 0.) |] in
  for _ = 1 to 1000 do
    assert (String.equal "always" (S.weighted_choice_linear_float state arr))
  done

let%expect_test "weighted_choice_linear_float" =
  let state = S.init_deterministic seed in
  let arr = [| ("alpha", 5.); ("beta", 3.); ("gamma", 1.); ("delta", 1.) |] in
  for _ = 1 to 30 do
    printf "%s\n" (S.weighted_choice_linear_float state arr)
  done;
  [%expect
    {|
    alpha
    alpha
    beta
    alpha
    alpha
    beta
    alpha
    delta
    alpha
    alpha
    beta
    beta
    delta
    beta
    alpha
    alpha
    gamma
    beta
    gamma
    alpha
    alpha
    alpha
    beta
    alpha
    beta
    beta
    gamma
    beta
    alpha
    alpha
    |}]

let%expect_test "weighted_choice_linear_float hist" =
  let state = Xs_S.init_deterministic seed in
  let arr = [| ("alpha", 5.); ("beta", 3.); ("gamma", 1.); ("delta", 1.) |] in
  let bucket_of = function
    | "alpha" -> 0
    | "beta" -> 1
    | "gamma" -> 2
    | "delta" -> 3
    | _ -> assert false
  in
  let buckets = Array.init 4 ~f:(fun _ -> 0) in
  for _ = 1 to 10000 do
    let s = S.weighted_choice_linear_float state arr in
    let i = bucket_of s in
    buckets.(i) <- buckets.(i) + 1
  done;
  printbuckets buckets 10000;
  [%expect {|
    4975
    3004
    1027
    994
    |}]

(* --- weighted_choice_float --- *)

let%test "weighted_table_float raises on empty array" =
  try
    ignore (S.weighted_table_float [||]);
    false
  with Invalid_argument _ -> true

let%test "weighted_table_float raises on negative weight" =
  try
    ignore (S.weighted_table_float [| ("a", -1.) |]);
    false
  with Invalid_argument _ -> true

let%test "weighted_table_float raises on NaN weight" =
  try
    ignore (S.weighted_table_float [| ("a", Float.nan) |]);
    false
  with Invalid_argument _ -> true

let%test "weighted_table_float raises on infinite weight" =
  try
    ignore (S.weighted_table_float [| ("a", Float.infinity) |]);
    false
  with Invalid_argument _ -> true

let%test "weighted_table_float raises when all weights are zero" =
  try
    ignore (S.weighted_table_float [| ("a", 0.); ("b", 0.) |]);
    false
  with Invalid_argument _ -> true

(* Finite weights whose sum overflows to infinity. Before the check, the total
   was infinity (or NaN once a third weight went through the Kahan
   compensation), and every draw returned the same element. *)
let overflowing_float_weights =
  [| ("a", Float.max_finite_value); ("b", Float.max_finite_value) |]

let%test "weighted_choice_linear_float raises when the weight sum overflows" =
  try
    ignore
      (S.weighted_choice_linear_float
         (S.init_deterministic seed)
         overflowing_float_weights);
    false
  with Invalid_argument _ -> true

let%test "weighted_table_float raises when the weight sum overflows" =
  try
    ignore (S.weighted_table_float overflowing_float_weights);
    false
  with Invalid_argument _ -> true

(* The largest weights whose sum is still finite are accepted, and both
   elements are drawn. *)
let%test_unit "weighted_choice_float accepts a sum of exactly max_finite_value"
    =
  let half = Float.max_finite_value /. 2. in
  let arr = [| ("a", half); ("b", half) |] in
  let state = S.init_deterministic seed in
  let tbl = S.weighted_table_float arr in
  let seen_a = ref false and seen_b = ref false in
  for _ = 1 to 100 do
    (match S.weighted_choice_linear_float state arr with
    | "a" -> seen_a := true
    | _ -> seen_b := true);
    match S.weighted_choice_float state tbl with
    | "a" -> seen_a := true
    | _ -> seen_b := true
  done;
  assert (!seen_a && !seen_b)

let%test_unit "weighted_choice_float single element always returned" =
  let state = S.init_deterministic seed in
  let tbl = S.weighted_table_float [| ("only", 42.) |] in
  for _ = 1 to 100 do
    assert (String.equal "only" (S.weighted_choice_float state tbl))
  done

let%test_unit "weighted_choice_float never selects zero-weight elements" =
  let state = S.init_deterministic seed in
  let tbl =
    S.weighted_table_float [| ("never", 0.); ("always", 1.); ("never2", 0.) |]
  in
  for _ = 1 to 1000 do
    assert (String.equal "always" (S.weighted_choice_float state tbl))
  done

(* weighted_table_float sorts its input by weight ascending (for numerical
   stability -- see rand_impl.ml) before summing, while
   weighted_choice_linear_float always sums arr in whatever order it's given
   and never sorts. Floating-point addition isn't associative, so the two
   only compute the same partition of [0, total) -- and are therefore only
   guaranteed to pick the same item for the same roll -- when arr already
   arrives pre-sorted by weight, which is what this test exercises. *)
let%test_unit
    "weighted_choice_linear_float and binsearch give identical choices when \
     input is pre-sorted by weight" =
  let state1 = S.init_deterministic seed in
  let state2 = S.init_deterministic seed in
  let arr = [| ("gamma", 1.); ("delta", 1.); ("beta", 3.); ("alpha", 5.) |] in
  let tbl = S.weighted_table_float arr in
  for _ = 1 to 200 do
    let a = S.weighted_choice_linear_float state1 arr in
    let b = S.weighted_choice_float state2 tbl in
    assert (String.equal a b)
  done

let%expect_test "weighted_choice" =
  let state = S.init_deterministic seed in
  let tbl =
    S.weighted_table_float
      [| ("alpha", 5.); ("beta", 3.); ("gamma", 1.); ("delta", 1.) |]
  in
  for _ = 1 to 30 do
    printf "%s\n" (S.weighted_choice_float state tbl)
  done;
  [%expect
    {|
    gamma
    gamma
    alpha
    gamma
    beta
    alpha
    beta
    alpha
    gamma
    delta
    alpha
    alpha
    alpha
    alpha
    beta
    beta
    alpha
    alpha
    alpha
    delta
    beta
    beta
    alpha
    beta
    alpha
    alpha
    alpha
    alpha
    beta
    gamma
    |}]

let%expect_test "weighted_choice_float hist" =
  let state = Xs_S.init_deterministic seed in
  let tbl =
    S.weighted_table_float
      [| ("alpha", 5.); ("beta", 3.); ("gamma", 1.); ("delta", 1.) |]
  in
  let bucket_of = function
    | "alpha" -> 0
    | "beta" -> 1
    | "gamma" -> 2
    | "delta" -> 3
    | _ -> assert false
  in
  let buckets = Array.init 4 ~f:(fun _ -> 0) in
  for _ = 1 to 10000 do
    let s = S.weighted_choice_float state tbl in
    let i = bucket_of s in
    buckets.(i) <- buckets.(i) + 1
  done;
  printbuckets buckets 10000;
  [%expect {|
    5025
    2989
    1005
    981
    |}]

let kahan_add x sum c =
  let y = x -. c in
  let t = sum +. y in
  let c = t -. sum -. y in
  let sum = t in
  (sum, c)

let kahan_sum list =
  let rec loop sum c = function
    | [] -> sum
    | hd :: tl ->
        let sum, c = kahan_add hd sum c in
        loop sum c tl
  in
  loop 0. 0. list

(* weighted_choice_linear_float sums arr in whatever order it's given and
   never sorts, so this is unaffected by weighted_table_float's ascending
   sort-by-weight below -- its expected output is unchanged from before that
   sort was introduced. *)
let%expect_test "weighted_choice_linear_float with fractional values" =
  let pi = Float.pi in
  let e = 2.718281828459045 in
  let sqrt2 = Float.sqrt 2. in
  let delta =
    let remainder = 10. -. kahan_sum [ sqrt2; e; pi ] in
    remainder /. 100.
  in
  let table_list =
    [ ("pi", pi); ("e", e); ("sqrt2", sqrt2) ]
    @ List.init 100 ~f:(fun i -> (sprintf "d%02d" i, delta))
  in
  let state = S.init_deterministic seed in
  let arr = Array.of_list table_list |> S.shuffled state in
  let naive_sum =
    Array.to_list arr |> List.map ~f:snd |> List.fold ~init:0. ~f:Float.add
  in
  let kahan_summed = Array.to_list arr |> List.map ~f:snd |> kahan_sum in
  printf "naive sum: %0.15f, kahan sum: %0.15f\n" naive_sum kahan_summed;
  Array.iter arr ~f:(fun (s, p) -> printf "%s, %f\n" s p);
  print_endline "";
  for _ = 1 to 60 do
    printf "%s\n" (S.weighted_choice_linear_float state arr)
  done;
  [%expect
    {|
    naive sum: 10.000000000000011, kahan sum: 10.000000000000000
    d02, 0.027259
    d67, 0.027259
    d12, 0.027259
    d90, 0.027259
    d08, 0.027259
    d38, 0.027259
    d05, 0.027259
    d44, 0.027259
    d87, 0.027259
    d40, 0.027259
    d83, 0.027259
    d19, 0.027259
    d93, 0.027259
    d84, 0.027259
    d55, 0.027259
    d01, 0.027259
    d58, 0.027259
    d89, 0.027259
    d60, 0.027259
    d52, 0.027259
    sqrt2, 1.414214
    d03, 0.027259
    d31, 0.027259
    d10, 0.027259
    d22, 0.027259
    d86, 0.027259
    d65, 0.027259
    d42, 0.027259
    d62, 0.027259
    d76, 0.027259
    d45, 0.027259
    d41, 0.027259
    d97, 0.027259
    d64, 0.027259
    d81, 0.027259
    d48, 0.027259
    d27, 0.027259
    d21, 0.027259
    d56, 0.027259
    d66, 0.027259
    d88, 0.027259
    d91, 0.027259
    d17, 0.027259
    d11, 0.027259
    d96, 0.027259
    pi, 3.141593
    d23, 0.027259
    d29, 0.027259
    d71, 0.027259
    d77, 0.027259
    d33, 0.027259
    d20, 0.027259
    d53, 0.027259
    d95, 0.027259
    d75, 0.027259
    d24, 0.027259
    d39, 0.027259
    d99, 0.027259
    d80, 0.027259
    d51, 0.027259
    d72, 0.027259
    d79, 0.027259
    d49, 0.027259
    d78, 0.027259
    d25, 0.027259
    d35, 0.027259
    d59, 0.027259
    d15, 0.027259
    d36, 0.027259
    d92, 0.027259
    e, 2.718282
    d57, 0.027259
    d47, 0.027259
    d98, 0.027259
    d50, 0.027259
    d73, 0.027259
    d07, 0.027259
    d13, 0.027259
    d46, 0.027259
    d06, 0.027259
    d14, 0.027259
    d82, 0.027259
    d63, 0.027259
    d43, 0.027259
    d04, 0.027259
    d85, 0.027259
    d94, 0.027259
    d18, 0.027259
    d37, 0.027259
    d54, 0.027259
    d28, 0.027259
    d09, 0.027259
    d68, 0.027259
    d74, 0.027259
    d70, 0.027259
    d30, 0.027259
    d34, 0.027259
    d00, 0.027259
    d26, 0.027259
    d61, 0.027259
    d69, 0.027259
    d16, 0.027259
    d32, 0.027259

    e
    pi
    sqrt2
    e
    d26
    e
    e
    d28
    d26
    d49
    d18
    pi
    sqrt2
    e
    d38
    e
    e
    e
    e
    d61
    e
    e
    pi
    d54
    d17
    pi
    e
    pi
    e
    pi
    e
    e
    pi
    d24
    d10
    e
    e
    sqrt2
    pi
    sqrt2
    d66
    e
    e
    d84
    sqrt2
    d59
    d05
    sqrt2
    sqrt2
    sqrt2
    d79
    d86
    d04
    e
    d32
    e
    pi
    e
    sqrt2
    e
    |}]

(* Same table_array as above, but weighted_table_float sorts it by weight
   ascending before summing -- see "weighted_choice_linear_float and
   binsearch give identical choices when input is pre-sorted by weight" for
   why that means this draw sequence is expected to diverge from the linear
   one above, rather than match it. *)
let%expect_test "weighted_choice_float with fractional values" =
  let pi = Float.pi in
  let e = 2.718281828459045 in
  let sqrt2 = Float.sqrt 2. in
  let delta =
    let remainder = 10. -. kahan_sum [ sqrt2; e; pi ] in
    remainder /. 100.
  in
  let table_list =
    [ ("pi", pi); ("e", e); ("sqrt2", sqrt2) ]
    @ List.init 100 ~f:(fun i -> (sprintf "d%02d" i, delta))
  in
  let state = S.init_deterministic seed in
  let arr = Array.of_list table_list |> S.shuffled state in
  let tbl = S.weighted_table_float arr in
  for _ = 1 to 60 do
    printf "%s\n" (S.weighted_choice_float state tbl)
  done;
  [%expect
    {|
    pi
    e
    d31
    pi
    pi
    pi
    pi
    pi
    pi
    e
    pi
    sqrt2
    d11
    pi
    d38
    pi
    pi
    pi
    pi
    pi
    e
    pi
    e
    pi
    d34
    e
    pi
    sqrt2
    pi
    e
    pi
    pi
    sqrt2
    e
    d07
    pi
    pi
    d53
    sqrt2
    d86
    d74
    pi
    pi
    d84
    d49
    e
    d05
    d42
    d36
    d65
    e
    d06
    pi
    pi
    pi
    pi
    e
    pi
    d23
    pi
    |}]

let%expect_test "weighted_choice_float fractional values histogram" =
  let pi = Float.pi in
  let e = 2.718281828459045 in
  let sqrt2 = Float.sqrt 2. in
  let delta =
    let remainder = 10. -. kahan_sum [ sqrt2; e; pi ] in
    remainder /. 100.
  in
  let table_list =
    [ ("pi", pi); ("e", e); ("sqrt2", sqrt2) ]
    @ List.init 100 ~f:(fun i -> (sprintf "d%02d" i, delta))
  in
  let state = S.init_deterministic seed in
  let table_array = Array.of_list table_list |> S.shuffled state in
  let tbl = S.weighted_table_float table_array in
  let bucket_of = function
    | "pi" -> 0
    | "e" -> 1
    | "sqrt2" -> 2
    | d -> begin
        let digits = String.sub d ~pos:1 ~len:2 in
        let i = Int.of_string digits in
        i + 3
      end
  in
  let buckets = Array.init (Array.length table_array) ~f:(fun _ -> 0) in
  for _ = 1 to 10000 do
    let s = S.weighted_choice_float state tbl in
    let i = bucket_of s in
    buckets.(i) <- buckets.(i) + 1
  done;
  printbuckets buckets 10000;
  [%expect
    {|
    3185
    2651
    1453
    32
    29
    38
    23
    25
    30
    19
    23
    23
    19
    34
    28
    23
    39
    29
    33
    25
    16
    24
    24
    21
    30
    22
    30
    28
    22
    28
    27
    33
    34
    26
    28
    33
    23
    32
    29
    30
    23
    34
    31
    33
    23
    19
    18
    19
    21
    17
    22
    29
    23
    31
    30
    27
    22
    31
    29
    19
    21
    21
    31
    30
    42
    25
    23
    29
    31
    25
    29
    22
    39
    20
    33
    25
    32
    37
    21
    29
    25
    28
    42
    32
    18
    21
    22
    31
    23
    32
    34
    25
    37
    26
    24
    21
    22
    31
    29
    25
    28
    30
    27
    |}]

(* --- weighted_choice_linear_int --- *)

let%test "weighted_choice_linear_int raises on empty array" =
  try
    ignore (S.weighted_choice_linear_int (S.init ()) [||]);
    false
  with Invalid_argument _ -> true

let%test "weighted_choice_linear_int raises on negative weight" =
  try
    ignore (S.weighted_choice_linear_int (S.init ()) [| ("a", -1) |]);
    false
  with Invalid_argument _ -> true

let%test "weighted_choice_linear_int raises when all weights are zero" =
  try
    ignore (S.weighted_choice_linear_int (S.init ()) [| ("a", 0); ("b", 0) |]);
    false
  with Invalid_argument _ -> true

let%test_unit "weighted_choice_linear_int single element always returned" =
  let state = S.init_deterministic seed in
  for _ = 1 to 100 do
    assert (
      String.equal "only"
        (S.weighted_choice_linear_int state [| ("only", 42) |]))
  done

let%test_unit "weighted_choice_linear_int never selects zero-weight elements" =
  let state = S.init_deterministic seed in
  let arr = [| ("never", 0); ("always", 1); ("never2", 0) |] in
  for _ = 1 to 1000 do
    assert (String.equal "always" (S.weighted_choice_linear_int state arr))
  done

let%expect_test "weighted_choice_linear_int" =
  let state = S.init_deterministic seed in
  let arr = [| ("alpha", 5); ("beta", 3); ("gamma", 1); ("delta", 1) |] in
  for _ = 1 to 30 do
    printf "%s\n" (S.weighted_choice_linear_int state arr)
  done;
  [%expect
    {|
    alpha
    beta
    alpha
    delta
    alpha
    alpha
    alpha
    alpha
    gamma
    alpha
    gamma
    alpha
    delta
    gamma
    alpha
    alpha
    beta
    gamma
    beta
    alpha
    alpha
    gamma
    alpha
    gamma
    alpha
    gamma
    beta
    alpha
    beta
    beta
    |}]

let%expect_test "weighted_choice_linear_int hist" =
  let state = Xs_S.init_deterministic seed in
  let arr = [| ("alpha", 5); ("beta", 3); ("gamma", 1); ("delta", 1) |] in
  let bucket_of = function
    | "alpha" -> 0
    | "beta" -> 1
    | "gamma" -> 2
    | "delta" -> 3
    | _ -> assert false
  in
  let buckets = Array.init 4 ~f:(fun _ -> 0) in
  for _ = 1 to 10000 do
    let s = S.weighted_choice_linear_int state arr in
    let i = bucket_of s in
    buckets.(i) <- buckets.(i) + 1
  done;
  printbuckets buckets 10000;
  [%expect {|
    5037
    2991
    994
    978
    |}]

(* --- weighted_choice_int --- *)

let%test "weighted_table_int raises on empty array" =
  try
    ignore (S.weighted_table_int [||]);
    false
  with Invalid_argument _ -> true

let%test "weighted_table_int raises on negative weight" =
  try
    ignore (S.weighted_table_int [| ("a", -1) |]);
    false
  with Invalid_argument _ -> true

let%test "weighted_table_int raises when all weights are zero" =
  try
    ignore (S.weighted_table_int [| ("a", 0); ("b", 0) |]);
    false
  with Invalid_argument _ -> true

(* Weights whose sum overflows a native int. max_int + max_int + 3 wraps to
   exactly 1 on both word sizes, so the total looks small and valid, and the
   prefix sums ([max_int; -2; 1]) are no longer increasing. Both functions must
   reject these weights rather than draw from a corrupted table. *)
let overflowing_weights =
  [| ("a", Int.max_value); ("b", Int.max_value); ("c", 3) |]

let%test "weighted_table_int raises when the weight sum overflows" =
  try
    ignore (S.weighted_table_int overflowing_weights);
    false
  with Invalid_argument _ -> true

let%test "weighted_choice_linear_int raises when the weight sum overflows" =
  try
    ignore
      (S.weighted_choice_linear_int
         (S.init_deterministic seed)
         overflowing_weights);
    false
  with Invalid_argument _ -> true

let%test_unit "weighted_choice_int single element always returned" =
  let state = S.init_deterministic seed in
  let tbl = S.weighted_table_int [| ("only", 42) |] in
  for _ = 1 to 100 do
    assert (String.equal "only" (S.weighted_choice_int state tbl))
  done

let%test_unit "weighted_choice_int never selects zero-weight elements" =
  let state = S.init_deterministic seed in
  let tbl =
    S.weighted_table_int [| ("never", 0); ("always", 1); ("never2", 0) |]
  in
  for _ = 1 to 1000 do
    assert (String.equal "always" (S.weighted_choice_int state tbl))
  done

let%test_unit "weighted_choice_linear_int and binsearch give identical choices"
    =
  let state1 = S.init_deterministic seed in
  let state2 = S.init_deterministic seed in
  let arr = [| ("alpha", 5); ("beta", 3); ("gamma", 1); ("delta", 1) |] in
  let tbl = S.weighted_table_int arr in
  for _ = 1 to 200 do
    let a = S.weighted_choice_linear_int state1 arr in
    let b = S.weighted_choice_int state2 tbl in
    assert (String.equal a b)
  done

let%expect_test "weighted_choice_int" =
  let state = S.init_deterministic seed in
  let tbl =
    S.weighted_table_int
      [| ("alpha", 5); ("beta", 3); ("gamma", 1); ("delta", 1) |]
  in
  for _ = 1 to 30 do
    printf "%s\n" (S.weighted_choice_int state tbl)
  done;
  [%expect
    {|
    alpha
    beta
    alpha
    delta
    alpha
    alpha
    alpha
    alpha
    gamma
    alpha
    gamma
    alpha
    delta
    gamma
    alpha
    alpha
    beta
    gamma
    beta
    alpha
    alpha
    gamma
    alpha
    gamma
    alpha
    gamma
    beta
    alpha
    beta
    beta
    |}]

let%expect_test "weighted_choice_int hist" =
  let state = Xs_S.init_deterministic seed in
  let tbl =
    S.weighted_table_int
      [| ("alpha", 5); ("beta", 3); ("gamma", 1); ("delta", 1) |]
  in
  let bucket_of = function
    | "alpha" -> 0
    | "beta" -> 1
    | "gamma" -> 2
    | "delta" -> 3
    | _ -> assert false
  in
  let buckets = Array.init 4 ~f:(fun _ -> 0) in
  for _ = 1 to 10000 do
    let s = S.weighted_choice_int state tbl in
    let i = bucket_of s in
    buckets.(i) <- buckets.(i) + 1
  done;
  printbuckets buckets 10000;
  [%expect {|
    5037
    2991
    994
    978
    |}]

(* --- weighted_choice for_testing internals (roll-based) ---

   These drive the binary-search (for_testing_weighted_choice_float) and
   linear-scan (for_testing_weighted_choice_linear_float) selection logic
   directly with a chosen roll [r], instead of drawing [r] from a state --
   which is what makes it possible to exercise pathological [r] values (in
   particular [r] landing on or past the total weight, which
   [uniform_float *. total] can round up to even though a real draw of
   [uniform_float] is always < 1.0) without reverse-engineering RNG output. *)

let%test_unit "for_testing_weighted_choice_float: r=0 returns first item" =
  let tbl = S.weighted_table_float [| ("a", 1.); ("b", 1.); ("c", 1.) |] in
  assert (String.equal "a" (S.for_testing_weighted_choice_float tbl 0.))

let%test_unit
    "for_testing_weighted_choice_float: boundary rolls resolve to the \
     half-open [prev, sum) range they fall into" =
  let tbl = S.weighted_table_float [| ("a", 1.); ("b", 1.); ("c", 1.) |] in
  (* Cumulative sums: a:1., b:2., c:3. *)
  assert (
    String.equal "a"
      (S.for_testing_weighted_choice_float tbl (Stdlib.Float.pred 1.0)));
  assert (String.equal "b" (S.for_testing_weighted_choice_float tbl 1.0));
  assert (
    String.equal "b"
      (S.for_testing_weighted_choice_float tbl (Stdlib.Float.pred 2.0)));
  assert (String.equal "c" (S.for_testing_weighted_choice_float tbl 2.0))

let%test_unit
    "for_testing_weighted_choice_float: a middle zero-weight item is never \
     selected, even exactly at its (empty) boundary" =
  let tbl = S.weighted_table_float [| ("a", 1.); ("never", 0.); ("b", 1.) |] in
  (* Cumulative sums: a:1., never:1., b:2. *)
  assert (
    String.equal "a"
      (S.for_testing_weighted_choice_float tbl (Stdlib.Float.pred 1.0)));
  assert (String.equal "b" (S.for_testing_weighted_choice_float tbl 1.0))

let%test_unit
    "for_testing_weighted_choice_float: a leading zero-weight item is never \
     selected, even at r=0" =
  let tbl = S.weighted_table_float [| ("never", 0.); ("a", 1.); ("b", 1.) |] in
  (* Cumulative sums: never:0., a:1., b:2. *)
  assert (String.equal "a" (S.for_testing_weighted_choice_float tbl 0.));
  assert (
    String.equal "a"
      (S.for_testing_weighted_choice_float tbl (Stdlib.Float.pred 1.0)));
  assert (String.equal "b" (S.for_testing_weighted_choice_float tbl 1.0))

let%test_unit
    "for_testing_weighted_choice_float: a roll landing exactly on the total \
     must not select a trailing zero-weight item" =
  let tbl = S.weighted_table_float [| ("a", 1.); ("b", 1.); ("c", 0.) |] in
  assert (String.equal "b" (S.for_testing_weighted_choice_float tbl 2.0))

let%test_unit
    "for_testing_weighted_choice_float: multiple trailing zero-weight items" =
  let tbl = S.weighted_table_float [| ("a", 1.); ("b", 0.); ("c", 0.) |] in
  assert (String.equal "a" (S.for_testing_weighted_choice_float tbl 1.0))

(* weighted_choice_linear_internal takes the already-filtered, cumulative-sum
   list weighted_choice_linear_float_filter produces, rather than a raw array --
   this joins the two exposed for_testing_ pieces back together so the tests
   below can drive the whole thing from a raw array and a chosen roll. *)
let for_testing_weighted_choice_linear_float arr r =
  let _total, filtered =
    S.for_testing_weighted_choice_linear_float_filter arr
  in
  S.for_testing_weighted_choice_linear_float filtered r

let%test_unit "for_testing_weighted_choice_linear_float: r=0 returns first item"
    =
  let arr = [| ("a", 1.); ("b", 1.); ("c", 1.) |] in
  assert (String.equal "a" (for_testing_weighted_choice_linear_float arr 0.))

let%test_unit
    "for_testing_weighted_choice_linear_float: boundary rolls resolve to the \
     half-open [prev, sum) range they fall into" =
  let arr = [| ("a", 1.); ("b", 1.); ("c", 1.) |] in
  assert (
    String.equal "a"
      (for_testing_weighted_choice_linear_float arr (Stdlib.Float.pred 1.0)));
  assert (String.equal "b" (for_testing_weighted_choice_linear_float arr 1.0));
  assert (
    String.equal "b"
      (for_testing_weighted_choice_linear_float arr (Stdlib.Float.pred 2.0)));
  assert (String.equal "c" (for_testing_weighted_choice_linear_float arr 2.0))

let%test_unit
    "for_testing_weighted_choice_linear_float: a middle zero-weight item is \
     never selected, even exactly at its (empty) boundary" =
  let arr = [| ("a", 1.); ("never", 0.); ("b", 1.) |] in
  assert (
    String.equal "a"
      (for_testing_weighted_choice_linear_float arr (Stdlib.Float.pred 1.0)));
  assert (String.equal "b" (for_testing_weighted_choice_linear_float arr 1.0))

let%test_unit
    "for_testing_weighted_choice_linear_float: a leading zero-weight item is \
     never selected, even at r=0" =
  let arr = [| ("never", 0.); ("a", 1.); ("b", 1.) |] in
  assert (String.equal "a" (for_testing_weighted_choice_linear_float arr 0.));
  assert (
    String.equal "a"
      (for_testing_weighted_choice_linear_float arr (Stdlib.Float.pred 1.0)));
  assert (String.equal "b" (for_testing_weighted_choice_linear_float arr 1.0))

let%test_unit
    "for_testing_weighted_choice_linear_float: a roll landing exactly on the \
     total must not select a trailing zero-weight item" =
  let arr = [| ("a", 1.); ("b", 1.); ("c", 0.) |] in
  assert (String.equal "b" (for_testing_weighted_choice_linear_float arr 2.))

let%test_unit
    "for_testing_weighted_choice_linear_float: multiple trailing zero-weight \
     items" =
  let arr = [| ("a", 1.); ("b", 0.); ("c", 0.) |] in
  assert (String.equal "a" (for_testing_weighted_choice_linear_float arr 1.))

(* As above: only guaranteed to agree when arr is already sorted by weight,
   since weighted_table_float sorts and for_testing_weighted_choice_linear_float
   never does. *)
let%test_unit
    "for_testing_weighted_choice_float and \
     for_testing_weighted_choice_linear_float agree for a sweep of rolls \
     across [0, total], including the boundary at total, when input is \
     pre-sorted by weight" =
  let arr = [| ("c", 1.); ("d", 1.); ("b", 3.); ("a", 5.) |] in
  let tbl = S.weighted_table_float arr in
  let total = 10. in
  let n = 1000 in
  for i = 0 to n do
    let r = Float.of_int i *. total /. Float.of_int n in
    let via_table = S.for_testing_weighted_choice_float tbl r in
    let via_linear = for_testing_weighted_choice_linear_float arr r in
    assert (String.equal via_table via_linear)
  done

(* --- weighted_choice_int for_testing internals (roll-based) ---

   Int counterpart of the float section above. Ints have no epsilon/rounding
   hazard the way [uniform_float *. total] does -- there's no way for a real
   [int state total] draw to round up past [total] -1 -- so the boundary
   cases here exist to pin down the shared selection logic's half-open [prev,
   sum) convention. *)

let%test_unit "for_testing_weighted_choice_int: r=0 returns first item" =
  let tbl = S.weighted_table_int [| ("a", 1); ("b", 1); ("c", 1) |] in
  assert (String.equal "a" (S.for_testing_weighted_choice_int tbl 0))

let%test_unit
    "for_testing_weighted_choice_int: boundary rolls resolve to the half-open \
     [prev, sum) range they fall into" =
  let tbl = S.weighted_table_int [| ("a", 1); ("b", 1); ("c", 1) |] in
  (* Cumulative sums: a:1, b:2, c:3. *)
  assert (String.equal "a" (S.for_testing_weighted_choice_int tbl 0));
  assert (String.equal "b" (S.for_testing_weighted_choice_int tbl 1));
  assert (String.equal "c" (S.for_testing_weighted_choice_int tbl 2))

let%test_unit
    "for_testing_weighted_choice_int: a middle zero-weight item is never \
     selected, even exactly at its (empty) boundary" =
  let tbl = S.weighted_table_int [| ("a", 1); ("never", 0); ("b", 1) |] in
  (* Cumulative sums: a:1, never:1, b:2. *)
  assert (String.equal "a" (S.for_testing_weighted_choice_int tbl 0));
  assert (String.equal "b" (S.for_testing_weighted_choice_int tbl 1))

let%test_unit
    "for_testing_weighted_choice_int: a leading zero-weight item is never \
     selected, even at r=0" =
  let tbl = S.weighted_table_int [| ("never", 0); ("a", 1); ("b", 1) |] in
  (* Cumulative sums: never:0, a:1, b:2. *)
  assert (String.equal "a" (S.for_testing_weighted_choice_int tbl 0));
  assert (String.equal "b" (S.for_testing_weighted_choice_int tbl 1))

let%test_unit
    "for_testing_weighted_choice_int: a roll landing exactly on the total must \
     not select a trailing zero-weight item" =
  let tbl = S.weighted_table_int [| ("a", 1); ("b", 1); ("c", 0) |] in
  assert (String.equal "b" (S.for_testing_weighted_choice_int tbl 2))

let%test_unit
    "for_testing_weighted_choice_int: multiple trailing zero-weight items" =
  let tbl = S.weighted_table_int [| ("a", 1); ("b", 0); ("c", 0) |] in
  assert (String.equal "a" (S.for_testing_weighted_choice_int tbl 1))

(* Joins the two exposed for_testing_ pieces back together, same as
   for_testing_weighted_choice_linear_float above, so the tests below can
   drive the whole thing from a raw array and a chosen roll. *)
let for_testing_weighted_choice_linear_int arr r =
  let _total, filtered = S.for_testing_weighted_choice_linear_int_filter arr in
  S.for_testing_weighted_choice_linear_int filtered r

let%test_unit "for_testing_weighted_choice_linear_int: r=0 returns first item" =
  let arr = [| ("a", 1); ("b", 1); ("c", 1) |] in
  assert (String.equal "a" (for_testing_weighted_choice_linear_int arr 0))

let%test_unit
    "for_testing_weighted_choice_linear_int: boundary rolls resolve to the \
     half-open [prev, sum) range they fall into" =
  let arr = [| ("a", 1); ("b", 1); ("c", 1) |] in
  assert (String.equal "a" (for_testing_weighted_choice_linear_int arr 0));
  assert (String.equal "b" (for_testing_weighted_choice_linear_int arr 1));
  assert (String.equal "c" (for_testing_weighted_choice_linear_int arr 2))

let%test_unit
    "for_testing_weighted_choice_linear_int: a middle zero-weight item is \
     never selected, even exactly at its (empty) boundary" =
  let arr = [| ("a", 1); ("never", 0); ("b", 1) |] in
  assert (String.equal "a" (for_testing_weighted_choice_linear_int arr 0));
  assert (String.equal "b" (for_testing_weighted_choice_linear_int arr 1))

let%test_unit
    "for_testing_weighted_choice_linear_int: a leading zero-weight item is \
     never selected, even at r=0" =
  let arr = [| ("never", 0); ("a", 1); ("b", 1) |] in
  assert (String.equal "a" (for_testing_weighted_choice_linear_int arr 0));
  assert (String.equal "b" (for_testing_weighted_choice_linear_int arr 1))

let%test_unit
    "for_testing_weighted_choice_linear_int: a roll landing exactly on the \
     total must not select a trailing zero-weight item" =
  let arr = [| ("a", 1); ("b", 1); ("c", 0) |] in
  assert (String.equal "b" (for_testing_weighted_choice_linear_int arr 2))

let%test_unit
    "for_testing_weighted_choice_linear_int: multiple trailing zero-weight \
     items" =
  let arr = [| ("a", 1); ("b", 0); ("c", 0) |] in
  assert (String.equal "a" (for_testing_weighted_choice_linear_int arr 1))

let%test_unit
    "for_testing_weighted_choice_int and \
     for_testing_weighted_choice_linear_int agree for every roll across [0, \
     total], including the boundary at total" =
  let arr = [| ("a", 5); ("b", 3); ("c", 1); ("d", 1) |] in
  let tbl = S.weighted_table_int arr in
  let total = 10 in
  for r = 0 to total do
    let via_table = S.for_testing_weighted_choice_int tbl r in
    let via_linear = for_testing_weighted_choice_linear_int arr r in
    assert (String.equal via_table via_linear)
  done

(* --- shuffle / shuffled / shuffled_list --- *)

let%test_unit "shuffle empty array is a no-op" =
  let state = Xs_S.init_deterministic seed in
  let arr = [||] in
  Xs_S.shuffle state arr;
  assert (Array.is_empty arr)

let%test_unit "shuffle single-element array is unchanged" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 20 do
    let arr = [| 42 |] in
    Xs_S.shuffle state arr;
    assert (arr.(0) = 42)
  done

let%test_unit "shuffle result is a permutation of original elements" =
  let state = Xs_S.init_deterministic seed in
  let primes = [| 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31 |] in
  for _ = 1 to 20 do
    let arr = Array.copy primes in
    Xs_S.shuffle state arr;
    let sorted = Array.copy arr in
    Array.sort sorted ~compare:Int.compare;
    assert (Array.equal Int.equal sorted primes)
  done

let%expect_test "shuffle" =
  let state = Xs_S.init_deterministic seed in
  let primes = [| 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31 |] in
  for _ = 1 to 10 do
    Xs_S.shuffle state primes;
    Array.to_list primes |> List.to_string ~f:Int.to_string |> print_endline
  done;
  [%expect
    {|
    [2; 19; 7; 31; 11; 3; 17; 23; 5; 29; 13]
    [29; 7; 13; 2; 23; 3; 5; 19; 31; 17; 11]
    [29; 17; 2; 7; 23; 13; 31; 19; 3; 11; 5]
    [2; 29; 5; 31; 11; 7; 13; 17; 23; 3; 19]
    [23; 3; 2; 7; 19; 11; 17; 31; 29; 13; 5]
    [11; 31; 2; 5; 3; 13; 29; 7; 23; 17; 19]
    [5; 29; 7; 31; 3; 2; 23; 11; 13; 19; 17]
    [5; 7; 29; 17; 31; 2; 11; 23; 13; 19; 3]
    [11; 17; 7; 23; 3; 5; 19; 29; 13; 31; 2]
    [29; 5; 19; 31; 2; 7; 3; 23; 17; 11; 13]
    |}]

let%test_unit "shuffled preserves original array" =
  let state = Xs_S.init_deterministic seed in
  let primes = [| 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31 |] in
  let original = Array.copy primes in
  for _ = 1 to 20 do
    ignore (Xs_S.shuffled state primes);
    assert (Array.equal Int.equal primes original)
  done

let%test_unit "shuffled result contains all original elements" =
  let state = Xs_S.init_deterministic seed in
  let primes = [| 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31 |] in
  for _ = 1 to 20 do
    let result = Xs_S.shuffled state primes in
    let sorted = Array.copy result in
    Array.sort sorted ~compare:Int.compare;
    assert (Array.equal Int.equal sorted primes)
  done

let%expect_test "shuffled" =
  let state = Xs_S.init_deterministic seed in
  let primes = [| 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31 |] in
  for _ = 1 to 10 do
    let arr = Xs_S.shuffled state primes in
    Array.to_list arr |> List.to_string ~f:Int.to_string |> print_endline
  done;
  [%expect
    {|
    [2; 19; 7; 31; 11; 3; 17; 23; 5; 29; 13]
    [29; 5; 31; 2; 19; 13; 23; 3; 7; 17; 11]
    [2; 29; 7; 3; 11; 5; 23; 19; 13; 31; 17]
    [5; 2; 31; 17; 29; 7; 13; 3; 11; 23; 19]
    [23; 29; 2; 13; 31; 11; 19; 7; 3; 17; 5]
    [13; 19; 5; 31; 3; 29; 23; 7; 2; 17; 11]
    [7; 17; 19; 3; 11; 5; 23; 2; 13; 31; 29]
    [2; 5; 3; 31; 7; 13; 19; 17; 23; 29; 11]
    [17; 7; 3; 19; 31; 2; 29; 5; 23; 11; 13]
    [19; 13; 17; 29; 31; 5; 11; 7; 3; 2; 23]
    |}]

let%test_unit "shuffled_list result contains all original elements" =
  let state = Xs_S.init_deterministic seed in
  let primes = [ 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31 ] in
  for _ = 1 to 20 do
    let result = Xs_S.shuffled_list state primes in
    let sorted = List.sort result ~compare:Int.compare in
    assert (List.equal Int.equal sorted primes)
  done

let%expect_test "shuffled_list" =
  let state = Xs_S.init_deterministic seed in
  let _ = Xs_S.bits state in
  let primes = [ 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31 ] in
  for _ = 1 to 32 do
    let l = Xs_S.shuffled_list state primes in
    l |> List.to_string ~f:Int.to_string |> print_endline
  done;
  [%expect
    {|
    [19; 5; 31; 7; 17; 13; 23; 3; 29; 2; 11]
    [3; 31; 29; 17; 11; 23; 2; 5; 19; 13; 7]
    [29; 5; 2; 7; 3; 23; 19; 11; 31; 17; 13]
    [5; 31; 17; 23; 13; 2; 29; 7; 11; 3; 19]
    [29; 19; 11; 31; 7; 3; 5; 23; 2; 13; 17]
    [19; 3; 31; 17; 29; 23; 5; 11; 2; 13; 7]
    [13; 17; 3; 7; 5; 23; 19; 11; 31; 29; 2]
    [3; 2; 31; 29; 11; 19; 13; 17; 23; 5; 7]
    [5; 2; 19; 29; 13; 23; 3; 17; 31; 11; 7]
    [11; 17; 23; 31; 13; 7; 5; 2; 3; 19; 29]
    [2; 31; 13; 19; 17; 29; 11; 3; 7; 23; 5]
    [7; 17; 5; 13; 2; 3; 29; 23; 11; 19; 31]
    [11; 29; 17; 5; 7; 31; 2; 23; 19; 13; 3]
    [2; 19; 5; 13; 11; 29; 17; 23; 7; 31; 3]
    [31; 2; 19; 5; 7; 11; 3; 23; 29; 17; 13]
    [31; 19; 13; 29; 5; 2; 7; 23; 11; 17; 3]
    [13; 17; 11; 3; 2; 7; 5; 23; 19; 29; 31]
    [17; 7; 11; 13; 31; 19; 5; 29; 3; 23; 2]
    [7; 11; 31; 29; 3; 17; 13; 5; 2; 23; 19]
    [13; 11; 31; 5; 3; 23; 19; 17; 29; 2; 7]
    [23; 7; 13; 11; 31; 29; 17; 5; 3; 2; 19]
    [31; 5; 3; 17; 2; 7; 13; 11; 29; 23; 19]
    [13; 3; 7; 2; 17; 11; 31; 5; 23; 19; 29]
    [19; 2; 13; 31; 29; 3; 7; 17; 11; 5; 23]
    [7; 29; 31; 3; 2; 23; 11; 13; 17; 5; 19]
    [11; 19; 13; 5; 17; 23; 29; 2; 31; 7; 3]
    [13; 19; 3; 5; 11; 2; 23; 31; 17; 29; 7]
    [17; 7; 29; 23; 19; 13; 3; 11; 31; 5; 2]
    [23; 17; 3; 29; 19; 13; 11; 5; 2; 7; 31]
    [23; 17; 29; 7; 2; 13; 5; 19; 11; 31; 3]
    [13; 2; 19; 31; 17; 29; 23; 5; 3; 7; 11]
    [11; 31; 3; 13; 17; 7; 23; 29; 2; 5; 19]
    |}]
