(* Ways to set up two identical states, if that is desired: seed both from a
   fixed string, seed both from a stored random string, or recover the seed
   string from a state that was seeded from OS entropy. *)

open Printf
module Xs = Xoshiro_simd

let print_results msg a1 a2 name1 name2 : unit =
  let string_of_array a =
    sprintf "[%s]" @@ String.concat " " @@ Array.to_list
    @@ Array.map Int.to_string a
  in
  let a1 = string_of_array a1 in
  let a2 = string_of_array a2 in
  let equal = if a1 = a2 then "equal" else "not equal" in
  printf "%s:\n%s: %s\n%s: %s\n%s\n" msg name1 a1 name2 a2 equal

let duplication_example_fixed_string () =
  let a = Xs.State.init_string "duplicate me" in
  let b = Xs.State.init_string "duplicate me" in
  let draws_from state = Array.init 5 (fun _ -> Xs.State.bits state) in
  print_results
    "Two states seeded from the same fixed string produce identical results"
    (draws_from a) (draws_from b) "a" "b"

let duplication_example_stored_random_string () =
  Xs.reseed ();
  let seed_string = Xs.get_insecure_bytes 32 |> String.of_bytes in
  let a = Xs.State.init_string seed_string in
  let b = Xs.State.init_string seed_string in
  let draws_from state = Array.init 5 (fun _ -> Xs.State.bits state) in
  print_results
    "Two states seeded from the same random string produce identical results"
    (draws_from a) (draws_from b) "a" "b"

let duplication_example_recovered_string () =
  let a = Xs.State.init () in
  let seed_string = Xs.State.recover_seed_bytes a in
  let b = Xs.State.init_string seed_string in
  let draws_from state = Array.init 5 (fun _ -> Xs.State.bits state) in
  print_results
    "One state seeded from OS entropy and another seeded from the string \
     recovered from the first produce identical results"
    (draws_from a) (draws_from b) "a" "b"

let () =
  duplication_example_fixed_string ();
  duplication_example_stored_random_string ();
  duplication_example_recovered_string ()
