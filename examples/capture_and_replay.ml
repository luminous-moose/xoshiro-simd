(* State.recover_seed_bytes lets you generate a seed string that you can pass to
   init_string to exactly reproduce a state that has had no non-bulk output
   drawn from it. This could be used, for example, to log an OS-seeded
   simulation run so it can be replayed later. Every state can have a seed
   string generated for it, not only states seeded from OS entropy or from
   strings.

   See also battery/ocaml_battery.ml for a real-world example of this pattern
   to make a statistical battery run reproducible with --seed-hex. *)

module Xs = Xoshiro_simd

let run_once state = Array.init 8 (fun _ -> Xs.State.int state 100)

let string_of_int_array a =
  String.concat " " (Array.to_list (Array.map string_of_int a))

let hex_of_string s =
  String.concat ""
    (List.init (String.length s) (fun i ->
         Printf.sprintf "%02x" (Char.code s.[i])))

let first_pass () =
  let state = Xs.State.init () in

  (* Must capture before drawing anything from state. *)
  let seed_bytes = Xs.State.recover_seed_bytes state in
  Printf.printf "Captured seed (log this): %s\n" (hex_of_string seed_bytes);

  let first_run = run_once state in
  Printf.printf "First run:  %s\n" (string_of_int_array first_run);
  (seed_bytes, first_run)

let replay_pass seed_bytes =
  let replayed = Xs.State.init_string seed_bytes in
  let second_run = run_once replayed in
  Printf.printf "Replayed:   %s\n" (string_of_int_array second_run);
  second_run

let () =
  let seed_bytes, first_run = first_pass () in
  let second_run = replay_pass seed_bytes in

  if first_run = second_run then
    print_endline "Replay matches the original run."
  else print_endline "Replay did NOT match (this would be a library bug)."
