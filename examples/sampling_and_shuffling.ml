(* These are examples of sampling helpers that Stdlib.Random doesn't have:
   shuffle, weighted choice, and reservoir sampling without replacement. *)

module Xs = Xoshiro_simd

let string_of_int_array a =
  String.concat " " (Array.to_list (Array.map string_of_int a))

let () =
  let deck = Array.init 52 (fun i -> i) in
  Xs.shuffle deck;
  Printf.printf "Shuffled deck of 52: %s\n" (string_of_int_array deck);

  let sample = Xs.reservoir_sample deck 5 in
  Printf.printf "Reservoir sample of 5 (no replacement): %s\n"
    (string_of_int_array sample);

  (* Weighted choice: build a table once, draw from it many times at
     O(log n) per draw. There are both float and int versions. *)
  let loot_table =
    Xs.weighted_table_float
      [| ("common", 70.0); ("rare", 25.0); ("legendary", 5.0) |]
  in
  let counts = Hashtbl.create 3 in
  for _ = 1 to 1000 do
    let item = Xs.weighted_choice_float loot_table in
    let prior = try Hashtbl.find counts item with Not_found -> 0 in
    Hashtbl.replace counts item (prior + 1)
  done;
  Printf.printf "1000 weighted draws from loot_table:\n";
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) counts []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  |> List.iter (fun (k, v) -> Printf.printf "  %-10s %4d\n" k v);

  (* weighted_bool: a quick independent coin flip at a chosen probability. *)
  let heads = ref 0 in
  for _ = 1 to 1000 do
    if Xs.weighted_bool 0.3 then incr heads
  done;
  Printf.printf "weighted_bool 0.3, heads out of 1000 flips: %d\n" !heads
