(* Forked processes get no automatic splitting: a child starts with an exact
   copy of the parent's generator. If no action is taken to manage the state, or
   if it's split in the child (see handled_badly ()), the children will generate
   identical outputs. There are two fixes: one is to split in the parent before
   each fork, and then either use the new state directly or use it to initialize
   the module-level state. The other is to reseed from OS entropy in the child,
   which is not reproducible. *)

module Xs = Xoshiro_simd

let string_of_int_array a =
  String.concat " " (Array.to_list (Array.map string_of_int a))

(* Run f in a forked child, and wait for it to finish. The flush stops the
   child from repeating any output still buffered in the parent. *)
let in_child label f =
  flush stdout;
  match Unix.fork () with
  | 0 ->
      let draws = f () in
      Printf.printf "  %s: %s\n%!" label (string_of_int_array draws);
      exit 0
  | pid -> ignore (Unix.waitpid [] pid)

let draw_3 draw = Array.init 3 (fun _ -> draw ())

let unhandled () =
  Printf.printf "Nothing done, so both children repeat each other:\n";
  for child = 0 to 1 do
    in_child (Printf.sprintf "child %d" child) (fun () ->
        draw_3 (fun () -> Xs.int 1000))
  done

let handled_badly () =
  (* Each child splits from the same copy of the parent state, so each child
     gets the same new state. *)
  Printf.printf "Split in the child after forking (does not work correctly):\n";
  for child = 0 to 1 do
    in_child (Printf.sprintf "child %d" child) (fun () ->
        let child_state = Xs.split () in
        draw_3 (fun () -> Xs.State.int child_state 1000))
  done

let split_in_parent () =
  Printf.printf "Split in the parent before forking:\n";
  for child = 0 to 1 do
    let child_state = Xs.split () in
    in_child (Printf.sprintf "child %d" child) (fun () ->
        draw_3 (fun () -> Xs.State.int child_state 1000))
  done

let split_in_parent_and_seed_module_level () =
  (* There's no way to install a State.t as the module-level generator, so
     instead the child creates a seed from child_state and uses that to
     initialize the module-level generator. *)
  Printf.printf
    "Split in the parent before forking, and use module-level functions:\n";
  for child = 0 to 1 do
    let child_state = Xs.split () in
    in_child (Printf.sprintf "child %d" child) (fun () ->
        let deterministic_seed = Xs.State.get_insecure_bytes child_state 32 in
        Xs.init_string (Bytes.to_string deterministic_seed);
        draw_3 (fun () -> Xs.int 1000))
  done

let reseed () =
  Printf.printf "Reseed in the child (different on every run):\n";
  for child = 0 to 1 do
    in_child (Printf.sprintf "child %d" child) (fun () ->
        Xs.reseed ();
        draw_3 (fun () -> Xs.int 1000))
  done

let () =
  Printf.printf "\nForking, 2 child processes each time:\n";
  if Sys.win32 then Printf.printf "  (no fork on Windows, skipped)\n"
  else begin
    Xs.init_string "forking example";
    unhandled ();
    handled_badly ();
    split_in_parent ();
    split_in_parent_and_seed_module_level ();
    reseed ()
  end
