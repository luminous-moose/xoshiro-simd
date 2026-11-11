(* These are examples of how to manage state for multiple workers. It can be
   done explicitly, with State.split, which derives a deterministic child state
   from a parent, or it can be done implicitly with Domain-local storage. Every
   new domain created automatically splits state from the parent so you don't
   have to manage it at all. Forking is different and more work to handle
   correctly: see examples/forking_streams.ml. *)

module Xs = Xoshiro_simd

let string_of_int_array a =
  String.concat " " (Array.to_list (Array.map string_of_int a))

let automatic_dls_example () =
  (* Here the state is managed automatically as part of new domain creation, and
     no explicit State.split calls are needed. *)
  Xs.init_string "automatic domain split example";
  Printf.printf "\nAutomatic per-domain split, 4 real domains:\n";
  let domains =
    List.init 4 (fun i ->
        Domain.spawn (fun () ->
            let draws = Array.init 3 (fun _ -> Xs.int 1000) in
            (i, draws)))
  in
  List.iter
    (fun d ->
      let i, draws = Domain.join d in
      Printf.printf "  domain %d: %s\n" i (string_of_int_array draws))
    domains

let manual_example () =
  (* Manual management of states. You don't have to use domains here, manual
     splitting works with forks too, where automatic splitting does not. *)
  let parent = Xs.State.init_string "manual split example" in
  Printf.printf "Manual split, 4 independent worker streams:\n";
  let domains =
    List.init 4 (fun i ->
        let worker_state = Xs.State.split parent in
        Domain.spawn (fun () ->
            let draws =
              Array.init 3 (fun _ -> Xs.State.int worker_state 1000)
            in
            (i, draws)))
  in
  List.iter
    (fun d ->
      let i, draws = Domain.join d in
      Printf.printf "  worker %d: %s\n" i (string_of_int_array draws))
    domains

let () =
  manual_example ();
  automatic_dls_example ()
