(* OCaml 5 only (see test/dune). Several domains at once call
   set_backend_limit, self_init, reseed and split and draw from the module-level
   generator, all on the same process-global backend limit. The .mli calls
   set_backend_limit "safe to call from multiple domains". What can be checked
   from outside is that nothing crashes, every state ends up on a backend this
   CPU supports, the draws stay in range, and the split children are distinct.
   A data race that only corrupted the limit's value wouldn't show up here: that
   relies on the limit being a C11 atomic, which this can't observe. *)

module X = Xoshiro_simd

let fail fmt =
  Printf.ksprintf
    (fun s ->
      prerr_endline ("FAIL: " ^ s);
      exit 1)
    fmt

let supported = X.available_backends ()
let is_supported b = List.exists (X.backend_equal b) supported
let every_backend = [| X.Auto; X.Plain; X.Sse2; X.Avx2; X.Avx512fvl; X.Neon |]
let explicit = Array.of_list supported
let rounds = 200

let worker id () =
  (* Each domain picks its actions from its own explicit state, so the run
     is the same every time apart from thread interleaving. *)
  let pick = X.State.init_deterministic (Int64.of_int id) in
  let firsts = ref [] in
  for _ = 1 to rounds do
    (match X.State.int pick 5 with
    | 0 ->
        X.set_backend_limit
          every_backend.(X.State.int pick (Array.length every_backend))
    | 1 -> X.self_init ()
    | 2 ->
        X.self_init
          ~backend:explicit.(X.State.int pick (Array.length explicit))
          ()
    | 3 -> X.reseed ()
    | _ ->
        let child = X.split () in
        if not (is_supported (X.State.state_backend child)) then
          fail "split child on unsupported backend %s"
            (X.backend_to_string (X.State.state_backend child));
        firsts := X.State.bits64 child :: !firsts);
    let s = X.State.init () in
    if not (is_supported (X.State.state_backend s)) then
      fail "State.init on unsupported backend %s"
        (X.backend_to_string (X.State.state_backend s));
    for _ = 1 to 20 do
      let v = X.int 10 in
      if v < 0 || v >= 10 then fail "int 10 returned %d" v;
      let f = X.uniform_float () in
      if not (f >= 0. && f < 1.) then fail "uniform_float returned %h" f
    done
  done;
  !firsts

let () =
  let domains = List.init 8 (fun id -> Domain.spawn (worker id)) in
  let firsts = List.concat_map Domain.join domains in
  let distinct = List.sort_uniq Int64.compare firsts in
  if List.length distinct <> List.length firsts then
    fail "%d split children but only %d distinct first draws"
      (List.length firsts) (List.length distinct);
  (* Lifting the limit afterwards must give Auto its first choice again. *)
  X.set_backend_limit X.Auto;
  let backend = X.State.state_backend (X.State.init ()) in
  if not (X.backend_equal backend (List.hd supported)) then
    fail "after set_backend_limit Auto, Auto picked %s, expected %s"
      (X.backend_to_string backend)
      (X.backend_to_string (List.hd supported))
