(* These examples demonstrate two reproducibility guarantees. Every backend on
   this CPU gives identical output, which the example checks. Every host gives
   identical output as well, but to verify that, you must run it on other
   architectures (e.g. AMD64, ARM64, powerpc, s390x), and diff the output. *)

module Xs = Xoshiro_simd

let hex_of_int64_array a =
  String.concat " " (Array.to_list (Array.map (Printf.sprintf "%016Lx") a))

let seed = "xoshiro-simd example seed"

let backend_independence_example () =
  let backends = Xs.available_backends () in
  Printf.printf "Available backends on this build/CPU: %s\n"
    (String.concat ", " (List.map Xs.backend_to_string backends));

  (* Backend-independence: draw the same handful of values under every
     available backend and check they all match. *)
  let draw backend =
    let state = Xs.State.init_string ~backend seed in
    Array.init 5 (fun _ -> Xs.State.bits64 state)
  in
  let baseline = draw (List.hd backends) in
  Printf.printf "\nDrawing 5 int64s from the same seed under every backend:\n";
  List.iter
    (fun backend ->
      let out = draw backend in
      Printf.printf "  %-10s %s  %s\n"
        (Xs.backend_to_string backend)
        (hex_of_int64_array out)
        (if out = baseline then "(matches)" else "(MISMATCH)"))
    backends

let set_backend_limit_example () =
  (* set_backend_limit lets you limit which backends Auto is willing to select.
     This can be useful if, for instance, AVX512 gets downclocked on your CPU.
     Plain is always available so the following call takes effect on every
     host. *)
  Xs.set_backend_limit Xs.Plain;
  let capped = Xs.State.init_string seed in
  Printf.printf
    "\nAfter set_backend_limit Plain, a fresh Auto state resolves to: %s\n"
    (Xs.backend_to_string (Xs.State.state_backend capped))

let () =
  backend_independence_example ();
  set_backend_limit_example ()
