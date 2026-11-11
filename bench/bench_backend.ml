(* Helpers for naming and selecting the backends being benchmarked. *)

let supported backend =
  List.exists
    (Xoshiro_simd.backend_equal backend)
    (Xoshiro_simd.available_backends ())

let backend_name : Xoshiro_simd.backend -> string = function
  | Auto -> "auto"
  | Plain -> "plain"
  | Sse2 -> "sse2"
  | Avx2 -> "avx2"
  | Avx512fvl -> "avx512fvl"
  | Neon -> "neon"

(* Auto doesn't always resolve to the most capable entry in [available_backends]
   -- a caller may have restricted it via [set_backend_limit] (see
   xoshiro_simd.mli), so a less-preferred backend can be picked even
   when the CPU supports something better. [state] must have been created
   with [~backend:Auto] (or default); querying [state_backend] is what actually
   tells us what Auto picked, rather than assuming it's the first (most
   capable) entry. *)
let resolved_backend_name ?extra ~suffix state =
  let iname = backend_name (Xoshiro_simd.State.state_backend state) in
  let middle =
    match extra with Some s -> Printf.sprintf "%s-%s" iname s | None -> iname
  in
  Printf.sprintf "xoshiro-simd-%s-%s" middle suffix

(* We can't get the domain-local state, but split () returns a state that is
   guaranteed to have the same backend as it, so we call that to get the backend that
   the DLS state uses so we can report what Auto picked for it. *)
let dls_name ?extra () =
  resolved_backend_name ?extra ~suffix:"dls" (Xoshiro_simd.split ())

(* Non-DLS counterpart of [dls_name], for any locally-constructed
   [~backend:Auto] state -- e.g. "xoshiro-simd-avx2-auto" rather than a bare
   "xoshiro-simd-auto" that doesn't say what Auto actually picked, and
   without which it could collide with an explicitly-named
   "xoshiro-simd-avx2" entry in the same benchmark run. *)
let auto_name ?extra state = resolved_backend_name ?extra ~suffix:"auto" state
