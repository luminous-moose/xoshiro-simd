(* Test: each State.init () call reads fresh entropy, producing a unique seed.
   We call State.init () twice and verify the first 32 bytes of output differ.
   A collision here requires the OS entropy function, or /dev/urandom, to return
   identical 64-bit seeds on two consecutive reads: probability 2^-64. *)

let () =
  let state1 = Xoshiro_simd.State.init () in
  let state2 = Xoshiro_simd.State.init () in
  let bytes1 = Xoshiro_simd.State.get_insecure_bytes state1 32 in
  let bytes2 = Xoshiro_simd.State.get_insecure_bytes state2 32 in
  if Bytes.equal bytes1 bytes2 then (
    Printf.eprintf "FAIL: two State.init () calls produced identical output\n%!";
    exit 1)

(* Test: the module-level DLS implicit init is also uniquely seeded.
   The first call to any Xoshiro_simd function triggers lazy initialization of
   the per-domain state via Domain.DLS (OCaml 5) or a global ref (OCaml 4).
   We verify that this implicit state is independently seeded from an explicit
   State.init () call. *)

let () =
  let dls_bytes = Xoshiro_simd.get_insecure_bytes 32 in
  let state = Xoshiro_simd.State.init () in
  let state_bytes = Xoshiro_simd.State.get_insecure_bytes state 32 in
  if Bytes.equal dls_bytes state_bytes then (
    Printf.eprintf
      "FAIL: module-level DLS init and explicit State.init () produced \
       identical output\n\
       %!";
    exit 1)
