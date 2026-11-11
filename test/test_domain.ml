(* This file is unconditionally compiled, regardless of the OCaml version being
   5 or 4, to test the dune conditional compilation logic for dls_compat.ml and
   catch any failures to compile the correct module for theOCaml version. It
   being unconditional means there's no way it can share a bug with the logic
   being tested. The for_testing dls_backend and spawn_and_join are always
   built, and have different implementations depending on major version. Each
   backend has a hardcoded string in it to name it, reached with the
   for_testing_dls_variant function: we check that the string is as expected,
   and then call for_testing_spawn_and_join to verify that the state is updated
   with split in a new domain on OCaml 5 where domains exist. *)

let ocaml_major =
  match String.index_opt Sys.ocaml_version '.' with
  | Some i -> int_of_string (String.sub Sys.ocaml_version 0 i)
  | None -> failwith ("could not parse Sys.ocaml_version: " ^ Sys.ocaml_version)

let expected_variant = if ocaml_major >= 5 then "ocaml5" else "ocaml4"
let actual_variant = Xoshiro_simd.for_testing_dls_variant ()

let () =
  if not (String.equal actual_variant expected_variant) then (
    Printf.eprintf
      "FAIL: running OCaml %s (major %d) but the library's Dls_compat was \
       built with the %S backend -- dune selected the wrong dls_compat.ml \
       variant for this compiler\n\
       %!"
      Sys.ocaml_version ocaml_major actual_variant;
    exit 1)

(* Only reachable when for_testing_spawn_and_join can really spawn (the
   backend check above already confirms that matches ocaml_major >= 5).
   Verifies split_from_parent is wired into the domain-local state key
   end-to-end: a freshly-spawned domain's state must be exactly what
   {!Xoshiro_simd.State.split} applied to the parent's pre-spawn state would
   produce -- not merely "looks different from the parent", which would
   also hold under a shared-singleton bug (a child that accidentally continues
   the parent's own advancing stream also "looks different" from whatever the
   parent drew before spawning).

   There is no public way to get at a domain's live state, so the check is on
   behavior instead: the child draws from its module-level generator, and the
   draws must equal those from a split of an identically-seeded reference
   state. Eight blocks of output (256 bytes) depend on every word of all four
   lanes' states, so a mismatch anywhere in the derived state shows up. *)
let seed = 424242L

let () =
  Xoshiro_simd.init_deterministic seed;
  (* Identical to the parent's domain-local state at this point -- neither has
     been drawn from yet. Splitting this independent state predicts what
     split_from_parent should produce at spawn time, without disturbing the
     real parent's own state. *)
  let reference = Xoshiro_simd.State.init_deterministic seed in
  let expected =
    Xoshiro_simd.State.(get_insecure_bytes (split reference) 256)
  in
  match
    Xoshiro_simd.for_testing_spawn_and_join (fun () ->
        Xoshiro_simd.get_insecure_bytes 256)
  with
  | None ->
      () (* OCaml 4: nothing to spawn into; backend check above covers it *)
  | Some actual ->
      if not (Bytes.equal expected actual) then (
        Printf.eprintf
          "FAIL: spawned domain's state was not derived via split_from_parent \
           as expected\n\
           %!";
        exit 1)
