let () = Printexc.record_backtrace true
let () = Random.self_init ()
let available () = true

(* Stdlib has no function that returns a float in [0, 1) without a bound, so
   every library is measured through its own function that takes an upper
   bound, which is what code written for Stdlib calls. Libraries whose bounded
   function is a range (GSL, splittable_random) get a lower bound of zero. The
   bound is a constant other than 1.0 so that scaling by it is real work and
   not a multiply by one. *)
let bound = 10.0

let tests, name =
  let auto_state = Xoshiro_simd.State.init ~backend:Auto () in
  let stdlib_state = Random.get_state () in
#ifdef GSL_AVAILABLE
  let gsl_state = Gsl.Rng.make (Gsl.Rng.default ()) in
  let () = Gsl.Rng.set gsl_state 42n in
#endif
  (* Splittable_random < v0.17 (needed for 32-bit, where newer versions are
     unavailable) only has [of_int] nested under [State]; newer versions hoist
     it to the top level and deprecate the [State] alias. *)
  let splittable_state =
    (Splittable_random.State.of_int [@alert "-deprecated"]) 42
  in
  let pringo_state = PRNG.Splitmix.State.make [| 1; 2; 3; 4 |] in
  let xoshiro_state = Xoshiro.State.make [| 1; 2; 3; 4 |] in
  let dls : Bench_common.test list =
    [
      {
        name = "stdlib-dls";
        available;
        test = (fun () -> ignore (Sys.opaque_identity (Random.float bound)));
      };
      {
        name = Bench_backend.dls_name ();
        available;
        test =
          (fun () -> ignore (Sys.opaque_identity (Xoshiro_simd.float bound)));
      };
    ]
  in
  let common : Bench_common.test list =
    [
      {
        name = "stdlib";
        available;
        test =
          (fun () ->
            ignore (Sys.opaque_identity (Random.State.float stdlib_state bound)));
      };
#ifdef GSL_AVAILABLE
      {
        name = "gsl";
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity (Gsl.Randist.flat gsl_state ~a:0. ~b:bound)));
      };
#endif
      {
        name = "splittable_random";
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Splittable_random.float splittable_state ~lo:0. ~hi:bound)));
      };
      {
        name = "pringo";
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (PRNG.Splitmix.State.float pringo_state bound)));
      };
      {
        name = "xoshiro";
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity (Xoshiro.State.float xoshiro_state bound)));
      };
      {
        name = Bench_backend.auto_name auto_state;
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity (Xoshiro_simd.State.float auto_state bound)));
      };
    ]
  in
  (dls @ common, "float-libs")

let () = Bench_common.run_and_print name tests
