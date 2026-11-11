let () = Printexc.record_backtrace true
let () = Random.self_init ()
let available () = true

let tests, name =
  let auto_state = Xoshiro_simd.State.init ~backend:Auto () in
  let nextrange = Bench_data.make_cycler Bench_data.intranges in
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
        test =
          (fun () -> ignore (Sys.opaque_identity (Random.int (nextrange ()))));
      };
      {
        name = Bench_backend.dls_name ();
        available;
        test =
          (fun () ->
            ignore (Sys.opaque_identity (Xoshiro_simd.int (nextrange ()))));
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
            ignore
              (Sys.opaque_identity
                 (Random.State.int stdlib_state (nextrange ()))));
      };
#ifdef GSL_AVAILABLE
      {
        name = "gsl";
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Gsl.Rng.uniform_int gsl_state (nextrange ()))));
      };
#endif
      {
        name = "splittable_random";
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Splittable_random.int splittable_state ~lo:0
                    ~hi:(nextrange () - 1))));
      };
      {
        name = "pringo";
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (PRNG.Splitmix.State.int pringo_state (nextrange ()))));
      };
      {
        name = "xoshiro";
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro.State.int xoshiro_state (nextrange ()))));
      };
      {
        name = Bench_backend.auto_name auto_state;
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.int auto_state (nextrange ()))));
      };
    ]
  in
  (dls @ common, "intrange-libs")

let () = Bench_common.run_and_print name tests
