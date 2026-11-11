let () = Printexc.record_backtrace true
let () = Random.self_init ()
let available () = true

(* Only libraries with a function that draws from a range are here (GSL,
   splittable_random and Base): the standard library, pringo and the opam
   xoshiro package have only a function that takes an upper bound, so there is
   nothing equivalent to time. The range is a constant that is neither
   symmetric about zero nor of width 1, so the scale and the offset are both
   real work. Base.Random.State.float_range exists from Base v0.10.0, and v0.16
   is the last Base that supports OCaml 4. *)
let range_min = -3.0
let range_max = 7.0

let tests, name =
  let auto_state = Xoshiro_simd.State.init ~backend:Auto () in
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
  let base_state = Base.Random.State.make [| 1; 2; 3; 4 |] in
  let dls : Bench_common.test list =
    [
      {
        name = Bench_backend.dls_name ();
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.float_range ~min:range_min ~max:range_max)));
      };
    ]
  in
  let common : Bench_common.test list =
    [
#ifdef GSL_AVAILABLE
      {
        name = "gsl";
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Gsl.Randist.flat gsl_state ~a:range_min ~b:range_max)));
      };
#endif
      {
        name = "splittable_random";
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Splittable_random.float splittable_state ~lo:range_min
                    ~hi:range_max)));
      };
      {
        name = "base";
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Base.Random.State.float_range base_state range_min range_max)));
      };
      {
        name = Bench_backend.auto_name auto_state;
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.float_range auto_state ~min:range_min
                    ~max:range_max)));
      };
    ]
  in
  (dls @ common, "float-range-libs")

let () = Bench_common.run_and_print name tests
