let () = Printexc.record_backtrace true
let () = Random.self_init ()
let available () = true

(* Stdlib has no function that returns a float in [0, 1) without a bound, so
   every test uses the function that takes an upper bound, which is what code
   written for Stdlib calls. The bound is a constant other than 1.0 so that
   scaling by it is real work and not a multiply by one. *)
let bound = 10.0

let tests, name =
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
  let stdlib_state = Random.get_state () in
  let plain_state = Xoshiro_simd.State.init ~backend:Plain () in
  let auto_state = Xoshiro_simd.State.init ~backend:Auto () in
  (* The states below are set with a try/with expression so that they are plain
     values, not optionals or lazy, because slower access to those could ruin
     the benchmark results. *)
  let sse2_state =
    try Xoshiro_simd.State.init ~backend:Sse2 ()
    with Invalid_argument _ -> auto_state
  in
  let avx2_state =
    try Xoshiro_simd.State.init ~backend:Avx2 ()
    with Invalid_argument _ -> auto_state
  in
  let avx512fvl_state =
    try Xoshiro_simd.State.init ~backend:Avx512fvl ()
    with Invalid_argument _ -> auto_state
  in
  let neon_state =
    try Xoshiro_simd.State.init ~backend:Neon ()
    with Invalid_argument _ -> auto_state
  in
  let backend_tests : Bench_common.test list =
    [
      {
        name = "stdlib";
        available;
        test =
          (fun () ->
            ignore (Sys.opaque_identity (Random.State.float stdlib_state bound)));
      };
      {
        name = "xoshiro-simd-plain";
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity (Xoshiro_simd.State.float plain_state bound)));
      };
      {
        name = Bench_backend.auto_name auto_state;
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity (Xoshiro_simd.State.float auto_state bound)));
      };
      {
        name = "xoshiro-simd-sse2";
        available = (fun () -> Bench_backend.supported Sse2);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity (Xoshiro_simd.State.float sse2_state bound)));
      };
      {
        name = "xoshiro-simd-avx2";
        available = (fun () -> Bench_backend.supported Avx2);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity (Xoshiro_simd.State.float avx2_state bound)));
      };
      {
        name = "xoshiro-simd-avx512fvl";
        available = (fun () -> Bench_backend.supported Avx512fvl);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.float avx512fvl_state bound)));
      };
      {
        name = "xoshiro-simd-neon";
        available = (fun () -> Bench_backend.supported Neon);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity (Xoshiro_simd.State.float neon_state bound)));
      };
    ]
  in
  (dls @ backend_tests, "float-backends")

let () = Bench_common.run_and_print name tests
