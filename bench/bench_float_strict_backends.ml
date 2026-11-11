let () = Printexc.record_backtrace true
let () = Random.self_init ()
let available () = true

(* Same shape as bench_float_backends.ml, but through [float_strict], which retries
   until the result is neither 0.0 nor the bound. The standard library is the
   comparison that matters: on OCaml 5 its [Random.float] also retries on zero,
   so it is doing the same work; on OCaml 4 it does not retry. The bound is a
   constant other than 1.0, as in bench_float_backends.ml. *)
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
          (fun () ->
            ignore (Sys.opaque_identity (Xoshiro_simd.float_strict bound)));
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
              (Sys.opaque_identity
                 (Xoshiro_simd.State.float_strict plain_state bound)));
      };
      {
        name = Bench_backend.auto_name auto_state;
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.float_strict auto_state bound)));
      };
      {
        name = "xoshiro-simd-sse2";
        available = (fun () -> Bench_backend.supported Sse2);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.float_strict sse2_state bound)));
      };
      {
        name = "xoshiro-simd-avx2";
        available = (fun () -> Bench_backend.supported Avx2);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.float_strict avx2_state bound)));
      };
      {
        name = "xoshiro-simd-avx512fvl";
        available = (fun () -> Bench_backend.supported Avx512fvl);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.float_strict avx512fvl_state bound)));
      };
      {
        name = "xoshiro-simd-neon";
        available = (fun () -> Bench_backend.supported Neon);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.float_strict neon_state bound)));
      };
    ]
  in
  (dls @ backend_tests, "float-strict-backends")

let () = Bench_common.run_and_print name tests
