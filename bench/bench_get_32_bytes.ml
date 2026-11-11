let buf_size = 32
let () = Printexc.record_backtrace true
let available () = true

let tests, name =
  let dls : Bench_common.test list =
    [
      {
        name = Bench_backend.dls_name ();
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity (Xoshiro_simd.get_insecure_bytes buf_size)));
      };
    ]
  in
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
        name = "xoshiro-simd-plain";
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.get_insecure_bytes plain_state buf_size)));
      };
      {
        name = Bench_backend.auto_name auto_state;
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.get_insecure_bytes auto_state buf_size)));
      };
      {
        name = "xoshiro-simd-sse2";
        available = (fun () -> Bench_backend.supported Sse2);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.get_insecure_bytes sse2_state buf_size)));
      };
      {
        name = "xoshiro-simd-avx2";
        available = (fun () -> Bench_backend.supported Avx2);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.get_insecure_bytes avx2_state buf_size)));
      };
      {
        name = "xoshiro-simd-avx512fvl";
        available = (fun () -> Bench_backend.supported Avx512fvl);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.get_insecure_bytes avx512fvl_state buf_size)));
      };
      {
        name = "xoshiro-simd-neon";
        available = (fun () -> Bench_backend.supported Neon);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.get_insecure_bytes neon_state buf_size)));
      };
    ]
  in
  (dls @ backend_tests, "get_insecure_bytes")

let () =
  Printf.printf "# get_insecure_bytes %d\n" buf_size;
  Bench_common.run_and_print name tests
