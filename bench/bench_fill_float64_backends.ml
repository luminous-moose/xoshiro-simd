let () = Printexc.record_backtrace true
let n = 1024
let available () = true

let tests, name =
  let dls_ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
  let dls : Bench_common.test list =
    [
      {
        name = Bench_backend.dls_name ();
        available;
        test =
          (fun () ->
            Sys.opaque_identity (Xoshiro_simd.fill_float64_bigarray dls_ba));
      };
    ]
  in
  let plain_state = Xoshiro_simd.State.init ~backend:Plain () in
  let plain_ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
  let auto_state = Xoshiro_simd.State.init ~backend:Auto () in
  let auto_ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
  (* The states below are set with a try/with expression so that they are plain
     values, not optionals or lazy, because slower access to those could ruin
     the benchmark results. *)
  let sse2_state =
    try Xoshiro_simd.State.init ~backend:Sse2 ()
    with Invalid_argument _ -> auto_state
  in
  let sse2_ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
  let avx2_state =
    try Xoshiro_simd.State.init ~backend:Avx2 ()
    with Invalid_argument _ -> auto_state
  in
  let avx2_ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
  let avx512fvl_state =
    try Xoshiro_simd.State.init ~backend:Avx512fvl ()
    with Invalid_argument _ -> auto_state
  in
  let avx512fvl_ba =
    Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n
  in
  let neon_state =
    try Xoshiro_simd.State.init ~backend:Neon ()
    with Invalid_argument _ -> auto_state
  in
  let neon_ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
  let backend_tests : Bench_common.test list =
    [
      {
        name = "xoshiro-simd-plain";
        available;
        test =
          (fun () ->
            Sys.opaque_identity
              (Xoshiro_simd.State.fill_float64_bigarray plain_state plain_ba));
      };
      {
        name = Bench_backend.auto_name auto_state;
        available;
        test =
          (fun () ->
            Sys.opaque_identity
              (Xoshiro_simd.State.fill_float64_bigarray auto_state auto_ba));
      };
      {
        name = "xoshiro-simd-sse2";
        available = (fun () -> Bench_backend.supported Sse2);
        test =
          (fun () ->
            Sys.opaque_identity
              (Xoshiro_simd.State.fill_float64_bigarray sse2_state sse2_ba));
      };
      {
        name = "xoshiro-simd-avx2";
        available = (fun () -> Bench_backend.supported Avx2);
        test =
          (fun () ->
            Sys.opaque_identity
              (Xoshiro_simd.State.fill_float64_bigarray avx2_state avx2_ba));
      };
      {
        name = "xoshiro-simd-avx512fvl";
        available = (fun () -> Bench_backend.supported Avx512fvl);
        test =
          (fun () ->
            Sys.opaque_identity
              (Xoshiro_simd.State.fill_float64_bigarray avx512fvl_state
                 avx512fvl_ba));
      };
      {
        name = "xoshiro-simd-neon";
        available = (fun () -> Bench_backend.supported Neon);
        test =
          (fun () ->
            Sys.opaque_identity
              (Xoshiro_simd.State.fill_float64_bigarray neon_state neon_ba));
      };
    ]
  in
  (dls @ backend_tests, "fill_float64_bigarray-backends")

let () =
  Printf.printf "# fill_float64_bigarray %d\n" n;
  Bench_common.run_and_print name tests
