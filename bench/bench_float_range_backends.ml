let () = Printexc.record_backtrace true
let () = Random.self_init ()
let available () = true

(* The standard library has no function that draws a float from a range, so
   there is nothing to compare [float_range] with: this benchmark only
   compares xoshiro-simd's own backends. The range is a constant that is
   neither symmetric about zero nor of width 1, so the scale and the offset are
   both real work. *)
let range_min = -3.0
let range_max = 7.0

let tests, name =
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
                 (Xoshiro_simd.State.float_range plain_state ~min:range_min
                    ~max:range_max)));
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
      {
        name = "xoshiro-simd-sse2";
        available = (fun () -> Bench_backend.supported Sse2);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.float_range sse2_state ~min:range_min
                    ~max:range_max)));
      };
      {
        name = "xoshiro-simd-avx2";
        available = (fun () -> Bench_backend.supported Avx2);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.float_range avx2_state ~min:range_min
                    ~max:range_max)));
      };
      {
        name = "xoshiro-simd-avx512fvl";
        available = (fun () -> Bench_backend.supported Avx512fvl);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.float_range avx512fvl_state ~min:range_min
                    ~max:range_max)));
      };
      {
        name = "xoshiro-simd-neon";
        available = (fun () -> Bench_backend.supported Neon);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.float_range neon_state ~min:range_min
                    ~max:range_max)));
      };
    ]
  in
  (dls @ backend_tests, "float-range-backends")

let () = Bench_common.run_and_print name tests
