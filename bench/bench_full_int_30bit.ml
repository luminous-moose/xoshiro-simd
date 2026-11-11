let () = Printexc.record_backtrace true
let () = Random.self_init ()
let available () = true

(* All ranges are < 2^30, keeping full_int on the fast path (no 64-bit
   multiplication needed). Mirrors bench_intrange but calls full_int. *)
let tests, name =
  let ranges = Bench_data.intranges in
  let idx = ref 0 in
  let nextrange () =
    if !idx = 64 then idx := 0;
    let v = ranges.(!idx) in
    idx := !idx + 1;
    v
  in
  let dls : Bench_common.test list =
    [
      {
        name = "stdlib-dls";
        available;
        test =
          (fun () ->
            ignore (Sys.opaque_identity (Random.full_int (nextrange ()))));
      };
      {
        name = Bench_backend.dls_name ();
        available;
        test =
          (fun () ->
            ignore (Sys.opaque_identity (Xoshiro_simd.full_int (nextrange ()))));
      };
    ]
  in
  let c_state =
    Xoshiro_simd.State.init_deterministic ~backend:Plain Bench_data.seed64
  in
  let stdlib_state = Random.get_state () in
  (* The states below are set with a try/with expression so that they are plain
     values, not optionals or lazy, because slower access to those could ruin
     the benchmark results. *)
  let sse2_state =
    try Xoshiro_simd.State.init ~backend:Sse2 ()
    with Invalid_argument _ -> c_state
  in
  let avx2_state =
    try Xoshiro_simd.State.init ~backend:Avx2 ()
    with Invalid_argument _ -> c_state
  in
  let avx512fvl_state =
    try Xoshiro_simd.State.init ~backend:Avx512fvl ()
    with Invalid_argument _ -> c_state
  in
  let neon_state =
    try Xoshiro_simd.State.init ~backend:Neon ()
    with Invalid_argument _ -> c_state
  in
  let backend_tests : Bench_common.test list =
    [
      {
        name = "stdlib";
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Random.State.full_int stdlib_state (nextrange ()))));
      };
      {
        name = "xoshiro";
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.full_int c_state (nextrange ()))));
      };
      {
        name = "xoshiro-simd-sse2";
        available = (fun () -> Bench_backend.supported Sse2);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.full_int sse2_state (nextrange ()))));
      };
      {
        name = "xoshiro-simd-avx2";
        available = (fun () -> Bench_backend.supported Avx2);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.full_int avx2_state (nextrange ()))));
      };
      {
        name = "xoshiro-simd-avx512fvl";
        available = (fun () -> Bench_backend.supported Avx512fvl);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.full_int avx512fvl_state (nextrange ()))));
      };
      {
        name = "xoshiro-simd-neon";
        available = (fun () -> Bench_backend.supported Neon);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.full_int neon_state (nextrange ()))));
      };
    ]
  in
  (dls @ backend_tests, "full_int_30bit")

let () = Bench_common.run_and_print name tests
