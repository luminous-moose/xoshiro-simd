let () = Printexc.record_backtrace true
let () = Random.self_init ()

let available () = true

(* Same shape as bench_intrange_backends, but with bounds that make the
   rejection path of Lemire's method common (see Bench_data.high_intranges),
   so that changes to how often it rejects show up in the timings. *)
let nextrange = Bench_data.make_cycler Bench_data.high_intranges

let (tests : Bench_common.test list), (name : string) =
  let stdlib_state = Random.get_state () in
  let plain_state =
    Xoshiro_simd.State.init_deterministic ~backend:Plain Bench_data.seed64
  in
  let auto_state = Xoshiro_simd.State.init ~backend:Auto () in
  (* The states below are set with a try/with expression so that they are
     plain values, not optionals or lazy, because slower access to those could
     ruin the benchmark results. *)
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
  ( [
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
      {
        name = "stdlib";
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Random.State.int stdlib_state (nextrange ()))));
      };
      {
        name = "xoshiro-simd-plain";
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.int plain_state (nextrange ()))));
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
      (* On 64-bit, State.int is int_lemire_native; this times the C kernel
         that 32-bit uses instead, rejections included, on the same host. *)
      {
        name = Bench_backend.auto_name ~extra:"int_lemire_c" auto_state;
        available = (fun () -> Stdlib.Sys.word_size = 64);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.for_testing_int_lemire_c auto_state
                    (nextrange ()))));
      };
      {
        name = "xoshiro-simd-sse2";
        available = (fun () -> Bench_backend.supported Sse2);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.int sse2_state (nextrange ()))));
      };
      {
        name = "xoshiro-simd-avx2";
        available = (fun () -> Bench_backend.supported Avx2);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.int avx2_state (nextrange ()))));
      };
      {
        name = "xoshiro-simd-avx512fvl";
        available = (fun () -> Bench_backend.supported Avx512fvl);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.int avx512fvl_state (nextrange ()))));
      };
      {
        name = "xoshiro-simd-neon";
        available = (fun () -> Bench_backend.supported Neon);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.int neon_state (nextrange ()))));
      };
    ],
    "intrange-high-backends" )

let () = Bench_common.run_and_print name tests
