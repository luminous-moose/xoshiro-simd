let () = Printexc.record_backtrace true
let () = Random.self_init ()

let available () = true

let nextrange = Bench_data.make_cycler Bench_data.intranges

let (tests : Bench_common.test list), (name : string) =
  let stdlib_state = Random.get_state () in
  let plain_state =
    Xoshiro_simd.State.init_deterministic ~backend:Plain Bench_data.seed64
  in
  let auto_state = Xoshiro_simd.State.init ~backend:Auto () in
  let sse2_state =
    try Xoshiro_simd.State.init ~backend:Sse2 ()
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
        name = Bench_backend.dls_name ~extra:"lemire" ();
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
        name = "xoshiro-simd-lemire-plain";
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.int plain_state (nextrange ()))));
      };
      {
        name =
          (let extra =
             if Stdlib.Sys.word_size = 64 then "int_lemire_native"
             else "int_lemire"
           in
           Bench_backend.auto_name ~extra auto_state);
        available;
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.int auto_state (nextrange ()))));
      };
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
        name = "xoshiro-simd-int_lemire_native-sse2";
        available =
          (fun () -> Bench_backend.supported Sse2 && Stdlib.Sys.word_size = 64);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.int sse2_state (nextrange ()))));
      };
      {
        name = "xoshiro-simd-int_lemire_c-sse2";
        available = (fun () -> Bench_backend.supported Sse2);
        test =
          (fun () ->
            ignore
              (Sys.opaque_identity
                 (Xoshiro_simd.State.for_testing_int_lemire_c sse2_state
                    (nextrange ()))));
      };
    ],
    "intrange-backends" )

let () = Bench_common.run_and_print name tests
