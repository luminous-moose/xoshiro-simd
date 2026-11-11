let () = Printexc.record_backtrace true
let n = 1024
let available () = true

let tests, name =
  let scalar_state = Xoshiro_simd.State.init ~backend:Auto () in
  let scalar_ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
  let bulk_state = Xoshiro_simd.State.init ~backend:Auto () in
  let bulk_ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
  let (common : Bench_common.test list) =
    [
      {
        name = Bench_backend.auto_name ~extra:"scalar-loop" scalar_state;
        available;
        test =
          (fun () ->
            Sys.opaque_identity
              (for i = 0 to n - 1 do
                 Bigarray.Array1.unsafe_set scalar_ba i
                   (Xoshiro_simd.State.uniform_float scalar_state)
               done));
      };
      {
        name = Bench_backend.auto_name ~extra:"fill" bulk_state;
        available;
        test =
          (fun () ->
            Sys.opaque_identity
              (Xoshiro_simd.State.fill_float64_bigarray bulk_state bulk_ba));
      };
    ]
  in
  (common, "fill_float64_bigarray_bulk_vs_scalar")

let () =
  Printf.printf "# fill_float64_bigarray vs %d individual uniform_float draws\n"
    n;
  Bench_common.run_and_print name tests
