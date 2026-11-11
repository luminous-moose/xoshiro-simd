let () = Printexc.record_backtrace true
let n = 1024
let min = -0.5
let max = 0.5
let available () = true

let tests, name =
  let xs_state = Xoshiro_simd.State.init ~backend:Auto () in
  let xs_ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n in
  let owl_ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n in
  let owl_ga = Bigarray.genarray_of_array1 owl_ba in
  let (common : Bench_common.test list) =
    [
      {
        name = "owl";
        available;
        test =
          (fun () ->
            Sys.opaque_identity
              (Owl.Dense.Ndarray.S.uniform_ ~a:min ~b:max ~out:owl_ga));
      };
      {
        name = Bench_backend.auto_name xs_state;
        available;
        test =
          (fun () ->
            Sys.opaque_identity
              (Xoshiro_simd.State.fill_float32_range_bigarray xs_state xs_ba
                 ~min ~max));
      };
    ]
  in
  (common, "fill_float32_range_bigarray")

let () =
  Printf.printf "# fill_float32_range_bigarray %d [%g, %g]\n" n min max;
  Bench_common.run_and_print name tests
