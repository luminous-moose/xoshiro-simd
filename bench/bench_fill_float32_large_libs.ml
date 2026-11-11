let () = Printexc.record_backtrace true

(* 10 * 1024 * 1024 float32s = 40 MiB per array, far larger than any L1 or L2
   cache and larger than most (not all) L3 caches, so on those CPUs this
   measures streaming throughput where bench_fill_float32_libs (1024 elements)
   measures cache-resident throughput plus per-call overhead. *)
let n = 10 * 1024 * 1024
let available () = true

let tests, name =
  let xs_state = Xoshiro_simd.State.init ~backend:Auto () in
  let xs_ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n in
  let owl_ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n in
  let owl_ga = Bigarray.genarray_of_array1 owl_ba in
  let (common : Bench_common.test list) =
    [
      (* Here, we use Owl's uniform_ instead of uniform, because the former does
         not allocate and the latter does. xoshiro-simd fills into a
         preallocated buffer, so for the test to be fair, Owl must do the same.
         Owl's uniform_ ?a ?b ~out has no trailing unlabelled argument, so
         omitting ?a/?b leaves the application partial. Passing ?a:None ?b:None
         makes the call total while still using uniform_'s [0,1) defaults. *)
      {
        name = "owl";
        available;
        test =
          (fun () ->
            Sys.opaque_identity
              (Owl.Dense.Ndarray.S.uniform_ ?a:None ?b:None ~out:owl_ga));
      };
      {
        name = Bench_backend.auto_name xs_state;
        available;
        test =
          (fun () ->
            Sys.opaque_identity
              (Xoshiro_simd.State.fill_float32_bigarray xs_state xs_ba));
      };
    ]
  in
  (common, "fill_float32_bigarray_large")

let () =
  Printf.printf "# fill_float32_bigarray_large %d\n" n;
  Bench_common.run_and_print name tests
