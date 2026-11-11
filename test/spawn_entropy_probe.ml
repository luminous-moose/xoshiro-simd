(* Run only by check_spawn_entropy_reads.sh, with getrandom_enosys_shim.so
   preloaded so every OS-entropy read is logged. OCaml 5 only (see test/dune).

   The README says spawned domains read no entropy: their generators are split
   from their parent's. So the initial domain's one read, from its first use
   below, should be the only one, however many domains are spawned. The script
   runs this with 0 spawns and with several, children spawning grandchildren,
   and compares the counts, so any reads the runtime itself makes cancel out. *)

let spawns = int_of_string Sys.argv.(1)
let draw () = ignore (Sys.opaque_identity (Xoshiro_simd.bits64 ()))

let child () =
  draw ();
  Domain.join (Domain.spawn draw);
  draw ()

let () =
  draw ();
  List.iter Domain.join (List.init spawns (fun _ -> Domain.spawn child));
  print_string "ok"
