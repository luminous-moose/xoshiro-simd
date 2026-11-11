(* Run only by check_urandom_fallback.sh, with getrandom_enosys_shim.so
   preloaded, so every OS-entropy read below has to come from /dev/urandom.
   Each check is one that a broken fallback would fail: an exception (the read
   failed), or two states that agree (the read returned the same bytes twice,
   e.g. a buffer left unfilled). Two independent 64-bit draws agree with
   probability 2^-64, so a false failure is not a practical concern. *)

module S = Xoshiro_simd.State

let differ a b = not (Int64.equal (S.bits64 a) (S.bits64 b))

let () =
  (* State.init: two OS-seeded states must differ. *)
  let a = S.init () and b = S.init () in
  assert (differ a b);
  (* State.reseed: compare against a copy rebuilt from the pre-reseed state. If
     the reseed read failed it would raise; if it wrote nothing, the copy and
     the reseeded state would still agree. *)
  let c = S.init () in
  let copy =
    S.init_string ~backend:(S.state_backend c) (S.recover_seed_bytes c)
  in
  S.reseed c;
  assert (differ c copy);
  (* The module-level generator: self_init and reseed both read entropy. There
     is no public way to copy the module-level state, so all this can check is
     that neither read raises. *)
  Xoshiro_simd.self_init ();
  Xoshiro_simd.reseed ();
  ignore (Sys.opaque_identity (Xoshiro_simd.bits64 ()));
  print_string "ok"
