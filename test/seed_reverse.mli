(* Given a desired lane-0 state (s0..s3, in that order), returns a 32-byte
   string that can be passed to Xoshiro_simd.State.init_string to produce
   exactly that state. *)
val seed_string_for_state : int64 array -> string
