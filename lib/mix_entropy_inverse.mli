(* Given a xoshiro256 state in the form of four int64 words, compute an array
   that would produce that state when run through the mix_entropy function in
   xs256_common.c. We can use that array to produce a string for the string
   init function. *)
val compute : int64 array -> int64 array
