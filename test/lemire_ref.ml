external attempt_raw : int -> int32 -> int
  = "xoshiro_simd_test_lemire_reference_attempt"
[@@noalloc]

let attempt ~bound ~x =
  let v = attempt_raw bound x in
  if v = -1 then None else Some v
