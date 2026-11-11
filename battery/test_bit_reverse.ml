(* Checks xoshiro_bitreverse64 (bit_reverse.c) against a bit-by-bit reference
   written independently here, so the table-and-bswap version can be edited
   safely. Exits 1 on the first mismatch. *)

external bitreverse64 : (int64[@unboxed]) -> (int64[@unboxed])
  = "xoshiro_battery_test_bitreverse64_byte" "xoshiro_battery_test_bitreverse64"
[@@noalloc]

let reference w =
  let r = ref 0L in
  for i = 0 to 63 do
    if Int64.logand (Int64.shift_right_logical w i) 1L <> 0L then
      r := Int64.logor !r (Int64.shift_left 1L (63 - i))
  done;
  !r

let failures = ref 0

let check what w =
  let got = bitreverse64 w in
  let want = reference w in
  if got <> want then begin
    incr failures;
    Printf.printf "FAIL %s: input %016Lx, got %016Lx, expected %016Lx\n" what w
      got want
  end;
  let back = bitreverse64 got in
  if back <> w then begin
    incr failures;
    Printf.printf "FAIL %s: reversing twice gave %016Lx, expected %016Lx\n" what
      back w
  end

(* A local splitmix64, so the inputs don't depend on the generator under test
   or on the Stdlib.Random version. *)
let splitmix64 state =
  state := Int64.add !state 0x9E3779B97F4A7C15L;
  let z = !state in
  let z =
    Int64.mul
      (Int64.logxor z (Int64.shift_right_logical z 30))
      0xBF58476D1CE4E5B9L
  in
  let z =
    Int64.mul
      (Int64.logxor z (Int64.shift_right_logical z 27))
      0x94D049BB133111EBL
  in
  Int64.logxor z (Int64.shift_right_logical z 31)

let () =
  check "zero" 0L;
  check "all ones" (-1L);
  for i = 0 to 63 do
    check (Printf.sprintf "bit %d" i) (Int64.shift_left 1L i)
  done;
  (* Distinct values in every byte, so a byte landing in the wrong position
     can't go unnoticed. *)
  check "byte pattern" 0x0102040810204080L;
  check "byte pattern" 0x0123456789ABCDEFL;
  let state = ref 42L in
  for _ = 1 to 200_000 do
    check "random" (splitmix64 state)
  done;
  if !failures > 0 then begin
    Printf.printf "%d failure(s)\n" !failures;
    exit 1
  end
