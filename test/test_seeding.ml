open Base
open! Stdio
module Xs = Xoshiro_simd
module Xs_S = Xoshiro_simd.State

(* Exercises init_string/init_int64array's seeding path (setup_hash's
   mix_entropy, see xs256_common.c) directly against the internal state it
   produces, via for_testing_raw_dump -- as opposed to test_vectors.ml,
   which only checks output streams. In particular: does every bit of the
   seed material actually influence the resulting state (an "avalanche"
   property), does material past mix_entropy's first 4 words still get
   folded in (Pass 3), and do the string and int-array paths agree when fed
   the same underlying bytes. *)

let popcount64 n =
  let rec loop n acc =
    if Int64.equal n 0L then acc
    else
      loop
        (Int64.shift_right_logical n 1)
        (acc + Int64.to_int_trunc (Int64.bit_and n 1L))
  in
  loop n 0

let hamming_distance a b =
  Array.fold2_exn a b ~init:0 ~f:(fun acc x y ->
      acc + popcount64 (Int64.bit_xor x y))

(* Lane 0's four state words (s0..s3): the direct output of mix_entropy,
   before jump derives lanes 1-3 from them. xs256_common.h's s array is
   words-major (word w, lane l at s.(w*4+l)), so lane 0 is index w*4 for
   w = 0..3. This isolates the seeding/mixing step from jump's own
   (separately-tested-elsewhere) diffusion. *)
let lane0_words (dump : Xs_S.raw_dump) =
  [| dump.s.(0); dump.s.(4); dump.s.(8); dump.s.(12) |]

let dump_lane0 state = lane0_words (Xs_S.for_testing_raw_dump state)
let dump_lane0_f init seed = dump_lane0 (init seed)

let flip_byte_bit bytes byte_idx bit_in_byte =
  let out = Bytes.copy bytes in
  let b = Char.to_int (Bytes.get bytes byte_idx) in
  Bytes.set out byte_idx (Char.of_int_exn (b lxor (1 lsl bit_in_byte)));
  out

(* bit_idx ranges over every bit of the int64 element (0..63). Each element
   is already a full 64-bit word, so unlike a native-int seed there's no
   platform-dependent width to worry about here: bit 63 is independently
   settable, and identically so on 32-bit and 64-bit hosts. *)
let flip_int_bit arr elem_idx bit_idx =
  let out = Array.copy arr in
  out.(elem_idx) <- Int64.(bit_xor out.(elem_idx) (shift_left 1L bit_idx));
  out

(* Mirrors store_seed_word_le64's byte layout in OCaml, to build a byte
   string equivalent to what an int64 array seed would serialize to. *)
let le64_seed_bytes arr =
  let n = Array.length arr in
  let buf = Bytes.create (n * 8) in
  Array.iteri arr ~f:(fun idx v64 ->
      Stdlib.Bytes.set_int64_le buf (idx * 8) v64);
  buf

(* --- Determinism --- *)

let%test_unit "init_string: same seed produces the same full state" =
  let d1 = Xs_S.for_testing_raw_dump (Xs_S.init_string "hello world") in
  let d2 = Xs_S.for_testing_raw_dump (Xs_S.init_string "hello world") in
  assert (Array.equal Int64.equal d1.s d2.s)

let%test_unit "init_int64array: same seed produces the same full state" =
  let d1 = Xs_S.for_testing_raw_dump (Xs_S.init_int64array [| 1L; 2L; 3L |]) in
  let d2 = Xs_S.for_testing_raw_dump (Xs_S.init_int64array [| 1L; 2L; 3L |]) in
  assert (Array.equal Int64.equal d1.s d2.s)

(* --- Empty seeds are rejected --- *)

let%test "init_string raises on empty seed" =
  try
    ignore (Xs_S.init_string "");
    false
  with Invalid_argument _ -> true

let%test "init_int64array raises on empty seed" =
  try
    ignore (Xs_S.init_int64array [||]);
    false
  with Invalid_argument _ -> true

(* --- Avalanche: every input bit should influence lane 0's state --- *)

(* True avalanche (mix_entropy is a good mixing function) gives an expected
   Hamming distance of 128/256 with a standard deviation around 8 (Binomial
   (256, 0.5)). 32 is comfortably far below that (12 standard deviations) to
   never flake on a correctly-mixing generator, while still being far above
   what a "technically nonzero but barely mixed" bug would produce -- e.g.
   every bug found and fixed in this project's own seeding work would have
   produced a Hamming distance of 0 for large swaths of bit positions, not
   something merely a bit low. *)
let min_hamming_distance = 32

let%test_unit "init_string: every input bit avalanches into lane 0's state" =
  let baseline_bytes = Bytes.make 32 '\000' in
  let baseline =
    dump_lane0_f (fun b -> Xs_S.init_string (Bytes.to_string b)) baseline_bytes
  in
  for bit = 0 to 255 do
    let byte_idx = bit / 8 and bit_in_byte = bit % 8 in
    let flipped_bytes = flip_byte_bit baseline_bytes byte_idx bit_in_byte in
    let flipped =
      dump_lane0_f (fun b -> Xs_S.init_string (Bytes.to_string b)) flipped_bytes
    in
    let d = hamming_distance baseline flipped in
    if d < min_hamming_distance then
      failwith
        (Printf.sprintf
           "string seed, bit %d: only %d/256 state bits changed (expected >= \
            %d)"
           bit d min_hamming_distance)
  done

let%test_unit "init_int64array: every input bit avalanches into lane 0's state"
    =
  let baseline_arr = Array.create ~len:4 0L in
  let baseline = dump_lane0_f Xs_S.init_int64array baseline_arr in
  for elem = 0 to 3 do
    for bit = 0 to 63 do
      let flipped_arr = flip_int_bit baseline_arr elem bit in
      let flipped = dump_lane0_f Xs_S.init_int64array flipped_arr in
      let d = hamming_distance baseline flipped in
      if d < min_hamming_distance then
        failwith
          (Printf.sprintf
             "int array seed, elem %d bit %d: only %d/256 state bits changed \
              (expected >= %d)"
             elem bit d min_hamming_distance)
    done
  done

(* --- Full state after seeding, human-readable --- *)

(* One representative expect test per deterministic init function, so a
   reviewer has a concrete, literal point of comparison for what these
   functions actually produce (unlike the avalanche/bijection tests above,
   which check properties of the mapping without ever printing it).

   init_int64array takes an int64 array (rather than a plain int array) for
   exactly this reason: an int64 element is always a full, boxed 64-bit
   word regardless of host word size, so unlike a native-int seed there's
   no host-width caveat to keep this test's hardcoded output compiling
   and matching on 32-bit hosts.

   Alongside each function's raw internal state, also print a run of 16
   bits64 draws -- the actual output stream a caller would see, as
   opposed to the internals for_testing_raw_dump exposes. *)

let print_lane0 label words =
  printf "%s:\n" label;
  Array.iter words ~f:(fun w -> printf "  0x%016Lx\n" w)

let print_lanes label (dump : Xs_S.raw_dump) =
  printf "%s:\n" label;
  for lane = 0 to 3 do
    printf "  lane %d:\n" lane;
    for w = 0 to 3 do
      printf "    0x%016Lx\n" dump.s.((w * 4) + lane)
    done
  done

let print_draws state n =
  for _ = 1 to n do
    printf "  0x%016Lx\n" (Xs_S.bits64 state)
  done

let%expect_test "init_deterministic: full state after seeding" =
  let state = Xs_S.init_deterministic 123456789L in
  print_lanes "state" (Xs_S.for_testing_raw_dump state);
  [%expect
    {|
    state:
      lane 0:
        0x223c74d93deb7679
        0x7a91dd183971ee2e
        0x310e0831409afde5
        0x851e061616a5bee5
      lane 1:
        0xbc3bc23fffbafa5e
        0xdda6d64294493e0b
        0x85f290ff67e440f8
        0x669a72f15a0ba872
      lane 2:
        0xf3f3d9f9e71dae6f
        0xb5e28f01b8502d0d
        0x785f49a9087c3927
        0xd9570aedc49fecc7
      lane 3:
        0x9c198381bc404806
        0xd93110b23914e908
        0xab887c9a998fbe16
        0x9764cd9b10283ee8
    |}];
  printf "16 bits64 draws:\n";
  print_draws state 16;
  [%expect
    {|
    16 bits64 draws:
      0x99e6bd73ed3f23b6
      0x54e8a59167cc6578
      0x67c9b8c7828453e1
      0x2a7fb7c5335a072e
      0xc23a804d68730d49
      0x00eaadff29a55775
      0x756ca4153b1e2f79
      0x0bf4ef8b1c33441a
      0x650e013620979041
      0xd087d08bd2f23b88
      0x29e46a7e31ce50cc
      0x517820589639522c
      0x6f44f98493c7f9c3
      0x816635a6a35c9c40
      0xe168bcc43ba06055
      0x5b23f155e81c6f2c
    |}]

let%expect_test "init_string: full state after seeding" =
  let state = Xs_S.init_string "hello world" in
  print_lanes "state" (Xs_S.for_testing_raw_dump state);
  [%expect
    {|
    state:
      lane 0:
        0xc47c6e40a43f6c51
        0x90d6d6dc510459ab
        0xb2b2e3f0b3ae9e0a
        0x547c518dab5bbd89
      lane 1:
        0xdc313ccf14458da9
        0x858b3393c447ba68
        0x743ad7ac9b3b0d27
        0xa3c739db8348ebbc
      lane 2:
        0xb81f17597a25de7e
        0xd7e88c46aa541bbd
        0x93811bdc8f96a741
        0x63de113e01304500
      lane 3:
        0x1c4ba7ba773f0d45
        0x5c3107099f939787
        0xf2c3317b0d88feee
        0x6e42351d272a8013
    |}];
  printf "16 bits64 draws:\n";
  print_draws state 16;
  [%expect
    {|
    16 bits64 draws:
      0xaba43bd5914be8b0
      0x317d040bc70589e4
      0x03dcc26b3933dd12
      0x881adc8123845433
      0xd42e8e753ddf3613
      0xa2e7807a02bd385a
      0x607c026ce90d8163
      0x2cd37a07c3aeb05e
      0x382f3a0813a58def
      0x18b9cbd27d5cf2ab
      0x27caa5b8b84f3a72
      0xd74b6b05782f5764
      0x079f275a289cd3e6
      0xf58626f5479851e2
      0xc0eebb84cefca9e7
      0x1494dc0d97cf8fa8
    |}]

let%expect_test "init_int64array: full state after seeding" =
  let state = Xs_S.init_int64array [| 1L; 2L; 3L; 4L; 5L |] in
  print_lanes "state" (Xs_S.for_testing_raw_dump state);
  [%expect
    {|
    state:
      lane 0:
        0xcfce8afa38b53ee5
        0x9300e12a251f99a4
        0xbeede713051d7e2d
        0x15ed5ccc7cd611bc
      lane 1:
        0x0840a3243f3819de
        0x1152bacba549e670
        0x1a14e4b2f328bb95
        0x7e995f7706239e02
      lane 2:
        0xbb3f816aeb78f6b3
        0xfb3f0649fc2a2ec9
        0xfced324806e5df18
        0x8274bd7150610daa
      lane 3:
        0x9c37b396903eee56
        0xd67053dd13b0b625
        0xc623f2555f302472
        0x87e0b644f8bb4b42
    |}];
  printf "16 bits64 draws:\n";
  print_draws state 16;
  [%expect
    {|
    16 bits64 draws:
      0xb32950a289a81cd8
      0x55e351002f7b86df
      0x295d6e6d1a17d0d2
      0x89fc30b35c50fa8a
      0x462fd3dd7cb9ca20
      0xb097cb93a92dae76
      0x805387c7f3c74624
      0xee734a37c45bbd51
      0x4d74a8c01375cbda
      0x0a53ded6e53f7959
      0xab24761a2c7270e8
      0x8cdccd18e4aba037
      0x3be663f4fddf28de
      0x45c4a775f6bd6e7d
      0x4dca65865472a55b
      0xe312d8c2ffab57de
    |}]

let%expect_test "init_int64array: full state after seeding, single int" =
  let state = Xs_S.init_int64array [| 1L |] in
  print_lanes "state" (Xs_S.for_testing_raw_dump state);
  [%expect
    {|
    state:
      lane 0:
        0x95dfe02b61750da6
        0x274e03c3dad5772b
        0xe1c72c7f226db6fc
        0xcaf4b66b87262904
      lane 1:
        0xb29a476a076caa14
        0x89d42a552e574912
        0xaac56af17555b98b
        0x4ce8035b333749a7
      lane 2:
        0x7f89cf116f99728d
        0x944f8f90c53b2ff2
        0xfa9fe4ffdeb02c9c
        0x392d45d79d1c3a2e
      lane 3:
        0xae498ae30f0e6dec
        0xf0c222db0c3cf6ad
        0x0eed4c53e22dd16d
        0x6fa1ebf5e1ef61e3
    |}];
  printf "16 bits64 draws:\n";
  print_draws state 16;
  [%expect
    {|
    16 bits64 draws:
      0xe1542dc6b6a577f1
      0x15379963e56c6b39
      0xf41029e7cd75ce17
      0x1ac209caf69d63a7
      0x15aef332e0786932
      0xfd76059b40c88934
      0x543388ff62993a9e
      0x0e5845ba113004ad
      0x9c2388fb0eab8c6d
      0x86f96945182fe381
      0xa139c425cb9f30cf
      0x61d6607a989ab950
      0xb3c292b70753cf90
      0x30740f5878df58bd
      0xfde997100686a9a3
      0xea2573ac124f07ae
    |}]

let%expect_test "single-bit flips visibly change lane 0's state (string seed)" =
  let baseline_bytes = Bytes.make 32 '\000' in
  let baseline =
    dump_lane0_f (fun b -> Xs_S.init_string (Bytes.to_string b)) baseline_bytes
  in
  print_lane0 "baseline (32 zero bytes)" baseline;
  [%expect
    {|
    baseline (32 zero bytes):
      0x9e65e73453fe0a31
      0x0b98298a29108a7e
      0x4f99944d87ff24bc
      0xa2de3efb35408b9d
    |}];
  List.iter [ 0; 1; 127; 255 ] ~f:(fun bit ->
      let byte_idx = bit / 8 and bit_in_byte = bit % 8 in
      let flipped_bytes = flip_byte_bit baseline_bytes byte_idx bit_in_byte in
      let flipped =
        dump_lane0_f
          (fun b -> Xs_S.init_string (Bytes.to_string b))
          flipped_bytes
      in
      print_lane0 (Printf.sprintf "bit %d flipped" bit) flipped;
      printf "hamming distance from baseline: %d/256\n"
        (hamming_distance baseline flipped));
  [%expect
    {|
    bit 0 flipped:
      0x95dfe02b61750da6
      0x274e03c3dad5772b
      0xe1c72c7f226db6fc
      0xcaf4b66b87262904
    hamming distance from baseline: 119/256
    bit 1 flipped:
      0x2530f6561f59e7fe
      0x754cade074f9c426
      0xa269370b7010f169
      0xc97294847dbf2ebf
    hamming distance from baseline: 145/256
    bit 127 flipped:
      0x167f8343ec83accc
      0x1271358fa7f719fe
      0x465679f42ab6efc8
      0xa7c2507ba8cb14d9
    hamming distance from baseline: 130/256
    bit 255 flipped:
      0xd956b0fc1dbc7530
      0x31b1446cb0577751
      0xfa9ccf5e721d77c2
      0xfe850da25bbf2ea7
    hamming distance from baseline: 140/256
    |}]

let%expect_test
    "single-bit flips visibly change lane 0's state (int array seed)" =
  let baseline_arr = Array.create ~len:4 0L in
  let baseline = dump_lane0_f Xs_S.init_int64array baseline_arr in
  print_lane0 "baseline ([|0;0;0;0|])" baseline;
  [%expect
    {|
    baseline ([|0;0;0;0|]):
      0x9e65e73453fe0a31
      0x0b98298a29108a7e
      0x4f99944d87ff24bc
      0xa2de3efb35408b9d
    |}];
  (* Every element is a full 64-bit int64, so unlike a native-int seed, no
     particular bit (including bit 63) is a platform-dependent sign bit
     here -- any bit is equally safe to demonstrate and stays
     platform-independent for this %expect_test's literal output. *)
  List.iter
    [ (0, 0); (0, 5); (3, 0); (3, 63) ]
    ~f:(fun (elem, bit) ->
      let flipped_arr = flip_int_bit baseline_arr elem bit in
      let flipped = dump_lane0_f Xs_S.init_int64array flipped_arr in
      print_lane0 (Printf.sprintf "elem %d bit %d flipped" elem bit) flipped;
      printf "hamming distance from baseline: %d/256\n"
        (hamming_distance baseline flipped));
  [%expect
    {|
    elem 0 bit 0 flipped:
      0x95dfe02b61750da6
      0x274e03c3dad5772b
      0xe1c72c7f226db6fc
      0xcaf4b66b87262904
    hamming distance from baseline: 119/256
    elem 0 bit 5 flipped:
      0x34b7a6f3f414b3e1
      0xbbcc626eab49c019
      0x45b041927a2f5018
      0x745260282571ce67
    hamming distance from baseline: 126/256
    elem 3 bit 0 flipped:
      0x42a78518926e5f04
      0x3e460958eb88aca7
      0x4ace805f0046afd1
      0xe16b1db932ef0b49
    hamming distance from baseline: 112/256
    elem 3 bit 63 flipped:
      0xd956b0fc1dbc7530
      0x31b1446cb0577751
      0xfa9ccf5e721d77c2
      0xfe850da25bbf2ea7
    hamming distance from baseline: 140/256
    |}]

(* --- Pass 3: material past mix_entropy's first 4 words still affects state --- *)

let%test_unit "init_string: bytes past the first 32 still affect the state" =
  let make last_byte =
    let b = Bytes.make 40 '\000' in
    Bytes.set b 39 (Char.of_int_exn last_byte);
    Bytes.to_string b
  in
  let d0 = (Xs_S.for_testing_raw_dump (Xs_S.init_string (make 0))).s in
  let d1 = (Xs_S.for_testing_raw_dump (Xs_S.init_string (make 1))).s in
  assert (not (Array.equal Int64.equal d0 d1))

let%test_unit
    "init_int64array: elements past the first 4 still affect the state" =
  let make last = [| 0L; 0L; 0L; 0L; last |] in
  let d0 = (Xs_S.for_testing_raw_dump (Xs_S.init_int64array (make 0L))).s in
  let d1 = (Xs_S.for_testing_raw_dump (Xs_S.init_int64array (make 1L))).s in
  assert (not (Array.equal Int64.equal d0 d1))

(* Zero padding up to 4 words (32 bytes) gives the same state, but a fifth word
   changes it even when it is zero: every word past the fourth is mixed in,
   whatever its value. init_string zero-pads its last 8-byte word, so trailing
   NULs only matter when they start a new word past the fourth. *)
let%expect_test "trailing zeros change the state only past the fourth word" =
  let lane0 state =
    Array.sub (Xs_S.for_testing_raw_dump state).s ~pos:0 ~len:4
  in
  let show name state =
    printf "%-14s" name;
    Array.iter (lane0 state) ~f:(fun w -> printf " %016Lx" w);
    printf "\n"
  in
  show "[1]" (Xs_S.init_int64array [| 1L |]);
  show "[1;0;0;0]" (Xs_S.init_int64array [| 1L; 0L; 0L; 0L |]);
  show "[1;0;0;0;0]" (Xs_S.init_int64array [| 1L; 0L; 0L; 0L; 0L |]);
  show "[1;0;0;0;0;0]" (Xs_S.init_int64array [| 1L; 0L; 0L; 0L; 0L; 0L |]);
  let x n = String.make n 'x' and z n = String.make n '\000' in
  show "x" (Xs_S.init_string (x 1));
  show "x+31 NUL" (Xs_S.init_string (x 1 ^ z 31));
  show "33x" (Xs_S.init_string (x 33));
  show "33x+7 NUL" (Xs_S.init_string (x 33 ^ z 7));
  show "33x+8 NUL" (Xs_S.init_string (x 33 ^ z 8));
  [%expect
    {|
    [1]            95dfe02b61750da6 b29a476a076caa14 7f89cf116f99728d ae498ae30f0e6dec
    [1;0;0;0]      95dfe02b61750da6 b29a476a076caa14 7f89cf116f99728d ae498ae30f0e6dec
    [1;0;0;0;0]    74fa3064dcaf7a64 960b96132f6d2f21 275c69fc38fd5fb0 795c96e7222a3108
    [1;0;0;0;0;0]  758e03117c9c5257 d2f772822df7ba10 52b4203d96328c1d 39e2acc28264116f
    x              31c98ec9467ecefd 1a95e90bcbb6613f f543d29a33731024 03526104de791bf2
    x+31 NUL       31c98ec9467ecefd 1a95e90bcbb6613f f543d29a33731024 03526104de791bf2
    33x            b221fd725af09bc6 8673249edefca648 4659486add7d59ea dbc5551290e8074b
    33x+7 NUL      b221fd725af09bc6 8673249edefca648 4659486add7d59ea dbc5551290e8074b
    33x+8 NUL      c53f3a233977213b eac856f56a00f26b 5b82da73f99711f6 9b8b45893a4a71eb
    |}]

(* --- String and int-array seeding agree on equivalent byte sequences --- *)

let%test_unit
    "init_int64array and init_string agree on equivalent byte sequences" =
  let arr = [| 1L; -2L; 12345L; 0L |] in
  let equivalent_bytes = le64_seed_bytes arr in
  let from_array = (Xs_S.for_testing_raw_dump (Xs_S.init_int64array arr)).s in
  let from_string =
    (Xs_S.for_testing_raw_dump
       (Xs_S.init_string (Bytes.to_string equivalent_bytes)))
      .s
  in
  assert (Array.equal Int64.equal from_array from_string)

(* --- mix_entropy is a bijection on exactly-256-bit inputs --- *)

(* recover_seed_bytes relies on mix_entropy being a bijection for 4-word
   (256-bit) inputs: every state has exactly one 32-byte preimage, so
   recovering it and getting back anything other than the original seed
   bytes would mean mix_entropy either isn't injective or recover_seed_bytes
   doesn't actually invert it. This demonstrates the real, production C
   mix_entropy is still lossless when exercised via the real code path end
   to end: seed through init_string (32-byte inputs) or init_int64array
   (4 int64 elements -- both give every one of the 256 input bits independent
   control), then confirm recover_seed_bytes reconstructs exactly the
   original seed bytes. *)

let%test_unit
    "init_string: recover_seed_bytes round-trips on random 32-byte seeds" =
  let rand_state = Stdlib.Random.State.make [| 0xc0ffee |] in
  for _ = 1 to 5000 do
    let words =
      Array.init 4 ~f:(fun _ -> Stdlib.Random.State.bits64 rand_state)
    in
    let seed_bytes = le64_seed_bytes words in
    let state = Xs_S.init_string (Bytes.to_string seed_bytes) in
    let recovered = Xs_S.recover_seed_bytes state in
    assert (String.equal recovered (Bytes.to_string seed_bytes))
  done

let%test_unit
    "init_int64array: recover_seed_bytes round-trips on random 4-int64 seeds" =
  let rand_state = Stdlib.Random.State.make [| 0xc0ffee + 1 |] in
  for _ = 1 to 5000 do
    let arr =
      Array.init 4 ~f:(fun _ -> Stdlib.Random.State.bits64 rand_state)
    in
    let state = Xs_S.init_int64array arr in
    let recovered = Xs_S.recover_seed_bytes state in
    assert (String.equal recovered (Bytes.to_string (le64_seed_bytes arr)))
  done

(* Seeds shorter than 32 bytes (or 4 words) are zero-padded before mixing, so
   the recovered bytes are the seed followed by zeros, and appending zero bytes
   (or words) up to that length gives the same state. *)

let%test_unit
    "init_string: recover_seed_bytes on a short seed is the seed padded with \
     zeros" =
  let rand_state = Stdlib.Random.State.make [| 0xc0ffee + 2 |] in
  for len = 1 to 32 do
    let seed =
      String.init len ~f:(fun _ ->
          Char.of_int_exn (32 + Stdlib.Random.State.int rand_state 95))
    in
    let padded = seed ^ String.make (32 - len) '\000' in
    let recovered = Xs_S.recover_seed_bytes (Xs_S.init_string seed) in
    if not (String.equal recovered padded) then
      Printf.failwithf "length %d: recovered %S, expected %S" len recovered
        padded ()
  done

let%expect_test "recover_seed_bytes on a short string seed" =
  let recovered = Xs_S.recover_seed_bytes (Xs_S.init_string "hello") in
  printf "%S\n" recovered;
  [%expect
    {| "hello\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000" |}]

let%test_unit
    "init_int64array: recover_seed_bytes on a short seed is the seed padded \
     with zero words" =
  let rand_state = Stdlib.Random.State.make [| 0xc0ffee + 3 |] in
  for len = 1 to 4 do
    let arr =
      Array.init len ~f:(fun _ -> Stdlib.Random.State.bits64 rand_state)
    in
    let padded = Array.append arr (Array.create ~len:(4 - len) 0L) in
    let recovered = Xs_S.recover_seed_bytes (Xs_S.init_int64array arr) in
    if not (String.equal recovered (Bytes.to_string (le64_seed_bytes padded)))
    then Printf.failwithf "length %d: recovered %S" len recovered ()
  done

(* --- The all-zero state is never built --- *)

(* Exactly one 32-byte seed mixes to the all-zero state, the degenerate state
   that outputs zeros forever. Seeding from it gives the same state as
   init_deterministic 42L instead. *)
let zero_state_seed_words = [| 0L; 0L; 0L; 0L |]

let draws state = Array.init 16 ~f:(fun _ -> Xs_S.bits64 state)

let%test "init_string: the seed that mixes to all zeros gives the 42L state" =
  let seed = Seed_reverse.seed_string_for_state zero_state_seed_words in
  List.for_all (Xs.available_backends ()) ~f:(fun backend ->
      let got = Xs_S.init_string ~backend seed in
      let expected = Xs_S.init_deterministic ~backend 42L in
      Array.equal Int64.equal (Xs_S.for_testing_raw_dump got).s
        (Xs_S.for_testing_raw_dump expected).s
      && Array.equal Int64.equal (draws got) (draws expected))

let%test "init_int64array: the seed that mixes to all zeros gives the 42L state"
    =
  let seed = Xs.for_testing_mix_entropy_inverse zero_state_seed_words in
  List.for_all (Xs.available_backends ()) ~f:(fun backend ->
      let got = Xs_S.init_int64array ~backend seed in
      let expected = Xs_S.init_deterministic ~backend 42L in
      Array.equal Int64.equal (Xs_S.for_testing_raw_dump got).s
        (Xs_S.for_testing_raw_dump expected).s
      && Array.equal Int64.equal (draws got) (draws expected))

let%test_unit "a seed next to the all-zero one gives its own state" =
  let seed = Seed_reverse.seed_string_for_state [| 0L; 0L; 0L; 1L |] in
  let got = draws (Xs_S.init_string seed) in
  assert (
    not (Array.equal Int64.equal got (draws (Xs_S.init_deterministic 42L))));
  assert (Array.exists got ~f:(fun w -> not (Int64.equal w 0L)))

let%test_unit "module-level init_string and init_int64array take the same path"
    =
  let seed = Seed_reverse.seed_string_for_state zero_state_seed_words in
  let expected = draws (Xs_S.init_deterministic 42L) in
  Xs.init_string seed;
  assert (
    Array.equal Int64.equal (Array.init 16 ~f:(fun _ -> Xs.bits64 ())) expected);
  Xs.init_int64array (Xs.for_testing_mix_entropy_inverse zero_state_seed_words);
  assert (
    Array.equal Int64.equal (Array.init 16 ~f:(fun _ -> Xs.bits64 ())) expected)

(* --- Single bit seeding and recovery --- *)

let%expect_test "single-bit state after one round, 0" =
  let desired_state = [| 1L; 0L; 0L; 0L |] in
  let seed_string = Seed_reverse.seed_string_for_state desired_state in
  let state = Xs_S.init_string seed_string in
  let baseline = dump_lane0 state in
  print_lane0 "baseline ([|1;0;0;0|])" baseline;
  [%expect
    {|
    baseline ([|1;0;0;0|]):
      0x0000000000000001
      0x0000000000000000
      0x0000000000000000
      0x0000000000000000
    |}];
  for _ = 0 to 31 do
    let draw = Xs_S.bits64 state in
    ignore @@ Xs_S.bits64 state;
    ignore @@ Xs_S.bits64 state;
    ignore @@ Xs_S.bits64 state;
    printf "0x%016Lx  popcount: %d\n" draw (popcount64 draw)
  done;
  let current_state = dump_lane0 state in
  print_lane0 "state" current_state;
  printf "hamming distance from baseline: %d/256\n"
    (hamming_distance baseline current_state);
  [%expect
    {|
    0x0000000000800001  popcount: 2
    0x0000000000800001  popcount: 2
    0x0000000000000010  popcount: 1
    0x0002200000800021  popcount: 5
    0x0004010044220010  popcount: 7
    0x4000000080400884  popcount: 6
    0x0312894444a21019  popcount: 18
    0x420300a215c88005  popcount: 16
    0x139a898424b64e2a  popcount: 26
    0xcb574215914814a6  popcount: 26
    0x120a518894ce6c17  popcount: 25
    0x550240411a2d52c8  popcount: 21
    0x24df1af404244946  popcount: 26
    0x8d7eb599b5db3d4e  popcount: 39
    0x370e4109dd547475  popcount: 30
    0x99f85467c79954d4  popcount: 33
    0x7e28c99073d1d475  popcount: 32
    0x959e9017e15c4ecc  popcount: 31
    0x6570c9f538c834b5  popcount: 31
    0x1f015ede618aee70  popcount: 32
    0x15c25258204340ed  popcount: 23
    0xbf3b3414a37a5bd4  popcount: 35
    0xa2a650bfbb04985f  popcount: 32
    0x1aedfd386ce3325e  popcount: 36
    0xfee0a622d9fcb2b1  popcount: 35
    0x2f8dda34d2fad5e5  popcount: 37
    0x3d67b33f5a23f623  popcount: 37
    0x02ace5e35251b5c6  popcount: 30
    0x5ab059594dc9a4d8  popcount: 30
    0xf1f28c5d97a06591  popcount: 32
    0xde6d61f716bc9d65  popcount: 38
    0xff16f0866482622a  popcount: 29
    state:
      0xb434fecb30a76f15
      0x98a31540687f7fdf
      0x3d16722d88cde3ff
      0x8c00dd25fba97ee8
    hamming distance from baseline: 138/256
    |}]

let%expect_test "single-bit state after one round, s1" =
  let desired_state = [| 0L; 1L; 0L; 0L |] in
  let seed_string = Seed_reverse.seed_string_for_state desired_state in
  let state = Xs_S.init_string seed_string in
  let baseline = dump_lane0 state in
  print_lane0 "baseline ([|0;1;0;0|])" baseline;
  [%expect
    {|
    baseline ([|0;1;0;0|]):
      0x0000000000000000
      0x0000000000000001
      0x0000000000000000
      0x0000000000000000
    |}];
  for _ = 0 to 31 do
    let draw = Xs_S.bits64 state in
    ignore @@ Xs_S.bits64 state;
    ignore @@ Xs_S.bits64 state;
    ignore @@ Xs_S.bits64 state;
    printf "0x%016Lx  popcount: %d\n" draw (popcount64 draw)
  done;
  let current_state = dump_lane0 state in
  print_lane0 "state" current_state;
  printf "hamming distance from baseline: %d/256\n"
    (hamming_distance baseline current_state);
  [%expect
    {|
    0x0000000000000000  popcount: 0
    0x0000000000800011  popcount: 3
    0x0002200000000020  popcount: 3
    0x0004010044220000  popcount: 6
    0x4002200080c008a5  popcount: 11
    0x0312884400801009  popcount: 12
    0x020300a295888881  popcount: 16
    0x1088004060144e31  popcount: 15
    0x8d5842b7849094a3  popcount: 26
    0x0294da4c986c3235  popcount: 26
    0x1e554234ab75632e  popcount: 31
    0x3555499c98f22d5a  popcount: 31
    0xe07cb598a8026a86  popcount: 27
    0x16d12a5ddd74ad30  popcount: 32
    0xa4cee13e7b543d02  popcount: 32
    0xe77ab9eb218fb0df  popcount: 39
    0x2cee4bb29dd54200  popcount: 29
    0x3dd948ee4a88f4e7  popcount: 34
    0xa3eb5a7c22830ce8  popcount: 30
    0xca5306c31acbc891  popcount: 28
    0xfcd1fbff745c7579  popcount: 43
    0x13df12eccc974b47  popcount: 34
    0x27ba78e8cf173af8  popcount: 36
    0x1871ed902639805a  popcount: 26
    0x979e710bb65dd2bf  popcount: 38
    0x50ae314d82bab9dc  popcount: 31
    0x0ffc12209c4135f2  popcount: 28
    0xff9aabfc79901b64  popcount: 37
    0xf06ce1b988ecca3e  popcount: 33
    0x06465e14c9581c1e  popcount: 26
    0x7eecbcf452262695  popcount: 34
    0x626c27b383f02033  popcount: 28
    state:
      0xfd8c152e64926131
      0x19254f2e08e76f15
      0x6ebebc5030ba6006
      0x21ade95756ba4e5c
    hamming distance from baseline: 122/256
    |}]

let%expect_test "single-bit state after one round, s2" =
  let desired_state = [| 0L; 0L; 1L; 0L |] in
  let seed_string = Seed_reverse.seed_string_for_state desired_state in
  let state = Xs_S.init_string seed_string in
  let baseline = dump_lane0 state in
  print_lane0 "baseline ([|0;0;1;0|])" baseline;
  [%expect
    {|
    baseline ([|0;0;1;0|]):
      0x0000000000000000
      0x0000000000000000
      0x0000000000000001
      0x0000000000000000
    |}];
  for _ = 0 to 31 do
    let draw = Xs_S.bits64 state in
    ignore @@ Xs_S.bits64 state;
    ignore @@ Xs_S.bits64 state;
    ignore @@ Xs_S.bits64 state;
    printf "0x%016Lx  popcount: %d\n" draw (popcount64 draw)
  done;
  let current_state = dump_lane0 state in
  print_lane0 "state" current_state;
  printf "hamming distance from baseline: %d/256\n"
    (hamming_distance baseline current_state);
  [%expect
    {|
    0x0000000000000000  popcount: 0
    0x0000000000000000  popcount: 0
    0x0000000000800011  popcount: 3
    0x0002200000800011  popcount: 5
    0x0002210044a20011  popcount: 10
    0x4004010044220894  popcount: 11
    0x4312894444a20895  popcount: 20
    0x43108966512a9014  popcount: 21
    0x53988966112ece25  popcount: 28
    0xd9d0cb9195b64ec6  popcount: 33
    0xdbdd537e060e7092  popcount: 33
    0x46087159a6e73ebf  popcount: 34
    0xf8dd3ad51a4f170d  popcount: 35
    0x6d31be65b9bd7d13  popcount: 38
    0x82d0de1e882d5928  popcount: 27
    0xef1e7d2e15dcf500  popcount: 35
    0x469115cd2c6a5541  popcount: 27
    0x442b0f7022220ae1  popcount: 23
    0x6d632c9d59144a2a  popcount: 29
    0xbb01e1f5fa518b09  popcount: 32
    0xf1f9962bf1911ad0  popcount: 33
    0x6092f088356649f1  popcount: 27
    0xb72def8738adde1c  popcount: 38
    0xb0d6c5e4e6a1aa2b  popcount: 32
    0x88ec1d404e9ecd9e  popcount: 31
    0xde4a36578890491c  popcount: 28
    0xffe93f72916b40c4  popcount: 35
    0x40da637b8b175c12  popcount: 30
    0x3c1fe8c09579515e  popcount: 32
    0xd1524941008bccac  popcount: 24
    0x67357f9de43adc1e  popcount: 38
    0x984a5bfe667df685  popcount: 37
    state:
      0xe92fdd4bf7446006
      0x9007c3c8b08de3ff
      0xf00a9265ffa30f13
      0x6d8bd6e6d41f82ce
    hamming distance from baseline: 131/256
    |}]

let%expect_test "single-bit state after one round, s3" =
  let desired_state = [| 0L; 0L; 0L; 1L |] in
  let seed_string = Seed_reverse.seed_string_for_state desired_state in
  let state = Xs_S.init_string seed_string in
  let baseline = dump_lane0 state in
  print_lane0 "baseline ([|0;0;0;1|])" baseline;
  [%expect
    {|
    baseline ([|0;0;0;1|]):
      0x0000000000000000
      0x0000000000000000
      0x0000000000000000
      0x0000000000000001
    |}];
  for _ = 0 to 31 do
    let draw = Xs_S.bits64 state in
    ignore @@ Xs_S.bits64 state;
    ignore @@ Xs_S.bits64 state;
    ignore @@ Xs_S.bits64 state;
    printf "0x%016Lx  popcount: %d\n" draw (popcount64 draw)
  done;
  let current_state = dump_lane0 state in
  print_lane0 "state" current_state;
  printf "hamming distance from baseline: %d/256\n"
    (hamming_distance baseline current_state);
  [%expect
    {|
    0x0000000000800000  popcount: 1
    0x0000000000800011  popcount: 3
    0x0002200000800011  popcount: 5
    0x0002200044000020  popcount: 5
    0x00022000448008a1  popcount: 9
    0x01102100802208a0  popcount: 10
    0x0114002284200880  popcount: 10
    0x0402896644a64095  popcount: 20
    0x8a14a0a244acc891  popcount: 22
    0x8c88ba4680b20495  popcount: 23
    0x8b48b35586484291  popcount: 25
    0x09d8608086e66cd5  popcount: 26
    0x8a18c3530a2650dd  popcount: 26
    0x2ed4a357104b0701  popcount: 26
    0x2d1ea24818aa8d2e  popcount: 27
    0xeffefe088f4d905d  popcount: 38
    0xf6fed4519ca55d65  popcount: 37
    0xbda47de8850418d0  popcount: 28
    0xad083b925d1be96f  popcount: 34
    0x09d5cb7248ed60a4  popcount: 29
    0xc37f5b3460929267  popcount: 32
    0xdb6c9dd8dc922a5d  popcount: 35
    0xbb28601d718d218a  popcount: 27
    0xd4300a72a58268a4  popcount: 24
    0x3f9769f29f6cbee7  popcount: 42
    0x8cc9b63324b6049d  popcount: 29
    0xbcf060282a5800e0  popcount: 22
    0xf613ef363b2cc2d3  popcount: 36
    0x8051a717c9225084  popcount: 23
    0x0c1028b015996f0c  popcount: 23
    0xd00c366bb95f9f0b  popcount: 34
    0x608adf3df42a76ef  popcount: 37
    state:
      0x395e948853a55c55
      0xe1ba90da46423456
      0xc522c52ecdaf6abc
      0xca185233275718fa
    hamming distance from baseline: 123/256
    |}]

let%expect_test "several low bit state after one round" =
  let desired_state = [| 1L; 2L; 3L; 4L |] in
  let seed_string = Seed_reverse.seed_string_for_state desired_state in
  let state = Xs_S.init_string seed_string in
  let baseline = dump_lane0 state in
  print_lane0 "baseline ([|1;2;3;4|])" baseline;
  [%expect
    {|
    baseline ([|1;2;3;4|]):
      0x0000000000000001
      0x0000000000000002
      0x0000000000000003
      0x0000000000000004
    |}];
  for _ = 0 to 31 do
    let draw = Xs_S.bits64 state in
    ignore @@ Xs_S.bits64 state;
    ignore @@ Xs_S.bits64 state;
    ignore @@ Xs_S.bits64 state;
    printf "0x%016Lx  popcount: %d\n" draw (popcount64 draw)
  done;
  let current_state = dump_lane0 state in
  print_lane0 "state" current_state;
  printf "hamming distance from baseline: %d/256\n"
    (hamming_distance baseline current_state);
  [%expect
    {|
    0x0000000002800001  popcount: 3
    0x0000000003800067  popcount: 8
    0x000cc00003800067  popcount: 12
    0x000cc201994400b2  popcount: 16
    0x8012a2019ac433cd  popcount: 23
    0x8a69978acdee33ba  popcount: 35
    0xc271134733154abd  popcount: 30
    0xac2ba09179169e97  popcount: 31
    0xdbf3190a8f073fd8  popcount: 35
    0x9105f14ab2229220  popcount: 23
    0x69576e14f7917c67  popcount: 36
    0xe817e79337dffde4  popcount: 41
    0x27c420f2b27951f1  popcount: 30
    0x8d82d937547b908d  popcount: 31
    0x093db8420f72979e  popcount: 31
    0x26df21f24c914924  popcount: 28
    0xf9be116f09a1d009  popcount: 30
    0x0838add013f0fa93  popcount: 29
    0x28d8c9410c0eeb41  popcount: 25
    0xf44d1b90fb59d005  popcount: 31
    0x0419ba581af22d18  popcount: 26
    0x8ade0f20f5c601a6  popcount: 29
    0x24ea8f92586ff0c3  popcount: 32
    0xcc52f83afac879e2  popcount: 34
    0x2ea67c2b72f80b0c  popcount: 31
    0xd040b8179211aced  popcount: 27
    0xfbcdbf2e77c03096  popcount: 37
    0x896d8c76a153471f  popcount: 32
    0x76016e4b9a136789  popcount: 30
    0xff13d1e99b2b3c8f  popcount: 38
    0x695e72a023f5587b  popcount: 33
    0x954711bc40aef756  popcount: 32
    state:
      0x8b0667659b8bdc3c
      0xbb70a46ef4b72e31
      0xefd926eb9af1888a
      0x03e11a6402830aee
    hamming distance from baseline: 128/256
    |}]

let%expect_test "2^64-1 bit state after one round" =
  let desired_state = [| 0L; 0L; 0L; 0xffffffffffffffffL |] in
  let seed_string = Seed_reverse.seed_string_for_state desired_state in
  let state = Xs_S.init_string seed_string in
  let baseline = dump_lane0 state in
  print_lane0 "baseline ([|0;0;0;2^64-1|])" baseline;
  [%expect
    {|
    baseline ([|0;0;0;2^64-1|]):
      0x0000000000000000
      0x0000000000000000
      0x0000000000000000
      0xffffffffffffffff
    |}];
  for _ = 0 to 31 do
    let draw = Xs_S.bits64 state in
    ignore @@ Xs_S.bits64 state;
    ignore @@ Xs_S.bits64 state;
    ignore @@ Xs_S.bits64 state;
    printf "0x%016Lx  popcount: %d\n" draw (popcount64 draw)
  done;
  let current_state = dump_lane0 state in
  print_lane0 "state" current_state;
  printf "hamming distance from baseline: %d/256\n"
    (hamming_distance baseline current_state);
  [%expect
    {|
    0xffffffffffffffff  popcount: 64
    0xffffffffff7ffffe  popcount: 62
    0xffffffffffffffff  popcount: 64
    0x0000000000000000  popcount: 0
    0x0000000000000000  popcount: 0
    0x000000ffffa1ffef  popcount: 34
    0x3ffde0ffffa1fff2  popcount: 48
    0x3e0267bc437ffff2  popcount: 39
    0xbdff88bc52497892  popcount: 34
    0x3089ffffd2b881ae  popcount: 36
    0x2fcb1aaeceb97e1b  popcount: 38
    0x2fca7af67c5fe40b  popcount: 38
    0xbd3a9ceb5c3b3413  popcount: 35
    0xa7fb375cc844ce9a  popcount: 35
    0x63857531ac74e3a8  popcount: 31
    0x72d4526f4557c634  popcount: 32
    0x827951444fba25e3  popcount: 30
    0xb72bbdd4fca8792a  popcount: 37
    0x846005b18a5174b9  popcount: 25
    0xa3cd74310102aaf6  popcount: 28
    0x4571c429b2f3d68f  popcount: 33
    0x754a1c79f088c8ba  popcount: 30
    0xef2aa7742d1081d7  popcount: 32
    0xaaa0386d921b360b  popcount: 28
    0x11a026ae090d4027  popcount: 22
    0x2e5c9b14dabc1c83  popcount: 31
    0xa050c4fe5f62c75f  popcount: 34
    0x1ac3c6cc98ec8eae  popcount: 32
    0x6a07ab5f01f89fbf  popcount: 37
    0xe63caf8f2449192e  popcount: 32
    0xd065d92296ae1de9  popcount: 32
    0x20de5bae9f56946b  popcount: 35
    state:
      0x32c2c9c71c5c211f
      0x9c62f364ac30e888
      0xa1f11a3cbb90582b
      0xdee98b0a2efb0c36
    hamming distance from baseline: 117/256
    |}]

let%expect_test "single-bit state after one round, full state" =
  let desired_state = [| 0L; 0L; 1L; 0L |] in
  let seed_string = Seed_reverse.seed_string_for_state desired_state in
  let state = Xs_S.init_string seed_string in
  let baseline = Xs_S.for_testing_raw_dump state in
  print_lanes "initial state" baseline;
  [%expect
    {|
    initial state:
      lane 0:
        0x0000000000000000
        0x0000000000000000
        0x0000000000000001
        0x0000000000000000
      lane 1:
        0xaf61c2e3bd9e66e9
        0xb493fa7697dc7648
        0xc4da4e8f7493585a
        0x462665f3c137de72
      lane 2:
        0xf476f98dde4c3eca
        0x363c43ed9f9717fb
        0x3c34b0770064033e
        0xb99216069cf4670b
      lane 3:
        0xafacb4646b76e67b
        0xb9fbb15b987e662c
        0x1dcddbd64811212a
        0x97c58f882a425e62
    |}];
  for _ = 0 to 127 do
    let draw = Xs_S.bits64 state in
    printf "0x%016Lx  popcount: %d\n" draw (popcount64 draw)
  done;
  let current_state = Xs_S.for_testing_raw_dump state in
  print_lanes "current_state" current_state;
  [%expect
    {|
    0x0000000000000000  popcount: 0
    0x1b212e066b992afd  popcount: 31
    0xbeb499e0c9234351  popcount: 30
    0xa5f79106da1a9f9c  popcount: 34
    0x0000000000000000  popcount: 0
    0x3ca410a3c3df3cb0  popcount: 30
    0xaa2ca2be30a44999  popcount: 28
    0xc1631d62174f2c5d  popcount: 31
    0x0000000000800011  popcount: 3
    0x2ff1240f52ee4d1a  popcount: 32
    0xb3e58c2f32c786ed  popcount: 35
    0xa3060905f94cfd80  popcount: 27
    0x0002200000800011  popcount: 5
    0x58b4b9a3abd16850  popcount: 30
    0x36a14f4a9773fc9a  popcount: 35
    0xbe743389732e9305  popcount: 32
    0x0002210044a20011  popcount: 10
    0x4e93f2ba4fa298a7  popcount: 34
    0x0effe6e9416a63f9  popcount: 37
    0x43a107ec4ec626e1  popcount: 29
    0x4004010044220894  popcount: 11
    0x901cb60fb1849a5f  popcount: 30
    0x9b70dffa2dfe7652  popcount: 40
    0x9c50eaf787754310  popcount: 31
    0x4312894444a20895  popcount: 20
    0xd7b4449b37ff1731  popcount: 37
    0xa562ade9d4d412cc  popcount: 31
    0xd27caab9369d8d5e  popcount: 36
    0x43108966512a9014  popcount: 21
    0x0007182622f50ddf  popcount: 26
    0x5374a98e405930de  popcount: 29
    0xb1b91c75a11a5b0d  popcount: 31
    0x53988966112ece25  popcount: 28
    0x3ce3cafd294ace50  popcount: 33
    0xc7a4a75edac94f4b  popcount: 36
    0x4c20282224f74db5  popcount: 26
    0xd9d0cb9195b64ec6  popcount: 33
    0xb061ce322954e1b0  popcount: 27
    0x121f84c5a9893ca8  popcount: 27
    0x8ac9c7de676aa65e  popcount: 36
    0xdbdd537e060e7092  popcount: 33
    0xc433e3e97051c4a9  popcount: 30
    0xa96eae9445789e71  popcount: 33
    0xf67f4c3e7a84f055  popcount: 36
    0x46087159a6e73ebf  popcount: 34
    0x3bda94f823accf6e  popcount: 36
    0x681563670ff9d1f0  popcount: 33
    0xcc9910546820325b  popcount: 24
    0xf8dd3ad51a4f170d  popcount: 35
    0x71241ab9b3a6bc4f  popcount: 33
    0x170df9cea6d3905c  popcount: 33
    0x2fd0b9b9f4afa34a  popcount: 36
    0x6d31be65b9bd7d13  popcount: 38
    0xb4e0e87d38014106  popcount: 25
    0x644c600cdbd5796a  popcount: 30
    0x7a58e33ed3496707  popcount: 34
    0x82d0de1e882d5928  popcount: 27
    0x8236de026da8a1fc  popcount: 30
    0x8152553ea506c485  popcount: 26
    0xe095cfc12b13caaf  popcount: 33
    0xef1e7d2e15dcf500  popcount: 35
    0xf33bcdb3cecb6fcd  popcount: 42
    0xabb0619b3b76965b  popcount: 35
    0xc0b250a6ffeeb9ea  popcount: 36
    0x469115cd2c6a5541  popcount: 27
    0xccab0ca3ac0fc9ef  popcount: 34
    0xb2f9864e51f3d26d  popcount: 35
    0x18432d4bd0f82a52  popcount: 27
    0x442b0f7022220ae1  popcount: 23
    0x30f5e9bb93f66339  popcount: 37
    0xf13c63f8bf909ccd  popcount: 36
    0xb82bae08890d80da  popcount: 26
    0x6d632c9d59144a2a  popcount: 29
    0xfa80ae4af3471751  popcount: 32
    0xb1f3803ee83d32a2  popcount: 31
    0x5cf91e136e050200  popcount: 25
    0xbb01e1f5fa518b09  popcount: 32
    0x30a682be2fa6f714  popcount: 32
    0x00db8e569bb50682  popcount: 28
    0x42e76e8806ccab2c  popcount: 29
    0xf1f9962bf1911ad0  popcount: 33
    0x36487273fb5b7baf  popcount: 39
    0x8efc8d77a3d6b158  popcount: 36
    0x37a768d6494df633  popcount: 35
    0x6092f088356649f1  popcount: 27
    0x47b056af119b013a  popcount: 29
    0x25cfd6f2cf89a204  popcount: 32
    0xb162cb9c1150be52  popcount: 29
    0xb72def8738adde1c  popcount: 38
    0x06de8ed3b1eac1b2  popcount: 33
    0x037b6524f07f3b91  popcount: 33
    0xb5924c872be5d2c2  popcount: 31
    0xb0d6c5e4e6a1aa2b  popcount: 32
    0x18176b76631931b2  popcount: 30
    0xdb22fabe2da4af60  popcount: 35
    0x938a90c36071baa1  popcount: 27
    0x88ec1d404e9ecd9e  popcount: 31
    0x5ce064622fc9ac43  popcount: 29
    0x21558e979541f30a  popcount: 29
    0xc466a55d2660f737  popcount: 33
    0xde4a36578890491c  popcount: 28
    0xd91d86221b7614a9  popcount: 29
    0x9d9e7a12573ef47d  popcount: 38
    0x288c907bc64d42c7  popcount: 28
    0xffe93f72916b40c4  popcount: 35
    0x0f9ec370e68b59c1  popcount: 32
    0xa4f2453d48bbd90e  popcount: 32
    0xdb2c558ad2ce8a77  popcount: 34
    0x40da637b8b175c12  popcount: 30
    0xe9c9d274f25d4347  popcount: 34
    0xe669a535ece4f323  popcount: 35
    0xc8b789e92a240c1c  popcount: 27
    0x3c1fe8c09579515e  popcount: 32
    0xc69d37d481dea15f  popcount: 35
    0x3a309065c00fd6fb  popcount: 30
    0xf9c68636746c95fb  popcount: 36
    0xd1524941008bccac  popcount: 24
    0x4542fbd4e6ac142e  popcount: 31
    0xd5e6886ce191bacb  popcount: 33
    0x70178624e14256ba  popcount: 27
    0x67357f9de43adc1e  popcount: 38
    0x64df339add9ac91d  popcount: 36
    0x0ec55ae88ca09c15  popcount: 27
    0x7def065313659e03  popcount: 33
    0x984a5bfe667df685  popcount: 37
    0x4826be2739b43434  popcount: 29
    0x1afaff28344b4960  popcount: 31
    0x7ac96162f26b4c80  popcount: 29
    current_state:
      lane 0:
        0xe92fdd4bf7446006
        0x9007c3c8b08de3ff
        0xf00a9265ffa30f13
        0x6d8bd6e6d41f82ce
      lane 1:
        0x91a3b057d2c72760
        0xed3da1bf42c3424c
        0x1cc4c68c2bb4a3a8
        0x11536578b959b5d6
      lane 2:
        0x82134b8567b854c2
        0x0c6775bb7d2ad0be
        0x34a944c7f2ce0e09
        0x3e86741695e8e3c3
      lane 3:
        0xcbfea92244d500e2
        0xdb398aa04c991a00
        0x923c0b05def9a6fb
        0xc8dc55d9f0cc1428
    |}]
