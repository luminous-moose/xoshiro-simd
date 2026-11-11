open Base
open! Stdio
open! Stdlib.Printf
module List = List_extra
module Xs_S = Xoshiro_simd.State
module Xs = Xoshiro_simd

let hex2byte h = Hex.to_bytes (`Hex h)
let seed = 42L

let printbuckets buckets sum =
  for i = 0 to Array.length buckets - 1 do
    printf "%d\n" buckets.(i)
  done;
  let sum' = Array.fold buckets ~init:0 ~f:(fun acc x -> acc + x) in
  assert (sum = sum')

(* --- bits function tests --- *)

let%test_unit "bits returns values in [0, 2^30)" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 1000 do
    let r = Xs_S.bits state in
    assert (r >= 0 && r <= Int.max_value_30_bits)
  done

let%expect_test "bits hist" =
  let state = Xs_S.init_deterministic seed in
  let nbuckets = 20 in
  let size = Int.max_value_30_bits / nbuckets in
  let buckets = Array.init nbuckets ~f:(fun _ -> 0) in
  for _ = 1 to 20000 do
    let r = Xs_S.bits state in
    let i = r / size in
    assert (i < nbuckets);
    buckets.(i) <- buckets.(i) + 1
  done;
  printbuckets buckets 20000;
  [%expect
    {|
    963
    1037
    964
    954
    980
    1028
    960
    970
    936
    1013
    1040
    1032
    978
    1076
    999
    968
    1041
    1073
    1008
    980
    |}]

let%expect_test "bits" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 15 do
    let r = Xs_S.bits state in
    printf "%d\n" r
  done;
  [%expect
    {|
    74868895
    276188495
    691739365
    11990206
    352289867
    1025146900
    773696092
    753452467
    393164689
    295584116
    65518387
    498326934
    515148392
    524936809
    533904376
    |}]

let%expect_test "signed_bits31 dls" =
  Xs.init_deterministic seed;
  for _ = 1 to 15 do
    let r = Xs.bits32 () in
    let r = Int32.shift_right r 1 in
    printf "%ld\n" r
  done;
  [%expect
    {|
    574305359
    -398776665
    345869682
    -530875809
    713015845
    -561168374
    386848046
    913597145
    733453256
    684662970
    -1040982631
    786034379
    257574196
    799339316
    266952188
    |}]

let%test_unit "bits32 has full 32-bit range" =
  let state = Xs_S.init_deterministic seed in
  let has_neg = ref false in
  let has_pos = ref false in
  for _ = 1 to 1000 do
    let r = Xs_S.bits32 state in
    if Int32.(r < 0l) then has_neg := true;
    if Int32.(r > 0l) then has_pos := true
  done;
  assert !has_neg;
  assert !has_pos

let%expect_test "bits32" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 15 do
    let r = Xs_S.bits32 state in
    printf "%ld\n" r
  done;
  [%expect
    {|
    1148610719
    -797553329
    691739365
    -1061751618
    1426031691
    -1122336748
    773696092
    1827194291
    1466906513
    1369325940
    -2081965261
    1572068758
    515148392
    1598678633
    533904376
    |}]

let%test_unit "bits64 has full 64-bit range" =
  let state = Xs_S.init_deterministic seed in
  let has_neg = ref false in
  let has_pos = ref false in
  for _ = 1 to 1000 do
    let r = Xs_S.bits64 state in
    if Int64.(r < 0L) then has_neg := true;
    if Int64.(r > 0L) then has_pos := true
  done;
  assert !has_neg;
  assert !has_pos

let%expect_test "bits64" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 15 do
    let r = Xs_S.bits64 state in
    printf "%Ld\n" r
  done;
  [%expect
    {|
    -3425465463722317665
    -4560188475093345563
    -4820399626332961717
    7847739724056603228
    5881210131331364753
    6751983904886340403
    6866272446064134760
    7232580594621922296
    -297100157724070516
    635420893945114766
    5967244582632191458
    -5122571522610674715
    -5513075133950446152
    -2500746728240233651
    3471631850228312087
    |}]

(* --- uniform_float tests --- *)

let%test_unit "uniform_float returns values in [0.0, 1.0)" =
  (* uniform_float no longer excludes 0.0 -- see uniform_float_strict below
     for the function that still guarantees the open interval. *)
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 1000 do
    let r = Xs_S.uniform_float state in
    assert (Float.(r >= 0.0 && r < 1.0))
  done

let%expect_test "uniform_float hist" =
  let state = Xs_S.init_deterministic seed in
  let nbuckets = 20 in
  let size = Float.(1.0 / of_int nbuckets) in
  let buckets = Array.init nbuckets ~f:(fun _ -> 0) in
  for _ = 1 to 30000 do
    let r = Xs_S.uniform_float state in
    assert (Float.(r < 1.0));
    assert (Float.(r >= 0.0));
    let i = r /. size |> Float.to_int in
    assert (i < nbuckets);
    buckets.(i) <- buckets.(i) + 1
  done;
  printbuckets buckets 30000;
  [%expect
    {|
    1560
    1519
    1455
    1449
    1479
    1498
    1500
    1496
    1466
    1547
    1476
    1496
    1540
    1521
    1491
    1461
    1469
    1538
    1546
    1493
    |}]

let%expect_test "uniform_float" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 15 do
    let r = Xs_S.uniform_float state in
    assert (Float.(r < 1.0));
    assert (Float.(r >= 0.0));
    print_endline @@ Float.to_string r
  done;
  [%expect
    {|
    0.085755595295460951
    0.64465907181611981
    0.043657787601690146
    0.53084272237271835
    0.31041139572710486
    0.805098723145103
    0.74948540995313384
    0.048011778021376794
    0.062569781563214133
    0.84995630024785573
    0.77765606057027
    0.032569528711478
    0.306461322653673
    0.064551417442654024
    0.098588285600238468
    |}]

let%test_unit "uniform_float always returns < 1.0" =
  let desired_state = [| 0L; 0L; 0L; 0xffffffffffffffffL |] in
  let seed_string = Seed_reverse.seed_string_for_state desired_state in
  (* The above lines make the generator return the highest value possible. *)
  let int64_draw = Xs_S.bits64 (Xs_S.init_string seed_string) in
  assert (Int64.(int64_draw = 0xffffffffffffffffL));
  let top = Xs_S.uniform_float (Xs_S.init_string seed_string) in
  assert (Float.(top < 1.0));
  assert (Float.equal top (1.0 -. Float.ldexp 1.0 (-53)))

(* --- bool tests --- *)

let%expect_test "bool" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 20 do
    printf "%b\n" (Xs_S.bool state)
  done;
  [%expect
    {|
    false
    true
    false
    true
    false
    true
    false
    false
    false
    false
    true
    false
    false
    false
    false
    false
    false
    true
    true
    false
    |}]

(* The exact-sequence test above is a regression check, not a correctness
   check: it would pass just as well if bool secretly returned the wrong
   bit, as long as that wrong bit happened to be what got hardcoded. The
   two tests below are independent correctness checks instead.

   This one decodes, from the raw bitstream word via for_testing_raw_dump,
   bit 31 (see rand_core.ml's [bool]: the same bit on every host width,
   unlike signed_bits31), entirely independently of bool's own code, and
   checks bool agrees on many draws. A frequency check alone couldn't catch
   a wrong-bit-position bug: any single bit of a good PRNG's output is
   close enough to 50/50 that using the wrong bit wouldn't show up as
   bias. *)
let%test_unit "bool matches the documented bit of its raw bitstream word" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 300 do
    let b = Xs_S.bool state in
    let { Xs_S.bitstream; int_idx; _ } = Xs_S.for_testing_raw_dump state in
    (* bitstream is dword-granular, the same unit int_idx counts (see
       for_testing_raw_dump's doc comment), so no unit conversion is needed
       here any more. int_idx has already advanced past the word bool just
       consumed, so that word sits at bitstream.(int_idx - 1). *)
    let raw = bitstream.(int_idx - 1) in
    let expected = Int32.(raw < 0l) in
    assert (Bool.equal b expected)
  done

(* Simple statistical sanity check, independent of any bit-position
   knowledge: catches "always returns the same value" or other gross
   breakage even if it happened to also break the bit-position test above
   in a matching way (e.g. both this file and the implementation making
   the same wrong assumption about which bit is used). *)
let%test_unit "bool is roughly balanced over many draws" =
  let state = Xs_S.init_deterministic seed in
  let n = 20_000 in
  let true_count = ref 0 in
  for _ = 1 to n do
    if Xs_S.bool state then true_count := !true_count + 1
  done;
  (* A fair coin's count at n = 20000 has stddev ~70.7; a window of +/-500
     is over 7 stddev, wide enough to never flake on a correct
     implementation while still catching gross bias. *)
  assert (abs (!true_count - (n / 2)) < 500)

(* --- nativebits function tests --- *)

(* nativebits/nativeint/nativeint_in_range dispatch on Stdlib.Sys.word_size at
   module load time, so expect_test snapshots would differ between 32-bit and
   64-bit platforms.  We use delegation tests and property tests instead. *)

let%test_unit "nativebits delegates to bits32 or bits64" =
  let state1 = Xs_S.init_deterministic 0L in
  let state2 = Xs_S.init_deterministic 0L in
  for _ = 1 to 32 do
    let expected =
      if Stdlib.Sys.word_size = 32 then Nativeint.of_int32 (Xs_S.bits32 state1)
      else Int64.to_nativeint_exn (Xs_S.bits64 state1)
    in
    let actual = Xs_S.nativebits state2 in
    assert (Nativeint.(expected = actual))
  done

(* --- Bulk fill tests --- *)

(* get_insecure_bytes generates whole 32-byte blocks (4 interleaved lanes x 8
   bytes) straight into the destination, and each call discards whatever is left
   of its last block. So splitting a request into several calls reproduces the
   single-call output only when every chunk except the last is a multiple of 32
   bytes; the two tests below pin both halves of that behavior. *)
let block_bytes = 32

let insecure_chunks sizes =
  let state = Xs_S.init_deterministic seed in
  List.map sizes ~f:(fun n ->
      Xs_S.get_insecure_bytes state n |> Bytes.to_string)

let%test_unit
    "get_insecure_bytes output identical for one chunk vs multiple chunks \
     whose sizes are multiples of 32" =
  let total = block_bytes * 12 in
  let reference = String.concat (insecure_chunks [ total ]) in
  (* Every chunk but the last must be block-aligned; the last may be any
     length, since it is just a prefix of the next block. *)
  let check sizes =
    let got = String.concat (insecure_chunks sizes) in
    assert (String.equal (String.prefix reference (String.length got)) got)
  in
  check (List.init 12 ~f:(fun _ -> block_bytes));
  check [ total / 2; total / 2 ];
  check [ block_bytes; total - block_bytes ];
  check [ total - block_bytes; block_bytes ];
  check [ block_bytes * 2; block_bytes * 3; block_bytes * 7 ];
  (* Zero-length calls consume no blocks. *)
  check [ 0; block_bytes; 0; block_bytes; 0 ];
  (* Non-aligned final chunks. *)
  check [ block_bytes * 4; (block_bytes * 4) + 5 ];
  check [ block_bytes * 11; block_bytes - 1 ];
  check [ 5 ]

let%test_unit
    "get_insecure_bytes: each call consumes whole 32-byte blocks, discarding \
     the rest of its last block" =
  let blocks_of n = (n + block_bytes - 1) / block_bytes in
  let check sizes =
    let total_blocks =
      List.fold sizes ~init:0 ~f:(fun acc n -> acc + blocks_of n)
    in
    let reference =
      String.concat (insecure_chunks [ total_blocks * block_bytes ])
    in
    let state = Xs_S.init_deterministic seed in
    let (_ : int) =
      List.fold sizes ~init:0 ~f:(fun consumed n ->
          let got = Xs_S.get_insecure_bytes state n |> Bytes.to_string in
          assert (
            String.equal got
              (String.sub reference ~pos:(consumed * block_bytes) ~len:n));
          consumed + blocks_of n)
    in
    ()
  in
  check [ 1; 1; 1 ];
  check [ 31; 33; 32; 1 ];
  check [ 0; 5; 0; 40; 0 ];
  check [ 63; 1; 64; 31; 65 ];
  (* Many tiny calls: each one burns a whole block. *)
  check (List.init 100 ~f:(fun _ -> 4));
  check (List.init 40 ~f:(fun i -> i * 3))

let%test_unit "fill_insecure_bigarray output matches get_insecure_bytes" =
  let state1 = Xs_S.init_deterministic seed in
  let state2 = Xs_S.init_deterministic seed in
  let n = 32 in
  let ba = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout n in
  Xs_S.fill_insecure_bigarray state1 ba;
  let bytes = Xs_S.get_insecure_bytes state2 n in
  for i = 0 to n - 1 do
    assert (Bigarray.Array1.get ba i = Char.to_int (Bytes.get bytes i))
  done

let%test_unit "fill_insecure_bigarray empty array is a no-op" =
  let state = Xs_S.init_deterministic seed in
  let ba = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout 0 in
  Xs_S.fill_insecure_bigarray state ba

(* xs256simdintf_get_bytes_ba (xs256_stubs.c) dispatches on Bigarray element
   kind on big-endian hosts; char/float32/float64 aren't exercised by the
   int8_unsigned-kind test above. The machines this was developed on are little-
   endian, so they can't catch a big-endian-only regression here, but it does
   run for real on s390x/powerpc hosts and test environments. *)
let%test_unit "fill_insecure_bigarray matches get_insecure_bytes for char kind"
    =
  let state1 = Xs_S.init_deterministic seed in
  let state2 = Xs_S.init_deterministic seed in
  let n = 32 in
  let ba = Bigarray.Array1.create Bigarray.char Bigarray.c_layout n in
  Xs_S.fill_insecure_bigarray state1 ba;
  let bytes = Xs_S.get_insecure_bytes state2 n in
  for i = 0 to n - 1 do
    assert (Char.equal (Bigarray.Array1.get ba i) (Bytes.get bytes i))
  done

let%test_unit
    "fill_insecure_bigarray matches get_insecure_bytes for float32 kind" =
  let state1 = Xs_S.init_deterministic seed in
  let state2 = Xs_S.init_deterministic seed in
  let n = 32 in
  let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (n / 4) in
  Xs_S.fill_insecure_bigarray state1 ba;
  let bytes = Xs_S.get_insecure_bytes state2 n in
  for i = 0 to (n / 4) - 1 do
    let expected_bits = Stdlib.Bytes.get_int32_le bytes (i * 4) in
    let actual_bits = Int32.bits_of_float (Bigarray.Array1.get ba i) in
    assert (Int32.equal actual_bits expected_bits)
  done

let%test_unit
    "fill_insecure_bigarray matches get_insecure_bytes for float64 kind" =
  let state1 = Xs_S.init_deterministic seed in
  let state2 = Xs_S.init_deterministic seed in
  let n = 32 in
  let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout (n / 8) in
  Xs_S.fill_insecure_bigarray state1 ba;
  let bytes = Xs_S.get_insecure_bytes state2 n in
  for i = 0 to (n / 8) - 1 do
    let expected_bits = Stdlib.Bytes.get_int64_le bytes (i * 8) in
    let actual_bits = Int64.bits_of_float (Bigarray.Array1.get ba i) in
    assert (Int64.equal actual_bits expected_bits)
  done

(* Complex32/64 join the float32/int32 and float64/int64 cases respectively
   in xs256simdintf_get_bytes_ba (xs256_stubs.c): each complex element is two
   consecutive native reads (real, then imaginary) at the component width,
   with no inter-element padding (Bigarray.kind_size_in_bytes complex32 = 8,
   complex64 = 16 -- exactly 2x the component width), so N complex elements
   should be byte-for-byte identical to 2N same-width float elements over the
   same memory. *)
let%test_unit
    "fill_insecure_bigarray matches get_insecure_bytes for complex32 kind" =
  let state1 = Xs_S.init_deterministic seed in
  let state2 = Xs_S.init_deterministic seed in
  let n = 32 in
  let ba =
    Bigarray.Array1.create Bigarray.complex32 Bigarray.c_layout (n / 8)
  in
  Xs_S.fill_insecure_bigarray state1 ba;
  let bytes = Xs_S.get_insecure_bytes state2 n in
  for i = 0 to (n / 8) - 1 do
    let c = Bigarray.Array1.get ba i in
    let expected_re = Stdlib.Bytes.get_int32_le bytes (i * 8) in
    let expected_im = Stdlib.Bytes.get_int32_le bytes ((i * 8) + 4) in
    assert (Int32.equal (Int32.bits_of_float c.Stdlib.Complex.re) expected_re);
    assert (Int32.equal (Int32.bits_of_float c.Stdlib.Complex.im) expected_im)
  done

let%test_unit
    "fill_insecure_bigarray matches get_insecure_bytes for complex64 kind" =
  let state1 = Xs_S.init_deterministic seed in
  let state2 = Xs_S.init_deterministic seed in
  let n = 64 in
  let ba =
    Bigarray.Array1.create Bigarray.complex64 Bigarray.c_layout (n / 16)
  in
  Xs_S.fill_insecure_bigarray state1 ba;
  let bytes = Xs_S.get_insecure_bytes state2 n in
  for i = 0 to (n / 16) - 1 do
    let c = Bigarray.Array1.get ba i in
    let expected_re = Stdlib.Bytes.get_int64_le bytes (i * 16) in
    let expected_im = Stdlib.Bytes.get_int64_le bytes ((i * 16) + 8) in
    assert (Int64.equal (Int64.bits_of_float c.Stdlib.Complex.re) expected_re);
    assert (Int64.equal (Int64.bits_of_float c.Stdlib.Complex.im) expected_im)
  done

(* int/nativeint join the int32/float32/complex32 or int64/float64/complex64
   case in xs256simdintf_get_bytes_ba depending on host word size (ARCH_
   SIXTYFOUR), unlike every other kind above, which has a fixed width
   regardless of platform -- so unlike those, this can't just compare
   against a fixed-width slice of get_insecure_bytes. Bigarray.kind_size_in_bytes
   gives the real per-platform element width directly, rather than assuming
   it from Sys.word_size. Bigarray.int additionally isn't a lossless view of
   the underlying native word -- Array1.get for int-kind elements drops the
   word's top bit (its "OCaml-style, 31/63-bit integer" contract), verified
   empirically to match Int64.to_int's own documented "modulo 2^word_bits"
   truncation exactly, which is what's used to compute the expected value
   below instead of a plain byte comparison. nativeint has no such loss
   (Int64.to_nativeint is exact on a 64-bit host, and takes the low 32 bits
   on a 32-bit host -- again matching nativeint-kind storage width exactly). *)
let%test_unit "fill_insecure_bigarray matches get_insecure_bytes for int kind" =
  let state1 = Xs_S.init_deterministic seed in
  let state2 = Xs_S.init_deterministic seed in
  let word_bytes = Bigarray.kind_size_in_bytes Bigarray.int in
  let n_elements = 4 in
  let n = n_elements * word_bytes in
  let ba = Bigarray.Array1.create Bigarray.int Bigarray.c_layout n_elements in
  Xs_S.fill_insecure_bigarray state1 ba;
  let bytes = Xs_S.get_insecure_bytes state2 n in
  for i = 0 to n_elements - 1 do
    let offset = i * word_bytes in
    let raw64 =
      if word_bytes = 8 then Stdlib.Bytes.get_int64_le bytes offset
      else Int64.of_int32 (Stdlib.Bytes.get_int32_le bytes offset)
    in
    assert (Int.equal (Bigarray.Array1.get ba i) (Stdlib.Int64.to_int raw64))
  done

let%test_unit
    "fill_insecure_bigarray matches get_insecure_bytes for nativeint kind" =
  let state1 = Xs_S.init_deterministic seed in
  let state2 = Xs_S.init_deterministic seed in
  let word_bytes = Bigarray.kind_size_in_bytes Bigarray.nativeint in
  let n_elements = 4 in
  let n = n_elements * word_bytes in
  let ba =
    Bigarray.Array1.create Bigarray.nativeint Bigarray.c_layout n_elements
  in
  Xs_S.fill_insecure_bigarray state1 ba;
  let bytes = Xs_S.get_insecure_bytes state2 n in
  for i = 0 to n_elements - 1 do
    let offset = i * word_bytes in
    let raw64 =
      if word_bytes = 8 then Stdlib.Bytes.get_int64_le bytes offset
      else Int64.of_int32 (Stdlib.Bytes.get_int32_le bytes offset)
    in
    assert (
      Nativeint.equal (Bigarray.Array1.get ba i)
        (Stdlib.Int64.to_nativeint raw64))
  done

let%test_unit "fill_insecure_bytes output matches get_insecure_bytes" =
  let state1 = Xs_S.init_deterministic seed in
  let state2 = Xs_S.init_deterministic seed in
  let n = 32 in
  let buf = Bytes.create n in
  Xs_S.fill_insecure_bytes state1 buf n;
  let bytes = Xs_S.get_insecure_bytes state2 n in
  assert (Bytes.equal buf bytes)

let%test_unit "fill_insecure_bytes only touches the first n bytes" =
  let state = Xs_S.init_deterministic seed in
  let sentinel = '\xAA' in
  let buf = Bytes.make 32 sentinel in
  Xs_S.fill_insecure_bytes state buf 16;
  for i = 16 to 31 do
    assert (Char.equal (Bytes.get buf i) sentinel)
  done

let%test_unit "fill_insecure_bytes with n=0 is a no-op" =
  let state = Xs_S.init_deterministic seed in
  let sentinel = '\xAA' in
  let buf = Bytes.make 8 sentinel in
  Xs_S.fill_insecure_bytes state buf 0;
  for i = 0 to 7 do
    assert (Char.equal (Bytes.get buf i) sentinel)
  done

let%test "fill_insecure_bytes raises on negative n" =
  try
    let state = Xs_S.init () in
    Xs_S.fill_insecure_bytes state (Bytes.create 8) (-1);
    false
  with Invalid_argument _ -> true

(* Sys.max_string_length + 1 is checked before Bytes.create, so nothing is
   allocated, and it doesn't overflow on either word size. *)
let%test "get_insecure_bytes raises on n out of range" =
  let expected =
    "Xoshiro_simd.get_insecure_bytes: n must be between 0 and \
     Sys.max_string_length"
  in
  let raises f =
    match f () with
    | _ -> false
    | exception Invalid_argument msg -> String.equal msg expected
  in
  List.for_all
    [ -1; Sys.max_string_length + 1 ]
    ~f:(fun n ->
      raises (fun () -> Xs_S.get_insecure_bytes (Xs_S.init ()) n)
      && raises (fun () -> Xs.get_insecure_bytes n))

let%test "fill_insecure_bytes raises when n exceeds buffer length" =
  try
    let state = Xs_S.init () in
    Xs_S.fill_insecure_bytes state (Bytes.create 8) 9;
    false
  with Invalid_argument _ -> true

let%test_unit "fill_float32_bigarray returns values in [0.0, 1.0)" =
  let state = Xs_S.init_deterministic seed in
  let n = 1000 in
  let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n in
  Xs_S.fill_float32_bigarray state ba;
  for i = 0 to n - 1 do
    let v = Bigarray.Array1.get ba i in
    assert (Float.(v >= 0.0) && Float.(v < 1.0))
  done

let%test_unit "fill_float64_bigarray returns values in [0.0, 1.0)" =
  let state = Xs_S.init_deterministic seed in
  let n = 1000 in
  let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
  Xs_S.fill_float64_bigarray state ba;
  for i = 0 to n - 1 do
    let v = Bigarray.Array1.get ba i in
    assert (Float.(v >= 0.0) && Float.(v < 1.0))
  done

(* Pinned exact values, unlike the range/no-op/prefix-stability checks around
   this one: these two are a regression guard against host-byte-order-dependent
   output -- everything else here only checks *properties* (range,
   no-op-on-empty, stability), which a byte-order-dependent value could
   satisfy just as well as a correct one. %h (exact hex float), not %f like
   this file's other expect tests: decimal formatting could round two
   genuinely different bit patterns to the same printed digits, which would
   defeat the point of a regression guard here specifically.

   We pin both fill_floatxx_bigarray functions, as well as the
   fill_floatxx_range_bigarray functions, as the latter can surface outputs
   changing due to FMA instructions. *)
let%expect_test "fill_float64_bigarray exact values" =
  let state = Xs_S.init_deterministic seed in
  let n = 15 in
  let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
  Xs_S.fill_float64_bigarray state ba;
  for i = 0 to n - 1 do
    printf "%h\n" (Bigarray.Array1.get ba i)
  done;
  [%expect
    {|
    0x1.5f414253e365p-4
    0x1.4a10c0fd0a36ep-1
    0x1.65a5043ee11p-5
    0x1.0fca9e07d2244p-1
    0x1.3ddc7c23d0844p-2
    0x1.9c35e65beb4d8p-1
    0x1.7fbc8d3927ddap-1
    0x1.894fff0d9687p-5
    0x1.00492bd882888p-4
    0x1.b32d78e12ff91p-1
    0x1.8e28ef675efa8p-1
    0x1.0acf40952e8ap-5
    0x1.39d0ff39254dcp-2
    0x1.0867112d39eep-4
    0x1.93d14f66bf6ap-4
    |}]

let%expect_test "fill_float32_bigarray exact values" =
  let state = Xs_S.init_deterministic seed in
  let n = 15 in
  let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n in
  Xs_S.fill_float32_bigarray state ba;
  for i = 0 to n - 1 do
    printf "%h\n" (Bigarray.Array1.get ba i)
  done;
  [%expect
    {|
    0x1.f1b29p-3
    0x1.5f414p-4
    0x1.0a36e4p-1
    0x1.4a10cp-1
    0x1.ee110ap-1
    0x1.65a5p-5
    0x1.d22444p-1
    0x1.0fca9ep-1
    0x1.e84226p-1
    0x1.3ddc7cp-2
    0x1.eb4d88p-1
    0x1.9c35e6p-1
    0x1.27dda2p-1
    0x1.7fbc8cp-1
    0x1.b2d0fp-2
    |}]

(* Pinned exact values for the scale/offset range-fill path, same purpose as
   the two tests above: a regression guard against any backend fusing the
   final "multiply by scale, then add offset" into a single FMA instruction,
   which rounds once instead of twice and can change the low bit of the
   result. min/max are chosen so scale and offset each have no exact binary
   representation, giving the multiply and the add a genuine rounding step to
   fuse. *)
let%expect_test "fill_float64_range_bigarray exact values" =
  let state = Xs_S.init_deterministic seed in
  let n = 15 in
  let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
  Xs_S.fill_float64_range_bigarray state ba ~min:(-1.3) ~max:2.4;
  for i = 0 to n - 1 do
    printf "%h\n" (Bigarray.Array1.get ba i)
  done;
  [%expect
    {|
    -0x1.f7250486006e2p-1
    0x1.15d231d41fb25p+0
    -0x1.2372851bedfa3p+0
    0x1.540748b689862p-1
    -0x1.363a02f6f8fap-3
    0x1.adca1d5d401c3p+0
    0x1.791cd210235a1p+0
    -0x1.1f52c01c07653p+0
    -0x1.1188addc5503ap+0
    0x1.d84752d3cbf33p+0
    0x1.93cbbae5a2e91p+0
    -0x1.2df33c8859d1ap+0
    -0x1.54289f591f27p-3
    -0x1.0fa82a6d8a9a9p+0
    -0x1.ded59813addfp-1
    |}]

let%expect_test "fill_float32_range_bigarray exact values" =
  let state = Xs_S.init_deterministic seed in
  let n = 15 in
  let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n in
  Xs_S.fill_float32_range_bigarray state ba ~min:(-1.3) ~max:2.4;
  for i = 0 to n - 1 do
    printf "%h\n" (Bigarray.Array1.get ba i)
  done;
  [%expect
    {|
    -0x1.9a75a4p-2
    -0x1.f72504p-1
    0x1.3f64b4p-1
    0x1.15d23p+0
    0x1.229c9p+1
    -0x1.237284p+0
    0x1.08c7f2p+1
    0x1.54074cp-1
    0x1.1d3d3p+1
    -0x1.363ap-3
    0x1.200e1ep+1
    0x1.adca2p+0
    0x1.ad1a7p-1
    0x1.791cdp+0
    0x1.159ebp-2
    |}]

let%test_unit "fill_float32_bigarray empty array is a no-op" =
  let state = Xs_S.init_deterministic seed in
  let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 0 in
  Xs_S.fill_float32_bigarray state ba

let%test_unit "fill_float64_bigarray empty array is a no-op" =
  let state = Xs_S.init_deterministic seed in
  let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout 0 in
  Xs_S.fill_float64_bigarray state ba

(* Remainder-handling regression coverage: xs256_core_plain_c.c fills whole
   logical blocks (8 float32s / 4 float64s) directly into the destination,
   then handles a leftover partial block via one extra block generated into
   a stack buffer and memcpy'd out. Sweeping every length against a single
   longer reference fill checks that every residue is handled correctly: a
   shorter fill must always be an exact prefix of a longer one from the same
   seed, even across a remainder boundary. If the block/remainder split ever
   mismatched (wrong element count, elements swapped, state double-advanced or
   skipped), this would catch it as a divergence at or after the first affected
   length. *)
let%test_unit
    "fill_float32_bigarray: prefix is stable as length grows across every \
     remainder boundary" =
  let max_len =
    32
    (* 4 full periods of the 8-float32 logical block *)
  in
  let reference_state = Xs_S.init_deterministic seed in
  let reference =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout max_len
  in
  Xs_S.fill_float32_bigarray reference_state reference;
  for len = 0 to max_len do
    let state = Xs_S.init_deterministic seed in
    let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout len in
    Xs_S.fill_float32_bigarray state ba;
    for i = 0 to len - 1 do
      assert (
        Float.equal (Bigarray.Array1.get ba i) (Bigarray.Array1.get reference i))
    done
  done

let%test_unit
    "fill_float64_bigarray: prefix is stable as length grows across every \
     remainder boundary" =
  let max_len =
    16
    (* 4 full periods of the 4-float64 logical block *)
  in
  let reference_state = Xs_S.init_deterministic seed in
  let reference =
    Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout max_len
  in
  Xs_S.fill_float64_bigarray reference_state reference;
  for len = 0 to max_len do
    let state = Xs_S.init_deterministic seed in
    let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout len in
    Xs_S.fill_float64_bigarray state ba;
    for i = 0 to len - 1 do
      assert (
        Float.equal (Bigarray.Array1.get ba i) (Bigarray.Array1.get reference i))
    done
  done

(* fill_float64_bigarray bypasses the small doublestream buffer entirely
   (see rand_core.ml), but both it and uniform_float ultimately walk the
   identical per-lane xs256plus_scalar_next sequence -- just with different
   buffering. n is chosen bigger than FLOATWORDS (128) so this also proves
   the bulk path agrees with the scalar path across a buffer-refill
   boundary, not just within one internal block, and n % 4 <> 0 so it also
   exercises the remainder path. *)
let%test_unit "fill_float64_bigarray output matches uniform_float" =
  let state1 = Xs_S.init_deterministic seed in
  let state2 = Xs_S.init_deterministic seed in
  let n = 150 in
  let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
  Xs_S.fill_float64_bigarray state1 ba;
  for i = 0 to n - 1 do
    let expected = Xs_S.uniform_float state2 in
    assert (Float.equal expected (Bigarray.Array1.get ba i))
  done

(* Mirrors the prefix-stability tests for fill_float32_bigarray/
   fill_float64_bigarray above: a regression guard on
   fill_insecure_bigarray's own block-plus-remainder splitting, checking that
   a shorter fill is always an exact prefix of a longer one from the same
   seed, across every remainder boundary. *)
let%test_unit
    "fill_insecure_bigarray int kind: prefix is stable as length grows across \
     every remainder boundary" =
  let word_bytes = Bigarray.kind_size_in_bytes Bigarray.int in
  let elements_per_block = block_bytes / word_bytes in
  let max_len = elements_per_block * 4 in
  let reference_state = Xs_S.init_deterministic seed in
  let reference =
    Bigarray.Array1.create Bigarray.int Bigarray.c_layout max_len
  in
  Xs_S.fill_insecure_bigarray reference_state reference;
  for len = 0 to max_len do
    let state = Xs_S.init_deterministic seed in
    let ba = Bigarray.Array1.create Bigarray.int Bigarray.c_layout len in
    Xs_S.fill_insecure_bigarray state ba;
    for i = 0 to len - 1 do
      assert (
        Int.equal (Bigarray.Array1.get ba i) (Bigarray.Array1.get reference i))
    done
  done
