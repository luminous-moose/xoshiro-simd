open Base
open! Stdio
open! Stdlib.Printf
module Xs_S = Xoshiro_simd.State

let seed = 42L

let printbuckets buckets sum =
  for i = 0 to Array.length buckets - 1 do
    printf "%d\n" buckets.(i)
  done;
  let sum' = Array.fold buckets ~init:0 ~f:(fun acc x -> acc + x) in
  assert (sum = sum')

(* --- Int function tests --- *)

let%expect_test "randint hist" =
  let state = Xs_S.init_deterministic seed in
  let buckets = Array.init 19 ~f:(fun _ -> 0) in
  for _ = 1 to 20000 do
    let i = Xs_S.int state 19 in
    assert (i < 19);
    buckets.(i) <- buckets.(i) + 1
  done;
  printbuckets buckets 20000;
  [%expect
    {|
    1057
    1059
    1098
    1082
    1072
    1034
    1044
    1079
    1098
    1073
    992
    1015
    1039
    1040
    1039
    1072
    1007
    1078
    1022
    |}]

let%expect_test "randint" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 15 do
    let s = Xs_S.int state 10000000 |> Int.to_string in
    printf "%s\n" s
  done;
  [%expect
    {|
    2674317
    8143051
    1610581
    7527916
    3320238
    7386856
    1801401
    4254268
    3415407
    3188210
    5152546
    3660257
    1199423
    3722213
    1243092
    |}]

let%expect_test "full_int 30 bit range" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 15 do
    let s = Xs_S.full_int state 10000000 |> Int.to_string in
    printf "%s\n" s
  done;
  [%expect
    {|
    2674317
    8143051
    1610581
    7527916
    3320238
    7386856
    1801401
    4254268
    3415407
    3188210
    5152546
    3660257
    1199423
    3722213
    1243092
    |}]

(* 64-bit only: bounds above 2^30 don't fit in a 32-bit native int. Integer
   literals that wide don't compile on 32-bit hosts either, hence the Int64
   bounds. *)
let%expect_test ("full_int > 30 bit range 1" [@tags "64-bits-only"]) =
  let state = Xs_S.init_deterministic seed in
  let bound = Int64.to_int_exn 0xffff_ffffL in
  for _ = 1 to 15 do
    printf "%d\n" (Xs_S.full_int state bound)
  done;
  [%expect
    {|
    3396754166
    888735697
    1225589295
    3447928839
    1418116226
    1892535396
    3204397160
    3256419033
    3288511811
    3785593918
    2872913819
    3113590301
    539683468
    27814323
    872157038
    |}]

let%expect_test ("full_int > 30 bit range 2" [@tags "64-bits-only"]) =
  let state = Xs_S.init_deterministic seed in
  let bound = Int64.to_int_exn 0x2_0000_0001L in
  for _ = 1 to 15 do
    printf "%d\n" (Xs_S.full_int state bound)
  done;
  [%expect
    {|
    6679273724
    4369403971
    456729120
    6372500418
    391121771
    5008451124
    2005388186
    6288411534
    6024746888
    7969602124
    1830895772
    6692463581
    8481714949
    3148856736
    4560897626
    |}]

let%test "int raises on non-positive bound" =
  let state = Xs_S.init () in
  (try
     ignore (Xs_S.int state 0);
     false
   with Invalid_argument _ -> true)
  &&
    try
      ignore (Xs_S.int state (-1));
      false
    with Invalid_argument _ -> true

let%expect_test "intincl hist" =
  let state = Xs_S.init_deterministic seed in
  let buckets = Array.init 11 ~f:(fun _ -> 0) in
  for _ = 1 to 10000 do
    let i = Xs_S.int_in_range state ~min:4 ~max:10 in
    assert (i < 11);
    assert (i >= 4);
    buckets.(i) <- buckets.(i) + 1
  done;
  printbuckets buckets 10000;
  [%expect
    {|
    0
    0
    0
    0
    1393
    1460
    1449
    1439
    1432
    1417
    1410
    |}]

let%expect_test "intincl" =
  let state = Xs_S.init_deterministic seed in
  let max = 9000 in
  let min = -9000 in
  for _ = 1 to 15 do
    let s = Xs_S.int_in_range state ~min ~max |> Int.to_string in
    printf "%s\n" s
  done;
  [%expect
    {|
    -4186
    5658
    -6101
    4551
    -3024
    4297
    -5758
    -1342
    -2852
    -3261
    275
    -2412
    -6841
    -2300
    -6763
    |}]

let%test_unit
    "int_in_range full range triggers rejection sampling (span wraps to 0)" =
  let state = Xs_S.init_deterministic seed in
  let has_neg = ref false in
  let has_pos = ref false in
  for _ = 1 to 256 do
    let r = Xs_S.int_in_range state ~min:Int.min_value ~max:Int.max_value in
    assert (r >= Int.min_value && r <= Int.max_value);
    if r < 0 then has_neg := true;
    if r > 0 then has_pos := true
  done;
  assert !has_neg;
  assert !has_pos

let%test_unit
    "int_in_range half range triggers rejection sampling (span overflows \
     negative)" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 1000 do
    let r = Xs_S.int_in_range state ~min:Int.min_value ~max:(-1) in
    assert (r >= Int.min_value && r <= -1)
  done

let%test_unit "int_in_range equal bounds at extremes" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 10 do
    assert (
      Xs_S.int_in_range state ~min:Int.min_value ~max:Int.min_value
      = Int.min_value);
    assert (
      Xs_S.int_in_range state ~min:Int.max_value ~max:Int.max_value
      = Int.max_value)
  done

(* Spans wider than max_int only exist on 64-bit, where int_in_range falls back
   to rejection sampling 63-bit draws; spans between 2^31 and max_int go through
   full_int's debiased-modulo loop. The tests above only check that results are
   in range, so these pin the actual draws, and how many 64-bit words the
   8 draws used (more than 8 means some were rejected). No literal here may
   exceed 31 bits, or the file wouldn't compile on 32-bit hosts. *)
let print_int_in_range_draws ~min ~max =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 8 do
    printf "%d\n" (Xs_S.int_in_range state ~min ~max)
  done;
  let next = Xs_S.bits64 state in
  let twin = Xs_S.init_deterministic seed in
  let rec words_used n =
    if n > 100 then failwith "next word not found"
    else if Int64.equal (Xs_S.bits64 twin) next then n
    else words_used (n + 1)
  in
  printf "words used: %d\n" (words_used 0)

(* Never rejects: every 63-bit draw is in range. *)
let%expect_test ("int_in_range full range (64-bit)" [@tags "64-bits-only"]) =
  print_int_in_range_draws ~min:Int.min_value ~max:Int.max_value;
  [%expect
    {|
    -3425465463722317665
    -4560188475093345563
    4402972410521814091
    -1375632312798172580
    -3342161905523411055
    -2471388131968435405
    -2357099590790641048
    -1990791442232853512
    words used: 8
    |}]

(* Rejects about half of all draws. *)
let%expect_test
    ("int_in_range negative half range (64-bit)" [@tags "64-bits-only"]) =
  print_int_in_range_draws ~min:Int.min_value ~max:(-1);
  [%expect
    {|
    -3425465463722317665
    -4560188475093345563
    -1375632312798172580
    -3342161905523411055
    -2471388131968435405
    -2357099590790641048
    -1990791442232853512
    -297100157724070516
    words used: 9
    |}]

(* Span of about 3/4 of 2^63, not a power of two: rejects about a quarter. *)
let%expect_test
    ("int_in_range span just over max_int (64-bit)" [@tags "64-bits-only"]) =
  print_int_in_range_draws ~min:(Int.min_value / 2) ~max:Int.max_value;
  [%expect
    {|
    4402972410521814091
    -1375632312798172580
    -1990791442232853512
    -297100157724070516
    635420893945114766
    4100800514244101093
    3710296902904329656
    3471631850228312087
    words used: 15
    |}]

(* Below max_int, so this goes through full_int's loop rather than rejection
   sampling. Not a power of two, so the loop's own rejection is reachable. *)
let%expect_test
    ("int_in_range span between 2^31 and max_int (64-bit)"
     [@tags "64-bits-only"]) =
  print_int_in_range_draws ~min:(-(Int.max_value / 3)) ~max:(Int.max_value / 3);
  [%expect
    {|
    1361724613757099770
    794363108071585821
    664257532451777744
    1403376392856553075
    -1219518225836571918
    1446393618506966428
    513171584312921245
    317919778643035527
    words used: 13
    |}]

(* int_in_range with a span of 2^30 or more must give the same draws on 32-bit
   and 64-bit hosts. On 64-bit the span goes through full_int's 62-bit
   debiased-modulo loop; on 32-bit it wraps in a 31-bit int, so int_in_range
   emulates that loop with Int64 (this is the span <= 0 branch there). The span
   below is deliberately not a power of two: for a power of two, mod discards
   any extra high bit and nothing is ever rejected, so a mismatched emulation
   (for example one using int64's 63-bit draw) would pass.

   The expected output below is shared by every word size and was generated on
   64-bit, so this test failing on a 32-bit host is exactly the bug it exists
   to catch. *)
let%expect_test "int_in_range span of 2^30 or more is identical on every width"
    =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 15 do
    let s =
      Xs_S.int_in_range state ~min:(-0x4000_0000) ~max:(0x3fff_ffff - 15)
    in
    printf "%d\n" s
  done;
  [%expect
    {|
    422165623
    525722877
    -15907910
    951216991
    872247835
    -8529400
    -458308096
    830386326
    138005069
    562093911
    470017907
    851947365
    -892833322
    175117250
    781380080
    |}]

(* The rejection branch of that loop can't be reached by sampling from a 32-bit
   host: the largest span there is 2^31, so a draw is rejected about once in
   2^31. Instead, use Seed_reverse to pin lane 0's state so that the first
   64-bit word drawn is exactly the one we choose, and probe both sides of the
   rejection threshold.

   The 62-bit rule: r = bits 1..62 of the word; r is accepted iff
   r < floor(2^62 / span) * span (equivalently r - r mod span <= 2^62 - span).
   Bit 0 and bit 63 of the word must not matter, so each case is run with them
   clear and with them set. A rejected draw consumes exactly one word, and the
   result then comes from the next word (lane 1's first output). The same test
   runs on 64-bit hosts, where it checks full_int's loop itself. *)
let%test_unit "int_in_range span of 2^30 or more: rejection boundary" =
  let rotr64 w n =
    let m = 64 - n in
    Int64.(bit_or (shift_right_logical w n) (shift_left w m))
  in
  (* xoshiro256++ outputs rotl(s0 + s3, 23) + s0, so with s0 = 0 and s3 =
     rotr(w, 23) the first output is exactly w. *)
  let state_with_first_word w =
    let desired_state =
      if Int64.(w = 0L) then [| 0L; 1L; 0L; 0L |]
      else [| 0L; 0L; 0L; rotr64 w 23 |]
    in
    Xs_S.init_string (Seed_reverse.seed_string_for_state desired_state)
  in
  let mask62 = 0x3fff_ffff_ffff_ffffL in
  let r_of_word w = Int64.(bit_and (shift_right_logical w 1) mask62) in
  List.iter
    [
      (-0x4000_0000, 0x3fff_fff0);
      (-0x4000_0000, 0);
      (-1_000_000_000, 1_000_000_000);
    ]
    ~f:(fun (min, max) ->
      (* In Int64: max - min + 1 wraps in a 31-bit native int on 32-bit. *)
      let min64 = Int64.of_int min in
      let max64 = Int64.of_int max in
      let span = Int64.(max64 - min64 + 1L) in
      assert (Int64.(span > 0x4000_0000L));
      let two_pow_62 = Int64.shift_left 1L 62 in
      (* How many complete buckets of [span] values fit in the 62-bit draw. *)
      let full_buckets = Int64.(two_pow_62 / span) in
      let first_rejected = Int64.(full_buckets * span) in
      let result_of v = Int64.to_int_exn Int64.(v + min64) in
      List.iter
        [
          (* r, then whether it should be accepted *)
          (0L, true);
          (Int64.pred first_rejected, true);
          (first_rejected, false);
          (mask62, false);
        ]
        ~f:(fun (r, accepted) ->
          List.iter
            (* We test against the lsb and msb being 0, and both being 1. *)
            [ 0L; Int64.(bit_or 1L min_value) ]
            ~f:(fun extra_bits ->
              let w = Int64.(bit_or (shift_left r 1) extra_bits) in
              assert (Int64.equal (r_of_word w) r);
              (* The words that follow, via a twin state. *)
              let twin = state_with_first_word w in
              assert (Int64.equal (Xs_S.bits64 twin) w);
              let w1 = Xs_S.bits64 twin in
              let w2 = Xs_S.bits64 twin in
              let state = state_with_first_word w in
              let got = Xs_S.int_in_range state ~min ~max in
              let expected, next_word =
                if accepted then (result_of Int64.(r % span), w1)
                else begin
                  (* The retry must itself be accepted for this to be the
                     result; the words used here are fixed, so check it. *)
                  let r1 = r_of_word w1 in
                  assert (Int64.(r1 < first_rejected));
                  (result_of Int64.(r1 % span), w2)
                end
              in
              assert (got = expected);
              assert (Int64.equal (Xs_S.bits64 state) next_word))))

let%test "int_in_range raises on invalid range" =
  try
    let state = Xs_S.init () in
    ignore (Xs_S.int_in_range state ~min:10 ~max:4);
    false
  with Invalid_argument _ -> true

let%expect_test "int32incl hist" =
  let state = Xs_S.init_deterministic seed in
  let buckets = Array.init 11 ~f:(fun _ -> 0) in
  for _ = 1 to 10000 do
    let i = Xs_S.int32_in_range state ~min:4l ~max:10l |> Int32.to_int_exn in
    assert (i < 11);
    assert (i >= 4);
    buckets.(i) <- buckets.(i) + 1
  done;
  printbuckets buckets 10000;
  [%expect
    {|
    0
    0
    0
    0
    1456
    1356
    1421
    1440
    1482
    1452
    1393
    |}]

let%expect_test "int32incl" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 15 do
    let i = Xs_S.int32_in_range state ~min:4l ~max:19l in
    printf "%ld\n" i
  done;
  [%expect
    {|
    19
    11
    6
    19
    9
    14
    18
    13
    12
    14
    13
    15
    8
    8
    16
    |}]

let%test_unit
    "int32_in_range full range triggers rejection sampling (span wraps to 0)" =
  let state = Xs_S.init_deterministic seed in
  let has_neg = ref false in
  let has_pos = ref false in
  for _ = 1 to 256 do
    let r =
      Xs_S.int32_in_range state ~min:Int32.min_value ~max:Int32.max_value
    in
    assert (Int32.(r >= min_value && r <= max_value));
    if Int32.(r < 0l) then has_neg := true;
    if Int32.(r > 0l) then has_pos := true
  done;
  assert !has_neg;
  assert !has_pos

let%test_unit
    "int32_in_range half range triggers rejection sampling (span overflows \
     negative)" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 1000 do
    let r = Xs_S.int32_in_range state ~min:Int32.min_value ~max:(-1l) in
    assert (Int32.(r >= min_value && r <= -1l))
  done

let%test_unit "int32_in_range equal bounds at extremes" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 10 do
    assert (
      Int32.(
        Xs_S.int32_in_range state ~min:min_value ~max:min_value = min_value));
    assert (
      Int32.(
        Xs_S.int32_in_range state ~min:max_value ~max:max_value = max_value))
  done

let%test_unit "int32 max_int32 bound terminates and stays in range" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 1000 do
    let r = Xs_S.int32 state Int32.max_value in
    assert (Int32.(r >= 0l && r < max_value))
  done

let%test "int32 raises on non-positive bound" =
  let state = Xs_S.init () in
  (try
     ignore (Xs_S.int32 state 0l);
     false
   with Invalid_argument _ -> true)
  &&
    try
      ignore (Xs_S.int32 state (-1l));
      false
    with Invalid_argument _ -> true

let%test "int32_in_range raises on invalid range" =
  try
    let state = Xs_S.init () in
    ignore (Xs_S.int32_in_range state ~min:10l ~max:4l);
    false
  with Invalid_argument _ -> true

let%expect_test "int64incl hist" =
  let state = Xs_S.init_deterministic seed in
  let buckets = Array.init 11 ~f:(fun _ -> 0) in
  for _ = 1 to 10000 do
    let i = Xs_S.int64_in_range state ~min:4L ~max:10L |> Int64.to_int_exn in
    assert (i < 11);
    assert (i >= 4);
    buckets.(i) <- buckets.(i) + 1
  done;
  printbuckets buckets 10000;
  [%expect
    {|
    0
    0
    0
    0
    1458
    1478
    1414
    1370
    1445
    1447
    1388
    |}]

let%expect_test "int64incl" =
  let state = Xs_S.init_deterministic seed in
  let max = Int64.max_value in
  let min = Int64.( / ) max 100L in
  for _ = 1 to 15 do
    let s = Xs_S.int64_in_range state ~min ~max |> Int64.to_string in
    printf "%s\n" s
  done;
  [%expect
    {|
    7602873025362164733
    7035511519676650784
    6905405944056842707
    4016103582396849372
    3032838786034230134
    3468225672811717959
    3525369943400615138
    3708524017679508906
    9167055678361288308
    409944167341105141
    3075856011684643487
    6754319995917986208
    6559068190248100490
    8065232393103206740
    1828049645482703801
    |}]

let%test_unit
    "int64_in_range full range triggers rejection sampling (span wraps to 0)" =
  let state = Xs_S.init_deterministic seed in
  let has_neg = ref false in
  let has_pos = ref false in
  for _ = 1 to 256 do
    let r =
      Xs_S.int64_in_range state ~min:Int64.min_value ~max:Int64.max_value
    in
    assert (Int64.(r >= min_value && r <= max_value));
    if Int64.(r < 0L) then has_neg := true;
    if Int64.(r > 0L) then has_pos := true
  done;
  assert !has_neg;
  assert !has_pos

let%test_unit
    "int64_in_range half range triggers rejection sampling (span overflows \
     negative)" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 1000 do
    let r = Xs_S.int64_in_range state ~min:Int64.min_value ~max:(-1L) in
    assert (Int64.(r >= min_value && r <= -1L))
  done

let%test_unit "int64_in_range equal bounds at extremes" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 10 do
    assert (
      Int64.(
        Xs_S.int64_in_range state ~min:min_value ~max:min_value = min_value));
    assert (
      Int64.(
        Xs_S.int64_in_range state ~min:max_value ~max:max_value = max_value))
  done

let%test_unit "int64 max_int64 bound terminates and stays in range" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 1000 do
    let r = Xs_S.int64 state Int64.max_value in
    assert (Int64.(r >= 0L && r < max_value))
  done

let%test "int64 raises on non-positive bound" =
  let state = Xs_S.init () in
  (try
     ignore (Xs_S.int64 state 0L);
     false
   with Invalid_argument _ -> true)
  &&
    try
      ignore (Xs_S.int64 state (-1L));
      false
    with Invalid_argument _ -> true

let%test "int64_in_range raises on invalid range" =
  try
    let state = Xs_S.init () in
    ignore (Xs_S.int64_in_range state ~min:10L ~max:4L);
    false
  with Invalid_argument _ -> true

let%test_unit "nativeint returns values in [0, bound)" =
  let state = Xs_S.init_deterministic seed in
  let bound = 100n in
  for _ = 1 to 10000 do
    let r = Xs_S.nativeint state bound in
    assert (Nativeint.(r >= 0n && r < bound))
  done

let%test_unit "nativeint_in_range returns values in [min, max]" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 10000 do
    let r = Xs_S.nativeint_in_range state ~min:4n ~max:19n in
    assert (Nativeint.(r >= 4n && r <= 19n))
  done

let%test "nativeint_in_range raises on invalid range" =
  try
    let state = Xs_S.init () in
    ignore (Xs_S.nativeint_in_range state ~min:10n ~max:4n);
    false
  with Invalid_argument _ -> true

let%test_unit
    "nativeint_in_range full range triggers rejection sampling (span wraps to \
     0)" =
  let state = Xs_S.init_deterministic seed in
  let has_neg = ref false in
  let has_pos = ref false in
  for _ = 1 to 256 do
    let r =
      Xs_S.nativeint_in_range state ~min:Nativeint.min_value
        ~max:Nativeint.max_value
    in
    assert (Nativeint.(r >= min_value && r <= max_value));
    if Nativeint.(r < 0n) then has_neg := true;
    if Nativeint.(r > 0n) then has_pos := true
  done;
  assert !has_neg;
  assert !has_pos

let%test_unit "nativeint_in_range equal bounds at extremes" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 10 do
    assert (
      Nativeint.(
        Xs_S.nativeint_in_range state ~min:min_value ~max:min_value = min_value));
    assert (
      Nativeint.(
        Xs_S.nativeint_in_range state ~min:max_value ~max:max_value = max_value))
  done

(* --- Float function tests --- *)

let%expect_test "float hist" =
  let state = Xs_S.init_deterministic seed in
  let buckets = Array.init 11 ~f:(fun _ -> 0) in
  for _ = 1 to 10000 do
    let r = Xs_S.float state 11. in
    assert (Float.(r < 11.0));
    assert (Float.(r >= 0.0));
    let i = Float.to_int r in
    assert (i < 11);
    buckets.(i) <- buckets.(i) + 1
  done;
  printbuckets buckets 10000;
  [%expect
    {|
    977
    896
    927
    875
    879
    936
    937
    867
    881
    939
    886
    |}]

let%expect_test "float" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 15 do
    let s = Xs_S.float state 10000000. |> Float.to_string in
    printf "%s\n" s
  done;
  [%expect
    {|
    857555.95295460953
    6446590.7181611983
    436577.87601690146
    5308427.2237271834
    3104113.9572710483
    8050987.23145103
    7494854.0995313386
    480117.78021376795
    625697.81563214131
    8499563.0024785567
    7776560.6057027
    325695.28711478005
    3064613.22653673
    645514.17442654027
    985882.85600238468
    |}]

let%expect_test "float_strict hist" =
  let state = Xs_S.init_deterministic seed in
  let buckets = Array.init 11 ~f:(fun _ -> 0) in
  for _ = 1 to 10000 do
    let r = Xs_S.float_strict state 11. in
    assert (Float.(r < 11.0));
    assert (Float.(r >= 0.0));
    let i = Float.to_int r in
    assert (i < 11);
    buckets.(i) <- buckets.(i) + 1
  done;
  printbuckets buckets 10000;
  [%expect
    {|
    977
    896
    927
    875
    879
    936
    937
    867
    881
    939
    886
    |}]

let%expect_test "float_strict" =
  let state = Xs_S.init_deterministic seed in
  for _ = 1 to 15 do
    let s = Xs_S.float_strict state 10000000. |> Float.to_string in
    printf "%s\n" s
  done;
  [%expect
    {|
    857555.95295460953
    6446590.7181611983
    436577.87601690146
    5308427.2237271834
    3104113.9572710483
    8050987.23145103
    7494854.0995313386
    480117.78021376795
    625697.81563214131
    8499563.0024785567
    7776560.6057027
    325695.28711478005
    3064613.22653673
    645514.17442654027
    985882.85600238468
    |}]

let%expect_test
    "float with smallest-subnormal bound: neither endpoint reliably excluded" =
  (* Regression test: `float`/`uniform_float` share stdlib Random's
     rawfloat *. bound formula (no rejection sampling after the
     multiplication: see lib/rand_core.ml). For a bound this tiny there
     is no representable value strictly between 0 and bound at all, so
     every draw lands on one endpoint or the other. This is the same
     failure family as float_range's adjacent-min/max case below, just at
     its most extreme possible instance.

     This is not a bug to resolve: it is `float`'s documented contract (see
     lib/xoshiro_simd.mli) that 0/bound can leak through at extreme magnitudes.
     Callers who need 0 excluded use `float_strict`, which never returns 0. For
     a bound this small it can still return the bound itself (the bound is the
     only nonzero float of its sign at or below it); see the pinned-draw test
     "float_strict ... largest draw" below. *)
  let state = Xs_S.init_deterministic seed in
  let bound =
    Int64.float_of_bits 0x1L
    (* smallest positive subnormal double *)
  in
  let hit_zero = ref 0 and hit_bound = ref 0 and hit_other = ref 0 in
  for _ = 1 to 1000 do
    let v = Xs_S.float state bound in
    if Float.(v = 0.0) then Int.incr hit_zero
    else if Float.(v = bound) then Int.incr hit_bound
    else Int.incr hit_other
  done;
  printf "hit_zero=%d hit_bound=%d hit_other=%d (out of 1000)\n" !hit_zero
    !hit_bound !hit_other;
  [%expect {| hit_zero=505 hit_bound=495 hit_other=0 (out of 1000) |}]

(* float_strict rejects the bounds that would make its draw loop spin forever or
   have no meaning: 0 (every product is 0), and NaN and the infinities (the
   product is the bound itself, or NaN). Each rejection here is also a check
   that the call returns at all. *)
let%test "float_strict rejects unusable bounds, each with its own message" =
  let state = Xs_S.init_deterministic seed in
  let raises_with msg bound =
    match Xs_S.float_strict state bound with
    | _ -> false
    | exception Invalid_argument m -> String.equal m msg
  in
  List.for_all [ 0.; -0. ]
    ~f:(raises_with "Xoshiro_simd.float_strict: zero bound")
  && raises_with "Xoshiro_simd.float_strict: NaN bound" Float.nan
  && List.for_all
       [ Float.infinity; Float.neg_infinity ]
       ~f:(raises_with "Xoshiro_simd.float_strict: infinite bound")

(* The smallest subnormal bound is left out: only about half of the draws map
   to a nonzero result there, so it terminates, but a hang would be the failure
   mode if the loop logic were ever wrong. The pinned-draw test below covers it
   without a loop. *)
let%test_unit "float_strict accepts the extreme usable bounds" =
  let state = Xs_S.init_deterministic seed in
  (* Returns true if the bound is high enough that it can never be itself a
     result. *)
  let strict_interior_expected bound =
    Float.(abs bound > min_positive_normal_value)
  in
  let ok bound r =
    (* Nonzero (sign_exn gives Zero for 0), the bound's sign, no larger than the
       bound, and strictly inside it when strict_interior_expected. *)
    let same_sign = Sign.equal (Float.sign_exn r) (Float.sign_exn bound) in
    let within = Float.(abs r <= abs bound) in
    same_sign && within
    && ((not (strict_interior_expected bound)) || Float.(abs r < abs bound))
  in
  let bounds =
    let bounds =
      [
        Int64.float_of_bits 0x2L;
        Int64.float_of_bits 0x000FFFFFFFFFFFFFL;
        Float.min_positive_normal_value;
        Stdlib.Float.succ Float.min_positive_normal_value;
        Float.max_finite_value;
      ]
    in
    bounds @ List.map bounds ~f:Float.neg
  in
  List.iter bounds ~f:(fun bound ->
      for _ = 1 to 1000 do
        assert (ok bound (Xs_S.float_strict state bound))
      done)

(* float_strict does not compare its result with the bound. That is safe for
   every bound above the smallest normal float, 2^-1022, because the product of
   the largest possible draw, 1 - 2^-53, and such a bound rounds to a float
   below the bound. At or below 2^-1022 it does not: the spacing below the
   bound is no longer finer than the spacing above it (subnormals all share
   2^-1074), so the product rounds back to the bound, and for 2^-1022 itself it
   is an exact tie that goes to the even float, the bound.

   This pins that largest draw (lane 0 all ones makes the first draw the
   maximum, as in the bulk fill tests below) and shows where the line sits:
   - 2^-1022 is the largest bound for which the draw returns the bound, as does
     every subnormal bound, including the smallest one, without any retry
     (a nonzero first draw is returned at once, so nothing here can loop);
   - the next float up, the smallest bound for which the draw is below the
     bound, gets exactly 2^-1022, the float just below it;
   - the same holds for each float in a sweep on both sides of the line, and
     for powers of two and the largest bound. *)
(* On i386, OCaml and the Plain C backend do float arithmetic on the x87 FPU,
   which rounds each result to a 64-bit mantissa and then again to 53 when it
   is stored. That sometimes gives a different result from the single rounding
   every other architecture does, so the tests below that pin exact rounding at
   a tie are skipped there. *)
let x87_double_rounding = String.(Config.architecture = "i386")

let%test_unit
    "float_strict at the largest draw: the bound is returned only at or below \
     the smallest normal" =
  if not x87_double_rounding then (
    let desired_state = [| 0L; 0L; 0L; 0xffffffffffffffffL |] in
    let seed_string = Seed_reverse.seed_string_for_state desired_state in
    let draw bound = Xs_S.float_strict (Xs_S.init_string seed_string) bound in
    let min_normal = Float.min_positive_normal_value in
    let min_normal_bits = 0x0010000000000000L in
    let at_offset k = Int64.(float_of_bits (min_normal_bits + k)) in
    let is_below bound r =
      if Float.(bound > 0.) then Float.(r > 0. && r < bound)
      else Float.(r < 0. && r > bound)
    in
    let powers_of_two =
      List.init 2045 ~f:(fun i -> Float.ldexp 1. (i - 1021))
    in
    let signed l = l @ List.map l ~f:Float.neg in
    (* The line itself, through the library. *)
    assert (
      List.for_all (signed [ min_normal ]) ~f:(fun b -> Float.equal (draw b) b));
    assert (
      List.for_all
        (signed
           [
             Int64.float_of_bits 0x1L; Int64.float_of_bits 0x2L; at_offset (-1L);
           ])
        ~f:(fun b -> Float.equal (draw b) b));
    assert (
      List.for_all
        (signed [ Stdlib.Float.succ min_normal ])
        ~f:(fun b ->
          is_below b (draw b)
          && Float.equal (draw b) (Stdlib.Float.copy_sign min_normal b)));
    (* Sweeps on both sides, by plain arithmetic. *)
    assert (
      List.for_all (List.init 100_000 ~f:Int64.of_int) ~f:(fun k ->
          let above = at_offset Int64.(k + 1L) in
          let below = at_offset (Int64.neg k) in
          Float.(draw above < above) && Float.equal (draw below) below));
    (* Powers of two, one float either side of each, and the largest bound. *)
    assert (
      List.for_all
        (signed
           (List.concat_map powers_of_two ~f:(fun p ->
                [ p; Stdlib.Float.succ p; Stdlib.Float.pred p ])
           @ [ Float.max_finite_value ]))
        ~f:(fun b -> is_below b (draw b))))

(* The same line for float, which the .mli states directly ("bound will not be
   returned exactly except when bound <= Float.min_float"). float_strict's first
   nonzero draw is the same product, but float has no retry loop of its own to
   reach it through, so it gets its own check. *)
let%test_unit
    "float at the largest draw: the bound is returned only at or below the \
     smallest normal" =
  if not x87_double_rounding then (
    let desired_state = [| 0L; 0L; 0L; 0xffffffffffffffffL |] in
    let seed_string = Seed_reverse.seed_string_for_state desired_state in
    let draw bound = Xs_S.float (Xs_S.init_string seed_string) bound in
    let min_normal = Float.min_positive_normal_value in
    let returned_at = [ min_normal; Int64.float_of_bits 0x1L ] in
    let below_at =
      [ Stdlib.Float.succ min_normal; 1.0; 3.0; Float.max_finite_value ]
    in
    let signed l = l @ List.map l ~f:Float.neg in
    List.iter (signed returned_at) ~f:(fun b ->
        if not (Float.equal (draw b) b) then
          Printf.failwithf "float %h returned %h, expected the bound" b (draw b)
            ());
    List.iter (signed below_at) ~f:(fun b ->
        let r = draw b in
        if
          Float.(abs r >= abs b)
          || Bool.(Stdlib.Float.sign_bit r <> Stdlib.Float.sign_bit b)
        then
          Printf.failwithf "float %h returned %h, expected below the bound" b r
            ()))

let%expect_test
    "float_range with adjacent min/max: no representable value between them" =
  (* Regression test for a bug found via property-based fuzzing
     (afl/ocaml_fuzz/): min/max here are the two closest possible adjacent
     floats (bit patterns differing by 1), so there is no representable
     value strictly between them: float_range's documented "strictly
     between min and max" contract cannot be satisfied by any
     implementation using this min +. uniform_float * (max - min)
     approach.

     This is no longer a bug to resolve: it's now `float_range`'s
     documented contract (see lib/xoshiro_simd.mli) that min/max can leak
     through at extreme magnitudes. Callers who need a real guarantee use
     `float_range_strict`, which raises Invalid_argument on this exact
     min/max pair instead of returning either endpoint -- see the test
     immediately below. *)
  let state = Xs_S.init_deterministic seed in
  let min_v = Int64.float_of_bits 0x0505050505050504L in
  let max_v = Int64.float_of_bits 0x0505050505050505L in
  let hit_min = ref 0 and hit_max = ref 0 and hit_other = ref 0 in
  for _ = 1 to 1000 do
    let v = Xs_S.float_range state ~min:min_v ~max:max_v in
    if Float.(v = min_v) then Int.incr hit_min
    else if Float.(v = max_v) then Int.incr hit_max
    else Int.incr hit_other
  done;
  printf "hit_min=%d hit_max=%d hit_other=%d (out of 1000)\n" !hit_min !hit_max
    !hit_other;
  [%expect {| hit_min=505 hit_max=495 hit_other=0 (out of 1000) |}]

let%test "float_range_strict raises on adjacent min/max" =
  (* Same min/max as the test above: min and max are adjacent
     representable floats, so no value can lie strictly between them, and
     float_range_strict must reject the input rather than silently
     returning either endpoint. *)
  let state = Xs_S.init_deterministic seed in
  let min_v = Int64.float_of_bits 0x0505050505050504L in
  let max_v = Int64.float_of_bits 0x0505050505050505L in
  try
    ignore (Xs_S.float_range_strict state ~min:min_v ~max:max_v);
    false
  with Invalid_argument _ -> true

let%expect_test "float_range hist" =
  let state = Xs_S.init_deterministic seed in
  let buckets = Array.init 11 ~f:(fun _ -> 0) in
  for _ = 1 to 10000 do
    let r = Xs_S.float_range state ~min:4.5 ~max:9.5 in
    assert (Float.(r < 9.5));
    assert (Float.(r >= 4.5));
    let i = Float.to_int r in
    assert (i < 11);
    buckets.(i) <- buckets.(i) + 1
  done;
  printbuckets buckets 10000;
  [%expect
    {|
    0
    0
    0
    0
    1066
    1988
    1952
    2040
    1983
    971
    0
    |}]

let%expect_test "float_range_strict hist" =
  let state = Xs_S.init_deterministic seed in
  let buckets = Array.init 11 ~f:(fun _ -> 0) in
  for _ = 1 to 10000 do
    let r = Xs_S.float_range_strict state ~min:4.5 ~max:9.5 in
    assert (Float.(r < 9.5));
    assert (Float.(r >= 4.5));
    let i = Float.to_int r in
    assert (i < 11);
    buckets.(i) <- buckets.(i) + 1
  done;
  printbuckets buckets 10000;
  [%expect
    {|
    0
    0
    0
    0
    1066
    1988
    1952
    2040
    1983
    971
    0
    |}]

let%expect_test "float_range" =
  let state = Xs_S.init_deterministic seed in
  let max = 9000. in
  let min = -9000. in
  for _ = 1 to 15 do
    let s = Xs_S.float_range state ~min ~max |> Float.to_string in
    printf "%s\n" s
  done;
  [%expect
    {|
    -7456.3992846817027
    2603.8632926901573
    -8214.1598231695771
    555.16900270892984
    -3412.5948769121123
    5491.7770166118535
    4490.73737915641
    -8135.7879956152174
    -7873.7439318621455
    6299.2134044614031
    4997.80909026486
    -8413.7484831933962
    -3483.696192233886
    -7838.0744860322275
    -7225.4108591957074
    |}]

let%expect_test "float_range_strict" =
  let state = Xs_S.init_deterministic seed in
  let max = 9000. in
  let min = -9000. in
  for _ = 1 to 15 do
    let s = Xs_S.float_range_strict state ~min ~max |> Float.to_string in
    printf "%s\n" s
  done;
  [%expect
    {|
    -7456.3992846817027
    2603.8632926901573
    -8214.1598231695771
    555.16900270892984
    -3412.5948769121123
    5491.7770166118535
    4490.73737915641
    -8135.7879956152174
    -7873.7439318621455
    6299.2134044614031
    4997.80909026486
    -8413.7484831933962
    -3483.696192233886
    -7838.0744860322275
    -7225.4108591957074
    |}]

let%expect_test "fill_float32_bigarray" =
  let state = Xs_S.init_deterministic seed in
  let n = 15 in
  let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n in
  Xs_S.fill_float32_bigarray state ba;
  for i = 0 to n - 1 do
    let s = Bigarray.Array1.get ba i in
    printf "%s\n" (Float_fmt.sprintf_g 9 s)
  done;
  [%expect
    {|
    0.243016362
    0.0857555866
    0.519950032
    0.644659042
    0.964973748
    0.0436577797
    0.910432935
    0.530842721
    0.953629673
    0.310411394
    0.959575891
    0.805098712
    0.577862799
    0.749485373
    0.424625158
    |}]

let%expect_test "fill_float64_bigarray" =
  let state = Xs_S.init_deterministic seed in
  let n = 15 in
  let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
  Xs_S.fill_float64_bigarray state ba;
  for i = 0 to n - 1 do
    let s = Bigarray.Array1.get ba i |> Float.to_string in
    printf "%s\n" s
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

let%expect_test "fill_float32_range_bigarray hist" =
  let state = Xs_S.init_deterministic seed in
  let n = 10000 in
  let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n in
  Xs_S.fill_float32_range_bigarray state ba ~min:4.5 ~max:9.5;
  let buckets = Array.init 11 ~f:(fun _ -> 0) in
  for i = 0 to n - 1 do
    let r = Bigarray.Array1.get ba i in
    assert (Float.(r < 9.5));
    assert (Float.(r >= 4.5));
    let bucket = Float.to_int r in
    assert (bucket < 11);
    buckets.(bucket) <- buckets.(bucket) + 1
  done;
  printbuckets buckets n;
  [%expect
    {|
    0
    0
    0
    0
    995
    1956
    1986
    2042
    2012
    1009
    0
    |}]

let%expect_test "fill_float64_range_bigarray hist" =
  let state = Xs_S.init_deterministic seed in
  let n = 10000 in
  let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
  Xs_S.fill_float64_range_bigarray state ba ~min:4.5 ~max:9.5;
  let buckets = Array.init 11 ~f:(fun _ -> 0) in
  for i = 0 to n - 1 do
    let r = Bigarray.Array1.get ba i in
    assert (Float.(r < 9.5));
    assert (Float.(r >= 4.5));
    let bucket = Float.to_int r in
    assert (bucket < 11);
    buckets.(bucket) <- buckets.(bucket) + 1
  done;
  printbuckets buckets n;
  [%expect
    {|
    0
    0
    0
    0
    1066
    1988
    1952
    2040
    1983
    971
    0
    |}]

let%expect_test "fill_float32_range_bigarray" =
  let state = Xs_S.init_deterministic seed in
  let n = 15 in
  let max = 9000. in
  let min = -9000. in
  let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n in
  Xs_S.fill_float32_range_bigarray state ba ~min ~max;
  for i = 0 to n - 1 do
    let s = Bigarray.Array1.get ba i in
    printf "%s\n" (Float_fmt.sprintf_g 9 s)
  done;
  [%expect
    {|
    -4625.70557
    -7456.39941
    359.100586
    2603.8623
    8369.52734
    -8214.16016
    7387.79297
    555.168945
    8165.33398
    -3412.59473
    8272.36523
    5491.77637
    1401.53027
    4490.73633
    -1356.74707
    |}]

let%expect_test "fill_float64_range_bigarray" =
  let state = Xs_S.init_deterministic seed in
  let n = 15 in
  let max = 9000. in
  let min = -9000. in
  let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
  Xs_S.fill_float64_range_bigarray state ba ~min ~max;
  for i = 0 to n - 1 do
    let s = Bigarray.Array1.get ba i |> Float.to_string in
    printf "%s\n" s
  done;
  [%expect
    {|
    -7456.3992846817027
    2603.8632926901573
    -8214.1598231695771
    555.16900270892984
    -3412.5948769121123
    5491.7770166118535
    4490.73737915641
    -8135.7879956152174
    -7873.7439318621455
    6299.2134044614031
    4997.80909026486
    -8413.7484831933962
    -3483.696192233886
    -7838.0744860322275
    -7225.4108591957074
    |}]

let%test_unit "fill_float32_range_bigarray with min = max: every element is min"
    =
  let state = Xs_S.init_deterministic seed in
  let n = 100 in
  let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n in
  Xs_S.fill_float32_range_bigarray state ba ~min:4.5 ~max:4.5;
  for i = 0 to n - 1 do
    assert (Float.equal (Bigarray.Array1.get ba i) 4.5)
  done

let%test_unit "fill_float64_range_bigarray with min = max: every element is min"
    =
  let state = Xs_S.init_deterministic seed in
  let n = 100 in
  let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
  Xs_S.fill_float64_range_bigarray state ba ~min:4.5 ~max:4.5;
  for i = 0 to n - 1 do
    assert (Float.equal (Bigarray.Array1.get ba i) 4.5)
  done

(* Demonstrates the "never zero, for log transforms" use case this feature
   was designed around:
   min + (max - min) *. u is always >= min, so passing a strictly positive
   min guarantees strictly positive output with no rejection sampling. This
   is just an example of how to produce strictly positive output: real use
   would probably choose a small normal float minimum. This also isn't
   exhaustive, and can't prove that the output is always positive: that test
   is below, in the "Edge testing floatrange functions" section. *)
let%test_unit
    "fill_float32_range_bigarray with positive min never produces non-positive \
     values" =
  let state = Xs_S.init_deterministic seed in
  let n = 10000 in
  let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n in
  let min =
    Int32.float_of_bits 1l
    (* smallest positive subnormal float32 *)
  in
  Xs_S.fill_float32_range_bigarray state ba ~min ~max:1.0;
  for i = 0 to n - 1 do
    assert (Float.(Bigarray.Array1.get ba i > 0.0))
  done

(* As above, just an example, not a proof. *)
let%test_unit
    "fill_float64_range_bigarray with positive min never produces non-positive \
     values" =
  let state = Xs_S.init_deterministic seed in
  let n = 10000 in
  let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
  let min = Float.min_positive_subnormal_value in
  Xs_S.fill_float64_range_bigarray state ba ~min ~max:1.0;
  for i = 0 to n - 1 do
    assert (Float.(Bigarray.Array1.get ba i > 0.0))
  done

(* Neither bound is a float32 here, and in float32 the range only holds about
   a hundred values, so rounding decides where the output lands. The output can
   at best be bounded by min and max rounded to float32, and it must not go
   past those. The largest draw (u24 = 2^24 - 1) is forced through Seed_reverse:
   lane 0's xoshiro256+ output is s0 + s3, and element 0 takes its low 32 bits.
   That draw came out one float32 above both max and float32(max) on every
   backend, and so did about 14% of the elements of a random fill. *)
let%test_unit
    "fill_float32_range_bigarray stays within min and max rounded to float32" =
  let to_f32 x = Stdlib.Int32.float_of_bits (Stdlib.Int32.bits_of_float x) in
  let min = 0x1.9786814aa49e8p+20 and max = 0x1.978684c6c8da8p+20 in
  let lo = to_f32 min and hi = to_f32 max in
  let check_all ba =
    for i = 0 to Bigarray.Array1.dim ba - 1 do
      let v = Bigarray.Array1.get ba i in
      if Float.(v < lo || v > hi) then
        Printf.failwithf "element %d = %h, outside [%h, %h]" i v lo hi ()
    done
  in
  let seed_string =
    Seed_reverse.seed_string_for_state [| 0xffffffffL; 1L; 1L; 0L |]
  in
  List.iter (Xoshiro_simd.available_backends ()) ~f:(fun backend ->
      let state = Xs_S.init_string ~backend seed_string in
      let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 1 in
      Xs_S.fill_float32_range_bigarray state ba ~min ~max;
      check_all ba);
  let state = Xs_S.init_deterministic seed in
  let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 10_000 in
  Xs_S.fill_float32_range_bigarray state ba ~min ~max;
  check_all ba

let%test "fill_float32_range_bigarray raises when min is NaN" =
  try
    let state = Xs_S.init () in
    let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 4 in
    Xs_S.fill_float32_range_bigarray state ba ~min:Float.nan ~max:1.0;
    false
  with Invalid_argument _ -> true

let%test "fill_float32_range_bigarray raises when max is NaN" =
  try
    let state = Xs_S.init () in
    let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 4 in
    Xs_S.fill_float32_range_bigarray state ba ~min:0.0 ~max:Float.nan;
    false
  with Invalid_argument _ -> true

let%test "fill_float32_range_bigarray raises when min > max" =
  try
    let state = Xs_S.init () in
    let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 4 in
    Xs_S.fill_float32_range_bigarray state ba ~min:1.0 ~max:0.0;
    false
  with Invalid_argument _ -> true

let%test
    "fill_float32_range_bigarray raises when max -. min overflows to infinity \
     as a double" =
  try
    let state = Xs_S.init () in
    let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 4 in
    Xs_S.fill_float32_range_bigarray state ba ~min:(-.Float.max_finite_value)
      ~max:Float.max_finite_value;
    false
  with Invalid_argument _ -> true

(* min and max are each finite as doubles and individually representable in
   float32, but their difference (6e38) exceeds FLT_MAX (~3.4e38). *)
let%test
    "fill_float32_range_bigarray raises when the range overflows float32 but \
     not double precision" =
  try
    let state = Xs_S.init () in
    let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 4 in
    Xs_S.fill_float32_range_bigarray state ba ~min:(-3e38) ~max:3e38;
    false
  with Invalid_argument _ -> true

(* max and max - min both fit in float32, but min (-3.5e38) is below -FLT_MAX.
   Only a negative min can be the sole value out of float32 range: a positive
   one would make max out of range too. The output would be -infinity whenever
   the draw is small. *)
let%test "fill_float32_range_bigarray raises when only min overflows float32" =
  try
    let state = Xs_S.init () in
    let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 4 in
    Xs_S.fill_float32_range_bigarray state ba ~min:(-3.5e38) ~max:(-3.4e38);
    false
  with Invalid_argument _ -> true

(* Both max - min and min overflow float32, in opposite directions, so the
   largest output would be infinity + -infinity = NaN. A combined check written
   as [is_infinite] rather than [not is_finite] would let this through. *)
let%test
    "fill_float32_range_bigarray raises when max -. min and min both overflow \
     float32" =
  try
    let state = Xs_S.init () in
    let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 4 in
    Xs_S.fill_float32_range_bigarray state ba ~min:(-3.5e38) ~max:3e38;
    false
  with Invalid_argument _ -> true

(* min and max - min both fit in float32, but max itself is beyond FLT_MAX
   (~3.4028e38) once rounded to float32. The last pair is the smallest such max:
   FLT_MAX + 2^103 is halfway between FLT_MAX and 2^128 and rounds to even,
   i.e. to infinity. *)
let%test "fill_float32_range_bigarray raises for a max beyond float32 range" =
  let flt_max = Stdlib.Int32.float_of_bits 0x7f7fffffl in
  List.for_all
    [ (3.0e38, 3.5e38); (3.3e38, 3.45e38); (flt_max, flt_max +. 0x1p103) ]
    ~f:(fun (min, max) ->
      let state = Xs_S.init_deterministic seed in
      let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 4 in
      match Xs_S.fill_float32_range_bigarray state ba ~min ~max with
      | exception Invalid_argument _ -> true
      | () -> false)

(* Rounded to float32, the bounds are 2^127 + 2^104 and -(2^127 - 2^104), whose
   difference 2^128 overflows. The unrounded difference is below the overflow
   threshold (FLT_MAX + 2^103) and rounds to FLT_MAX, so the raise depends on
   computing the scale from the rounded bounds. *)
let%test
    "fill_float32_range_bigarray raises when only the rounded bounds' \
     difference overflows float32" =
  let max = 0x1p127 +. 0x1p103 +. 0x1p80 in
  let min = -.(0x1p127 -. 0x1p104 -. 0x1p102 +. 0x1p81) in
  let state = Xs_S.init_deterministic seed in
  let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 4 in
  match Xs_S.fill_float32_range_bigarray state ba ~min ~max with
  | exception Invalid_argument _ -> true
  | () -> false

(* min and max are both within half a float32 ULP (2^103) of FLT_MAX, so both
   round to it and the range is empty. Half the draws used to come out infinite
   on every backend, when min rounded up to FLT_MAX but the scale was taken from
   the unrounded max -. min. *)
let%test
    "fill_float32_range_bigarray returns FLT_MAX when min and max round to it" =
  let flt_max = Stdlib.Int32.float_of_bits 0x7f7fffffl in
  let min = flt_max -. Float.ldexp 1. 103 +. Float.ldexp 1. 80 in
  let max = flt_max +. Float.ldexp 1. 103 -. Float.ldexp 1. 80 in
  List.for_all (Xoshiro_simd.available_backends ()) ~f:(fun backend ->
      let state = Xs_S.init_deterministic ~backend seed in
      let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 1000 in
      Xs_S.fill_float32_range_bigarray state ba ~min ~max;
      List.for_all (List.range 0 1000) ~f:(fun i ->
          Float.equal (Bigarray.Array1.get ba i) flt_max))

let%test "fill_float64_range_bigarray raises when min is NaN" =
  try
    let state = Xs_S.init () in
    let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout 4 in
    Xs_S.fill_float64_range_bigarray state ba ~min:Float.nan ~max:1.0;
    false
  with Invalid_argument _ -> true

let%test "fill_float64_range_bigarray raises when max is NaN" =
  try
    let state = Xs_S.init () in
    let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout 4 in
    Xs_S.fill_float64_range_bigarray state ba ~min:0.0 ~max:Float.nan;
    false
  with Invalid_argument _ -> true

let%test "fill_float64_range_bigarray raises when min > max" =
  try
    let state = Xs_S.init () in
    let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout 4 in
    Xs_S.fill_float64_range_bigarray state ba ~min:1.0 ~max:0.0;
    false
  with Invalid_argument _ -> true

let%test
    "fill_float64_range_bigarray raises when max -. min overflows to infinity" =
  try
    let state = Xs_S.init () in
    let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout 4 in
    Xs_S.fill_float64_range_bigarray state ba ~min:(-.Float.max_finite_value)
      ~max:Float.max_finite_value;
    false
  with Invalid_argument _ -> true

(* With min = max = +/-infinity, max -. min is NaN, which slips past a
   "range is infinite" check written as an ordering comparison (NaN compares
   false). Raises Invalid_argument. *)
let%test "fill_float64_range_bigarray with min = max = infinity raises" =
  List.for_all [ Float.infinity; Float.neg_infinity ] ~f:(fun bound ->
      let state = Xs_S.init_deterministic seed in
      let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout 100 in
      match Xs_S.fill_float64_range_bigarray state ba ~min:bound ~max:bound with
      | exception Invalid_argument _ -> true
      | () -> false)

(* Same NaN-range hole as the bulk fill above, in the scalar function. The
   strict variant doesn't need a specific check for the above case: with no
   float strictly between min and max it raises "unsatisfiable range" for every
   min = max, infinite or not. *)
let%test "float_range with min = max = infinity raises or returns min" =
  List.for_all [ Float.infinity; Float.neg_infinity ] ~f:(fun bound ->
      let state = Xs_S.init_deterministic seed in
      match Xs_S.float_range state ~min:bound ~max:bound with
      | exception Invalid_argument _ -> true
      | _ -> false)

let%test "float_range_strict with min = max = infinity raises" =
  List.for_all [ Float.infinity; Float.neg_infinity ] ~f:(fun bound ->
      let state = Xs_S.init_deterministic seed in
      match Xs_S.float_range_strict state ~min:bound ~max:bound with
      | exception Invalid_argument _ -> true
      | _ -> false)

(* The bound checks of float_range and float_range_strict are each one combined
   condition, with the message chosen only on the failure path (rand_impl.ml).
   These pin down which input gets which message, including the order the
   checks run in (a decreasing pair with an infinite span reports the span, not
   the order), and the inputs just inside the accepted region. *)
let range_message f ~min ~max =
  let state = Xs_S.init_deterministic seed in
  match (f : Xs_S.t -> min:float -> max:float -> float) state ~min ~max with
  | _ -> None
  | exception Invalid_argument m -> Some m

let succ_f = Stdlib.Float.succ
let max_f = Float.max_finite_value
let smallest = Int64.float_of_bits 0x1L

let with_prefix fn reason = "Xoshiro_simd." ^ fn ^ ": " ^ reason

(* Rejected by both functions, with the same reason. Each message is the reason
   prefixed with the name of the function that raised it. *)
let rejected_by_both =
  [
    (Float.nan, 1., "min is NaN");
    (1., Float.nan, "max is NaN");
    (Float.nan, Float.nan, "min is NaN");
    (Float.neg_infinity, 0., "range is infinite");
    (0., Float.infinity, "range is infinite");
    (Float.neg_infinity, Float.infinity, "range is infinite");
    (* decreasing too, but the span check comes first *)
    (Float.infinity, Float.neg_infinity, "range is infinite");
    (* finite bounds whose span overflows *)
    (-.max_f, max_f, "range is infinite");
    (-.max_f, 0.5 *. max_f, "range is infinite");
    (2., 1., "min must not be greater than max");
    (0., -1., "min must not be greater than max");
    (succ_f 1., 1., "min must not be greater than max");
  ]

let%test
    "float_range and float_range_strict reject bad bounds with the right \
     message" =
  List.for_all rejected_by_both ~f:(fun (min, max, reason) ->
      Option.equal String.equal
        (range_message Xs_S.float_range ~min ~max)
        (Some (with_prefix "float_range" reason))
      && Option.equal String.equal
           (range_message Xs_S.float_range_strict ~min ~max)
           (Some (with_prefix "float_range_strict" reason)))

(* min = max = +/-infinity: the span is NaN. float_range says so; the strict
   function has no gap to draw from and says that instead. *)
let%test "min = max = infinity gives each function its own message" =
  List.for_all [ Float.infinity; Float.neg_infinity ] ~f:(fun b ->
      Option.equal String.equal
        (range_message Xs_S.float_range ~min:b ~max:b)
        (Some (with_prefix "float_range" "range is NaN"))
      && Option.equal String.equal
           (range_message Xs_S.float_range_strict ~min:b ~max:b)
           (Some (with_prefix "float_range_strict" "unsatisfiable range")))

let%test "float_range_strict rejects ranges with no value strictly inside" =
  List.for_all
    [
      (5., 5.);
      (0., -0.);
      (-0., 0.);
      (1., succ_f 1.);
      (-.max_f, -.max_f);
      (max_f, max_f);
      (-.smallest, -0.);
    ]
    ~f:(fun (min, max) ->
      Option.equal String.equal
        (range_message Xs_S.float_range_strict ~min ~max)
        (Some (with_prefix "float_range_strict" "unsatisfiable range")))

(* min = max is a valid range for float_range and returns min; -0. and 0. are
   the same bound. A span of exactly max_float is the widest that is accepted. *)
let%test "float_range accepts min = max and the widest finite span" =
  let state = Xs_S.init_deterministic seed in
  let draws min max =
    List.init 200 ~f:(fun _ -> Xs_S.float_range state ~min ~max)
  in
  List.for_all [ 5.; -3.; max_f; -.max_f; smallest ] ~f:(fun b ->
      List.for_all (draws b b) ~f:(fun r -> Float.equal r b))
  && List.for_all
       [ (0., -0.); (-0., 0.) ]
       ~f:(fun (min, max) ->
         List.for_all (draws min max) ~f:(fun r -> Float.(r = 0.)))
  && List.for_all
       [ (0., max_f); (-.max_f, 0.); (-3., 7.); (-1e300, 1e300) ]
       ~f:(fun (lo, hi) ->
         List.for_all (draws lo hi) ~f:(fun r -> Float.(r >= lo && r <= hi)))

(* The tightest accepted ranges for float_range_strict: exactly one value
   strictly inside, so every draw has to land on it (the retry loop is what
   gets it there), then the widest spans. *)
let%test "float_range_strict accepts the tightest and the widest ranges" =
  let state = Xs_S.init_deterministic seed in
  let draws min max =
    List.init 200 ~f:(fun _ -> Xs_S.float_range_strict state ~min ~max)
  in
  List.for_all
    (draws 1. (succ_f (succ_f 1.)))
    ~f:(fun r -> Float.equal r (succ_f 1.))
  && List.for_all
       (draws (-0.) (2. *. smallest))
       ~f:(fun r -> Float.equal r smallest)
  && List.for_all (draws (-.smallest) smallest) ~f:(fun r -> Float.(r = 0.))
  && List.for_all
       [ (0., max_f); (-.max_f, 0.); (-3., 7.); (-1e300, 1e300) ]
       ~f:(fun (lo, hi) ->
         List.for_all (draws lo hi) ~f:(fun r -> Float.(r > lo && r < hi)))

let%test "fill_float32_range_bigarray with min = max = infinity raises" =
  List.for_all [ Float.infinity; Float.neg_infinity ] ~f:(fun bound ->
      let state = Xs_S.init_deterministic seed in
      let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 100 in
      match Xs_S.fill_float32_range_bigarray state ba ~min:bound ~max:bound with
      | exception Invalid_argument _ -> true
      | () -> false)

(* --- Edge testing floatrange functions --- *)

(* Test the limits of the actual float_range functions by creating a state that
   will output 0 and 2^64-1, so we can prove that the output never exceeds the
   given bounds and that this is a valid way to e.g. generate a nonzero minimum
   for log transforms. *)
let%expect_test
    "test that float_range with epsilon and Float.pred 1.0 will never be out \
     of those bounds" =
  let desired_state = [| 0L; 0L; 0L; 0xffffffffffffffffL |] in
  let seed_string = Seed_reverse.seed_string_for_state desired_state in
  let print_fill ~min ~max =
    let state = Xs_S.init_string seed_string in
    let state2 = Xs_S.init_string seed_string in
    let print_float_and_bits d =
      printf "%s\n" (Float_fmt.sprintf_g 17 d);
      let bits = Stdlib.Int64.bits_of_float d in
      printf "0x%016Lx\n" bits
    in
    printf "min: ";
    print_float_and_bits min;
    printf "max: ";
    print_float_and_bits max;
    print_endline "";
    let draw state =
      let result = Xs_S.uniform_float state in
      ignore @@ Xs_S.uniform_float state;
      ignore @@ Xs_S.uniform_float state;
      ignore @@ Xs_S.uniform_float state;
      result
    in
    let draw_range state ~min ~max =
      let result = Xs_S.float_range state ~min ~max in
      ignore @@ Xs_S.uniform_float state;
      ignore @@ Xs_S.uniform_float state;
      ignore @@ Xs_S.uniform_float state;
      result
    in
    let floats = List.init 5 ~f:(fun _ -> draw state) in
    List.iter floats ~f:print_float_and_bits;
    print_endline "range:";
    let rangefloats = List.init 5 ~f:(fun _ -> draw_range state2 ~min ~max) in
    (* The raw draws above hit both extremes a float_range input can take (the
       all-zero word and the all-ones word), and float_range is a monotonic
       function of that input, so these bounds checks on the mapped output
       cover every value float_range can ever produce, not just these two. *)
    List.iter rangefloats ~f:(fun r ->
        assert (Float.( >= ) r min && Float.( <= ) r max));
    List.iter rangefloats ~f:print_float_and_bits
  in
  let min = Stdlib.Float.succ 0.0 in
  let max = 1.0 in
  print_fill ~min ~max;
  [%expect
    {|
    min: 4.9406564584124654e-324
    0x0000000000000001
    max: 1
    0x3ff0000000000000

    0
    0x0000000000000000
    0
    0x0000000000000000
    0.99999999999999989
    0x3fefffffffffffff
    0.99999999999999989
    0x3fefffffffffffff
    0.99999999999999989
    0x3fefffffffffffff
    range:
    4.9406564584124654e-324
    0x0000000000000001
    4.9406564584124654e-324
    0x0000000000000001
    0.99999999999999989
    0x3fefffffffffffff
    0.99999999999999989
    0x3fefffffffffffff
    0.99999999999999989
    0x3fefffffffffffff
    |}];
  let min = 1.0 in
  let max = 1.5 in
  print_fill ~min ~max;
  [%expect
    {|
    min: 1
    0x3ff0000000000000
    max: 1.5
    0x3ff8000000000000

    0
    0x0000000000000000
    0
    0x0000000000000000
    0.99999999999999989
    0x3fefffffffffffff
    0.99999999999999989
    0x3fefffffffffffff
    0.99999999999999989
    0x3fefffffffffffff
    range:
    1
    0x3ff0000000000000
    1
    0x3ff0000000000000
    1.5
    0x3ff8000000000000
    1.5
    0x3ff8000000000000
    1.5
    0x3ff8000000000000
    |}];
  let min = Float.min_positive_normal_value in
  let max = Stdlib.Float.pred 1.0 in
  print_fill ~min ~max;
  [%expect
    {|
    min: 2.2250738585072014e-308
    0x0010000000000000
    max: 0.99999999999999989
    0x3fefffffffffffff

    0
    0x0000000000000000
    0
    0x0000000000000000
    0.99999999999999989
    0x3fefffffffffffff
    0.99999999999999989
    0x3fefffffffffffff
    0.99999999999999989
    0x3fefffffffffffff
    range:
    2.2250738585072014e-308
    0x0010000000000000
    2.2250738585072014e-308
    0x0010000000000000
    0.99999999999999978
    0x3feffffffffffffe
    0.99999999999999978
    0x3feffffffffffffe
    0.99999999999999978
    0x3feffffffffffffe
    |}]

(* Bulk-fill analog of the test above. Fills write in word-major,
   lane-minor order (index = word * 4 + lane -- see the raw-state-dump note
   in xoshiro_simd.mli), so lane 0 -- pinned by [desired_state] to the fixed
   0/0/0/max-bits pattern used above -- lands at indices 0, 4, 8, 12, 16.
   The other three indices in each group of four come from lanes 1-3, which
   are ordinary jumped-from-lane-0 states and look like normal output; they
   are printed too (not skipped) so both facts are visible side by side. *)
let%expect_test "bulk fill_float64_range_bigarray: same bounds as above" =
  let desired_state = [| 0L; 0L; 0L; 0xffffffffffffffffL |] in
  let seed_string = Seed_reverse.seed_string_for_state desired_state in
  let print_float_and_bits d =
    let f = Float_fmt.sprintf_g 17 d in
    printf "%23s " f;
    let bits = Stdlib.Int64.bits_of_float d in
    printf "0x%016Lx\n" bits
  in
  let print_fill ~min ~max =
    let state = Xs_S.init_string seed_string in
    let n = 20 in
    let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
    Xs_S.fill_float64_range_bigarray state ba ~min ~max;
    printf "min: ";
    print_float_and_bits min;
    printf "max: ";
    print_float_and_bits max;
    print_endline "";
    for i = 0 to n - 1 do
      let r = Bigarray.Array1.get ba i in
      assert (Float.( >= ) r min && Float.( <= ) r max)
    done;
    for i = 0 to n - 1 do
      print_float_and_bits (Bigarray.Array1.get ba i)
    done
  in
  print_fill ~min:(Stdlib.Float.succ 0.0) ~max:1.0;
  [%expect
    {|
    min: 4.9406564584124654e-324 0x0000000000000001
    max:                       1 0x3ff0000000000000

        0.99999999999999989 0x3fefffffffffffff
       0.072201691127457779 0x3fb27bcf5e1bbb28
        0.33434332313470261 0x3fd565e1899ff7f0
        0.13989459488964862 0x3fc1e810eaf81740
        0.99999999999999989 0x3fefffffffffffff
         0.9232853463483216 0x3fed8b8db5ee2579
        0.74305909924569458 0x3fe7c723e0482cdd
         0.8319427262354705 0x3fea9f465a2a7217
        0.99999999999999989 0x3fefffffffffffff
        0.72992079850142821 0x3fe75b82dcc77b9e
        0.43902545636877721 0x3fdc18fe3a4dc8ac
        0.32739850215106281 0x3fd4f418d8dfe284
    4.9406564584124654e-324 0x0000000000000001
        0.63902826727188633 0x3fe472eb68a4dfa2
        0.30533468224232407 0x3fd38a9a7aa42e6e
       0.094986180740524784 0x3fb85103abda3d60
    4.9406564584124654e-324 0x0000000000000001
        0.14794957464455172 0x3fc2f002fc471954
        0.23335528737359401 0x3fcdde9607cf2368
        0.82052481352508821 0x3fea41bd40f4b232
    |}];
  print_fill ~min:1.0 ~max:1.5;
  [%expect
    {|
    min:                       1 0x3ff0000000000000
    max:                     1.5 0x3ff8000000000000

                        1.5 0x3ff8000000000000
         1.0361008455637288 0x3ff093de7af0ddd9
         1.1671716615673513 0x3ff2acbc3133fefe
         1.0699472974448243 0x3ff11e810eaf8174
                        1.5 0x3ff8000000000000
         1.4616426731741607 0x3ff762e36d7b895e
         1.3715295496228472 0x3ff5f1c8f8120b37
         1.4159713631177353 0x3ff6a7d1968a9c86
                        1.5 0x3ff8000000000000
         1.3649603992507142 0x3ff5d6e0b731dee8
         1.2195127281843887 0x3ff3831fc749b916
         1.1636992510755313 0x3ff29e831b1bfc50
                          1 0x3ff0000000000000
         1.3195141336359431 0x3ff51cbada2937e8
         1.1526673411211621 0x3ff271534f5485ce
         1.0474930903702624 0x3ff0c2881d5ed1eb
                          1 0x3ff0000000000000
         1.0739747873222758 0x3ff12f002fc47195
         1.1166776436867969 0x3ff1dde9607cf236
          1.410262406762544 0x3ff6906f503d2c8c
    |}];
  print_fill ~min:Float.min_positive_normal_value ~max:(Stdlib.Float.pred 1.0);
  [%expect
    {|
    min: 2.2250738585072014e-308 0x0010000000000000
    max:     0.99999999999999989 0x3fefffffffffffff

        0.99999999999999978 0x3feffffffffffffe
       0.072201691127457765 0x3fb27bcf5e1bbb27
        0.33434332313470255 0x3fd565e1899ff7ef
        0.13989459488964859 0x3fc1e810eaf8173f
        0.99999999999999978 0x3feffffffffffffe
        0.92328534634832149 0x3fed8b8db5ee2578
        0.74305909924569447 0x3fe7c723e0482cdc
        0.83194272623547039 0x3fea9f465a2a7216
        0.99999999999999978 0x3feffffffffffffe
         0.7299207985014281 0x3fe75b82dcc77b9d
        0.43902545636877716 0x3fdc18fe3a4dc8ab
        0.32739850215106275 0x3fd4f418d8dfe283
    2.2250738585072014e-308 0x0010000000000000
        0.63902826727188622 0x3fe472eb68a4dfa1
        0.30533468224232402 0x3fd38a9a7aa42e6d
        0.09498618074052477 0x3fb85103abda3d5f
    2.2250738585072014e-308 0x0010000000000000
        0.14794957464455169 0x3fc2f002fc471953
        0.23335528737359398 0x3fcdde9607cf2367
         0.8205248135250881 0x3fea41bd40f4b231
    |}];
  print_fill ~min:(-1e10) ~max:1e10;
  [%expect
    {|
    min:            -10000000000 0xc202a05f20000000
    max:             10000000000 0x4202a05f20000000

         9999999999.9999962 0x4202a05f1ffffffe
        -8555966177.4508448 0xc1ffdf9aee1736a9
        -3313133537.3059483 0xc1e8af4cfc29ca54
        -7202108102.2070274 0xc1fad4772c634ffc
         9999999999.9999962 0x4202a05f1ffffffe
         8465706926.9664307 0x41ff8986faef7680
         4861181984.9138908 0x41f21bfc020e9f4c
         6638854524.7094097 0x41f8bb4e17cb59be
         9999999999.9999962 0x4202a05f1ffffffe
         4598415970.0285645 0x41f1216426207500
        -1219490872.6244564 0xc1d22bfd0e27f718
        -3452029956.9787436 0xc1e9b839809f51de
               -10000000000 0xc202a05f20000000
          2780565345.437727 0x41e4b781ec2e01dc
        -3893306355.1535187 0xc1ed01e47e64e9a0
        -8100276385.1895046 0xc1fe2d068a130836
               -10000000000 0xc202a05f20000000
        -7041008507.1089659 0xc1fa3ad437b1be53
          -5332894252.52812 0xc1f3ddd822c8732e
         6410496270.5017643 0x41f7e18690e8073a
    |}];

  (* min:0, max: 2 * the smallest normal double. The fill's granularity here
     is max / 2^53 -- exactly the smallest subnormal double -- so the bottom
     half of the 2^53 possible draws (by index) land subnormal and the top
     half land normal, all within one pinned dump. *)
  print_fill ~min:0.0 ~max:(Float.ldexp 1. (-1021));
  [%expect
    {|
    min:                       0 0x0000000000000000
    max: 4.4501477170144028e-308 0x0020000000000000

    4.4501477170144023e-308 0x001fffffffffffff
    3.2130819093543529e-309 0x00024f79ebc37765
    1.4878771761469056e-308 0x000ab2f0c4cffbf8
    6.2255161207082452e-309 0x00047a043abe05d0
    4.4501477170144023e-308 0x001fffffffffffff
    4.1087561762048355e-308 0x001d8b8db5ee2579
    3.3067227541150062e-308 0x0017c723e0482cdd
    3.7022680238435173e-308 0x001a9f465a2a7217
    4.4501477170144023e-308 0x001fffffffffffff
    3.2482553750524607e-308 0x00175b82dcc77b9e
    1.9537281323707202e-308 0x000e0c7f1d26e456
    1.4569716969014872e-308 0x000a7a0c6c6ff142
                          0 0x0000000000000000
    2.8437701847076545e-308 0x001472eb68a4dfa2
    1.3587844391059966e-308 0x0009c54d3d521737
     4.227025353703638e-309 0x00030a20757b47ac
                          0 0x0000000000000000
     6.583974618377038e-309 0x0004bc00bf11c655
    1.0384654993588393e-308 0x000777a581f3c8da
    3.6514566256623399e-308 0x001a41bd40f4b232
    |}]

let%expect_test "bulk fill_float32_range_bigarray: same bounds as above" =
  let desired_state = [| 0L; 0L; 0L; 0xffffffffffffffffL |] in
  let seed_string = Seed_reverse.seed_string_for_state desired_state in
  let print_float_and_bits d =
    let f = Float_fmt.sprintf_g 9 d in
    printf "%15s " f;
    let bits = Stdlib.Int32.bits_of_float d in
    printf "0x%08lx\n" bits
  in
  let print_fill ~min ~max =
    let state = Xs_S.init_string seed_string in
    let n = 40 in
    let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n in
    Xs_S.fill_float32_range_bigarray state ba ~min ~max;
    printf "min: ";
    print_float_and_bits min;
    printf "max: ";
    print_float_and_bits max;
    print_endline "";
    for i = 0 to n - 1 do
      let r = Bigarray.Array1.get ba i in
      assert (Float.( >= ) r min && Float.( <= ) r max)
    done;
    for i = 0 to n - 1 do
      print_float_and_bits (Bigarray.Array1.get ba i)
    done
  in
  print_fill ~min:0x1.p-126 ~max:1.0;
  [%expect
    {|
    min:  1.17549435e-38 0x00800000
    max:               1 0x3f800000

         0.99999994 0x3f7fffff
         0.99999994 0x3f7fffff
        0.108324707 0x3dddd958
       0.0722016692 0x3d93de78
        0.499507904 0x3effbf80
        0.334343314 0x3eab2f0c
        0.938209951 0x3f702e87
        0.139894545 0x3e0f4084
         0.99999994 0x3f7fffff
         0.99999994 0x3f7fffff
        0.442074597 0x3ee2579a
        0.923285306 0x3f6c5c6d
        0.255476475 0x3e82cdd0
        0.743059099 0x3f3e391f
          0.3264274 0x3ea7217e
        0.831942677 0x3f54fa32
         0.99999994 0x3f7fffff
         0.99999994 0x3f7fffff
        0.233840406 0x3e6f73dc
        0.729920745 0x3f3adc16
        0.215373278 0x3e5c8ad0
        0.439025402 0x3ee0c7f0
        0.498200774 0x3eff142c
        0.327398479 0x3ea7a0c6
     1.17549435e-38 0x00800000
     1.17549435e-38 0x00800000
        0.152299047 0x3e1bf448
        0.639028251 0x3f23975b
        0.565334022 0x3f10b9bb
        0.305334628 0x3e9c54d2
        0.852499127 0x3f5a3d62
       0.0949861407 0x3dc28818
     1.17549435e-38 0x00800000
     1.17549435e-38 0x00800000
        0.555460811 0x3f0e32ae
        0.147949517 0x3e178014
        0.618268371 0x3f1e46d6
        0.233355284 0x3e6ef4b0
        0.646752775 0x3f259197
        0.820524812 0x3f520dea
    |}];
  print_fill ~min:1.0 ~max:1.5;
  [%expect
    {|
    min:               1 0x3f800000
    max:             1.5 0x3fc00000

                1.5 0x3fc00000
                1.5 0x3fc00000
         1.05416238 0x3f86eecb
         1.03610086 0x3f849ef4
         1.24975395 0x3f9ff7f0
         1.16717172 0x3f9565e2
         1.46910501 0x3fbc0ba2
         1.06994724 0x3f88f408
                1.5 0x3fc00000
                1.5 0x3fc00000
         1.22103727 0x3f9c4af3
         1.46164262 0x3fbb171b
         1.12773824 0x3f9059ba
         1.37152958 0x3faf8e48
         1.16321373 0x3f94e430
         1.41597128 0x3fb53e8c
                1.5 0x3fc00000
                1.5 0x3fc00000
         1.11692023 0x3f8ef73e
         1.36496043 0x3faeb706
         1.10768664 0x3f8dc8ad
          1.2195127 0x3f9c18fe
         1.24910045 0x3f9fe286
         1.16369927 0x3f94f419
                  1 0x3f800000
                  1 0x3f800000
         1.07614946 0x3f89bf44
         1.31951416 0x3fa8e5d7
         1.28266704 0x3fa42e6f
         1.15266728 0x3f938a9a
          1.4262495 0x3fb68f58
          1.0474931 0x3f861441
                  1 0x3f800000
                  1 0x3f800000
         1.27773046 0x3fa38cac
         1.07397473 0x3f897801
         1.30913424 0x3fa791b6
         1.11667764 0x3f8eef4b
         1.32337642 0x3fa96466
         1.41026235 0x3fb4837a
    |}];
  print_fill ~min:0x1.p-24 ~max:(Stdlib.Float.pred 1.0);
  [%expect
    {|
    min:  5.96046448e-08 0x33800000
    max:               1 0x3f800000

         0.99999994 0x3f7fffff
         0.99999994 0x3f7fffff
        0.108324759 0x3dddd95f
       0.0722017214 0x3d93de7f
        0.499507934 0x3effbf81
        0.334343344 0x3eab2f0d
        0.938209951 0x3f702e87
         0.13989459 0x3e0f4087
         0.99999994 0x3f7fffff
         0.99999994 0x3f7fffff
        0.442074627 0x3ee2579b
        0.923285306 0x3f6c5c6d
        0.255476505 0x3e82cdd1
        0.743059099 0x3f3e391f
         0.32642743 0x3ea7217f
        0.831942677 0x3f54fa32
         0.99999994 0x3f7fffff
         0.99999994 0x3f7fffff
        0.233840451 0x3e6f73df
        0.729920745 0x3f3adc16
        0.215373322 0x3e5c8ad3
        0.439025432 0x3ee0c7f1
        0.498200804 0x3eff142d
        0.327398509 0x3ea7a0c7
     5.96046448e-08 0x33800000
     5.96046448e-08 0x33800000
        0.152299091 0x3e1bf44b
        0.639028251 0x3f23975b
        0.565334022 0x3f10b9bb
        0.305334657 0x3e9c54d3
        0.852499127 0x3f5a3d62
       0.0949861929 0x3dc2881f
     5.96046448e-08 0x33800000
     5.96046448e-08 0x33800000
        0.555460811 0x3f0e32ae
        0.147949561 0x3e178017
        0.618268371 0x3f1e46d6
        0.233355328 0x3e6ef4b3
        0.646752775 0x3f259197
        0.820524812 0x3f520dea
    |}];
  print_fill ~min:(-1e10) ~max:1e10;
  [%expect
    {|
    min:          -1e+10 0xd01502f9
    max:           1e+10 0x501502f9

     9.99999795e+09 0x501502f7
     9.99999795e+09 0x501502f7
    -7.83350579e+09 0xcfe974e8
    -8.55596646e+09 0xcffefcd8
           -9841664 0xcb162c00
    -3.31313357e+09 0xcf457a68
     8.76419994e+09 0x500298c5
    -7.20210944e+09 0xcfd6a3bc
     9.99999795e+09 0x501502f7
     9.99999795e+09 0x501502f7
    -1.15850854e+09 0xce8a1ae0
     8.46570598e+09 0x4ffc4c36
     -4.8904704e+09 0xcf91bf54
     4.86118195e+09 0x4f90dfe0
    -3.47145216e+09 0xcf4eea28
     6.63885312e+09 0x4fc5da6e
     9.99999795e+09 0x501502f7
     9.99999795e+09 0x501502f7
    -5.32319181e+09 0xcf9ea4bb
     4.59841536e+09 0x4f890b20
    -5.69253427e+09 0xcfa9a697
    -1.21949184e+09 0xce915ff0
          -35984384 0xcc094500
    -3.45203046e+09 0xcf4dc1ce
             -1e+10 0xd01502f9
             -1e+10 0xd01502f9
    -6.95401882e+09 0xcfcf3ef4
      2.7805655e+09 0x4f25bc10
     1.30668032e+09 0x4e9bc4b8
    -3.89330739e+09 0xcf680f28
     7.04998298e+09 0x4fd21b1a
    -8.10027725e+09 0xcff16836
             -1e+10 0xd01502f9
             -1e+10 0xd01502f9
     1.10921626e+09 0x4e843a98
    -7.04100966e+09 0xcfd1d6a4
      2.3653673e+09 0x4f0cfca4
    -5.33289421e+09 0xcf9eeec1
     2.93505536e+09 0x4f2ef164
       6.410496e+09 0x4fbf0c34
    |}];

  (* min:0, max: 2 * the smallest normal float32. The fill's granularity here
     is max / 2^24 -- exactly the smallest subnormal float32 -- so the bottom
     half of the 2^24 possible draws (by index) land subnormal and the top
     half land normal, all within one pinned dump. *)
  print_fill ~min:0.0 ~max:(Float.ldexp 1. (-125));
  [%expect
    {|
    min:               0 0x00000000
    max:   2.3509887e-38 0x01000000

     2.35098856e-38 0x00ffffff
     2.35098856e-38 0x00ffffff
     2.54670161e-39 0x001bbb2b
     1.69745309e-39 0x00127bcf
     1.17433744e-38 0x007fdfc0
     7.86037354e-39 0x00559786
     2.20572099e-38 0x00f02e87
     3.28890495e-39 0x0023d021
     2.35098856e-38 0x00ffffff
     2.35098856e-38 0x00ffffff
     1.03931238e-38 0x00712bcd
     2.17063332e-38 0x00ec5c6d
     6.00622306e-39 0x004166e8
     1.74692355e-38 0x00be391f
      7.6742713e-39 0x005390bf
     1.95588784e-38 0x00d4fa32
     2.35098856e-38 0x00ffffff
     2.35098856e-38 0x00ffffff
     5.49756152e-39 0x003bdcf7
     1.71603542e-38 0x00badc16
     5.06340142e-39 0x003722b4
     1.03214376e-38 0x007063f8
     1.17126439e-38 0x007f8a16
     7.69710125e-39 0x0053d063
                  0 0x00000000
                  0 0x00000000
     3.58053338e-39 0x0026fd12
      1.5023482e-38 0x00a3975b
      1.3290939e-38 0x0090b9bb
      7.1783826e-39 0x004e2a69
     2.00421582e-38 0x00da3d62
     2.23311344e-39 0x00185103
                  0 0x00000000
                  0 0x00000000
     1.30588209e-38 0x008e32ae
     3.47827642e-39 0x0025e005
     1.45354195e-38 0x009e46d6
     5.48615636e-39 0x003bbd2c
     1.52050847e-38 0x00a59197
     1.92904456e-38 0x00d20dea
    |}]

let%expect_test "bulk fill_float64_bigarray" =
  let desired_state = [| 0L; 0L; 0L; 0xffffffffffffffffL |] in
  let seed_string = Seed_reverse.seed_string_for_state desired_state in
  let print_float_and_bits d =
    let f = Float_fmt.sprintf_g 17 d in
    printf "%23s " f;
    let bits = Stdlib.Int64.bits_of_float d in
    printf "0x%016Lx\n" bits
  in
  let state = Xs_S.init_string seed_string in
  let n = 20 in
  let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n in
  Xs_S.fill_float64_bigarray state ba;
  for i = 0 to n - 1 do
    print_float_and_bits (Bigarray.Array1.get ba i)
  done;
  [%expect
    {|
     0.99999999999999989 0x3fefffffffffffff
    0.072201691127457779 0x3fb27bcf5e1bbb28
     0.33434332313470261 0x3fd565e1899ff7f0
     0.13989459488964862 0x3fc1e810eaf81740
     0.99999999999999989 0x3fefffffffffffff
      0.9232853463483216 0x3fed8b8db5ee2579
     0.74305909924569458 0x3fe7c723e0482cdd
      0.8319427262354705 0x3fea9f465a2a7217
     0.99999999999999989 0x3fefffffffffffff
     0.72992079850142821 0x3fe75b82dcc77b9e
     0.43902545636877721 0x3fdc18fe3a4dc8ac
     0.32739850215106281 0x3fd4f418d8dfe284
                       0 0x0000000000000000
     0.63902826727188633 0x3fe472eb68a4dfa2
     0.30533468224232407 0x3fd38a9a7aa42e6e
    0.094986180740524784 0x3fb85103abda3d60
                       0 0x0000000000000000
     0.14794957464455172 0x3fc2f002fc471954
     0.23335528737359401 0x3fcdde9607cf2368
     0.82052481352508821 0x3fea41bd40f4b232
    |}]

let%expect_test "bulk fill_float32_bigarray" =
  let desired_state = [| 0L; 0L; 0L; 0xffffffffffffffffL |] in
  let seed_string = Seed_reverse.seed_string_for_state desired_state in
  let print_float_and_bits d =
    let f = Float_fmt.sprintf_g 9 d in
    printf "%15s " f;
    let bits = Stdlib.Int32.bits_of_float d in
    printf "0x%08lx\n" bits
  in
  let state = Xs_S.init_string seed_string in
  let n = 40 in
  let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout n in
  Xs_S.fill_float32_bigarray state ba;
  for i = 0 to n - 1 do
    print_float_and_bits (Bigarray.Array1.get ba i)
  done;
  [%expect
    {|
      0.99999994 0x3f7fffff
      0.99999994 0x3f7fffff
     0.108324707 0x3dddd958
    0.0722016692 0x3d93de78
     0.499507904 0x3effbf80
     0.334343314 0x3eab2f0c
     0.938209951 0x3f702e87
     0.139894545 0x3e0f4084
      0.99999994 0x3f7fffff
      0.99999994 0x3f7fffff
     0.442074597 0x3ee2579a
     0.923285306 0x3f6c5c6d
     0.255476475 0x3e82cdd0
     0.743059099 0x3f3e391f
       0.3264274 0x3ea7217e
     0.831942677 0x3f54fa32
      0.99999994 0x3f7fffff
      0.99999994 0x3f7fffff
     0.233840406 0x3e6f73dc
     0.729920745 0x3f3adc16
     0.215373278 0x3e5c8ad0
     0.439025402 0x3ee0c7f0
     0.498200774 0x3eff142c
     0.327398479 0x3ea7a0c6
               0 0x00000000
               0 0x00000000
     0.152299047 0x3e1bf448
     0.639028251 0x3f23975b
     0.565334022 0x3f10b9bb
     0.305334628 0x3e9c54d2
     0.852499127 0x3f5a3d62
    0.0949861407 0x3dc28818
               0 0x00000000
               0 0x00000000
     0.555460811 0x3f0e32ae
     0.147949517 0x3e178014
     0.618268371 0x3f1e46d6
     0.233355284 0x3e6ef4b0
     0.646752775 0x3f259197
     0.820524812 0x3f520dea
    |}]

(* --- Rounding at the ends of a range --- *)

(* A range fill computes (u * 2^-k) * scale + offset in the output type (k = 24
   for float32 and 53 for float64) and rounds once, so when fewer than 2^k floats
   lie between min and max, nearby draws round to the same float. That makes
   min more likely than 2^-k, and can make max itself the result. Each case
   below pins the draw that sits exactly on a rounding boundary and checks the
   float that comes out. The documented claims in xoshiro_simd.mli rest on
   these:
   - 1.0 to 2.0 can return 2.0 (the largest draw is a tie, which rounds to
     even), and min is returned for two draws, not one;
   - a narrow range at a large exponent gives min and max a probability of
     several percent each;
   - a range from 0 never returns max, as the largest draw is exactly
     representable.

   xoshiro256+ outputs s0 + s3, so with s0 = 0 the first word of lane 0 is s3.
   The float64 fills and float_range use its top 53 bits as the draw (index 0
   of a fill), and the float32 fill takes u24 from bits 8..31 for index 0 and
   from bits 40..63 for index 1. The tie cases are the interesting ones: a tie
   goes to the even neighbour, and min = 2^k has an even mantissa. *)
let first_word_state ~backend w =
  let desired_state =
    if Int64.(w = 0L) then [| 0L; 1L; 0L; 0L |] else [| 0L; 0L; 0L; w |]
  in
  Xs_S.init_string ~backend (Seed_reverse.seed_string_for_state desired_state)

let check_rounding_cases ?(backends = Xoshiro_simd.available_backends ()) ~what
    cases ~generate =
  List.iter backends ~f:(fun backend ->
      List.iter cases ~f:(fun (min, max, draws) ->
          List.iter draws ~f:(fun (draw, expected) ->
              let got = generate ~backend ~min ~max draw in
              if not (Float.equal got expected) then
                failwith
                  (sprintf
                     "%s: backend %s, min %h, max %h, draw %Ld: got %h, \
                      expected %h"
                     what
                     (Xoshiro_simd.backend_to_string backend)
                     min max draw got expected))))

let float64_range_cases =
  let two53 = Int64.shift_left 1L 53 in
  let pred_two = Stdlib.Float.pred 2.0 in
  [
    (* [1, 2) holds only 2^52 floats, half the 2^53 draws. Draws 0 and 1 both
       give min (1 is a tie: 2^-53 is half the spacing at 1.0), and the largest
       draw is a tie that rounds up to 2.0. *)
    ( 1.0,
      2.0,
      [
        (0L, 1.0);
        (1L, 1.0);
        (2L, Stdlib.Float.succ 1.0);
        (3L, Stdlib.Float.succ (Stdlib.Float.succ 1.0));
        (Int64.(two53 - 2L), pred_two);
        (Int64.(two53 - 1L), 2.0);
      ] );
    (* The spacing at 2^60 is 2^8, so 2^60 to 2^60 + 4 spacings has a draw
       space of 2^53 for 5 floats. Draw 2^50 is the tie between min and
       min + 1 spacing, 7 * 2^50 the tie between min + 3 spacings and max. *)
    ( 0x1p60,
      0x1p60 +. 1024.,
      [
        (Int64.shift_left 1L 50, 0x1p60);
        (Int64.(shift_left 1L 50 + 1L), 0x1p60 +. 256.);
        (Int64.(shift_left 7L 50 - 1L), 0x1p60 +. 768.);
        (Int64.shift_left 7L 50, 0x1p60 +. 1024.);
      ] );
    (* From 0 the largest draw is exactly representable, so max is never
       returned, and min is returned only by draw 0. *)
    ( 0.0,
      1.0,
      [ (0L, 0.0); (1L, 0x1p-53); (Int64.(two53 - 1L), Stdlib.Float.pred 1.0) ]
    );
  ]

let%test_unit "fill_float64_range_bigarray: min and max at rounding boundaries"
    =
  let backends =
    List.filter (Xoshiro_simd.available_backends ()) ~f:(fun backend ->
        not (x87_double_rounding && Xoshiro_simd.backend_equal backend Plain))
  in
  check_rounding_cases ~backends ~what:"fill_float64_range_bigarray"
    float64_range_cases ~generate:(fun ~backend ~min ~max draw ->
      let state = first_word_state ~backend (Int64.shift_left draw 11) in
      let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout 4 in
      Xs_S.fill_float64_range_bigarray state ba ~min ~max;
      Bigarray.Array1.get ba 0)

let%test_unit "float_range: min and max at rounding boundaries" =
  if not x87_double_rounding then
    check_rounding_cases ~what:"float_range" float64_range_cases
      ~generate:(fun ~backend ~min ~max draw ->
        let state = first_word_state ~backend (Int64.shift_left draw 11) in
        Xs_S.float_range state ~min ~max)

let%test_unit "fill_float32_range_bigarray: min and max at rounding boundaries"
    =
  let two24 = Int64.shift_left 1L 24 in
  let pred_two = 0x1.fffffep0 in
  let cases =
    [
      (* Same as the float64 case: 2^23 floats in [1, 2), so draws 0 and 1
         both give min and the largest draw rounds up to 2.0. *)
      ( 1.0,
        2.0,
        [
          (0L, 1.0);
          (1L, 1.0);
          (2L, 0x1.000002p0);
          (3L, 0x1.000004p0);
          (Int64.(two24 - 2L), pred_two);
          (Int64.(two24 - 1L), 2.0);
        ] );
      (* The spacing at 2^30 is 128: draw 2^21 is the tie between min and min
         + 1 spacing, 7 * 2^21 the tie between min + 3 spacings and max. *)
      ( 0x1p30,
        0x1p30 +. 512.,
        [
          (Int64.shift_left 1L 21, 0x1p30);
          (Int64.(shift_left 1L 21 + 1L), 0x1p30 +. 128.);
          (Int64.(shift_left 7L 21 - 1L), 0x1p30 +. 384.);
          (Int64.shift_left 7L 21, 0x1p30 +. 512.);
        ] );
      ( 0.0,
        1.0,
        [ (0L, 0.0); (1L, 0x1p-24); (Int64.(two24 - 1L), 0x1.fffffep-1) ] );
    ]
  in
  (* Index 0 takes its draw from bits 8..31 of the first word and index 1 from
     bits 40..63, so each case is run once through each position. *)
  check_rounding_cases ~what:"fill_float32_range_bigarray (index 0)" cases
    ~generate:(fun ~backend ~min ~max draw ->
      let state = first_word_state ~backend (Int64.shift_left draw 8) in
      let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 8 in
      Xs_S.fill_float32_range_bigarray state ba ~min ~max;
      Bigarray.Array1.get ba 0);
  check_rounding_cases ~what:"fill_float32_range_bigarray (index 1)" cases
    ~generate:(fun ~backend ~min ~max draw ->
      let state = first_word_state ~backend (Int64.shift_left draw 40) in
      let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 8 in
      Xs_S.fill_float32_range_bigarray state ba ~min ~max;
      Bigarray.Array1.get ba 1)
