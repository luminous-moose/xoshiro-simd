module C = C_intf

module I32 = struct
  include Int32

  external ( + ) : int32 -> int32 -> int32 = "%int32_add"
  external ( - ) : int32 -> int32 -> int32 = "%int32_sub"
  external ( mod ) : int32 -> int32 -> int32 = "%int32_mod"
  external ( > ) : int32 -> int32 -> bool = "%greaterthan"
end

module I64 = struct
  include Int64

  external ( + ) : int64 -> int64 -> int64 = "%int64_add"
  external ( - ) : int64 -> int64 -> int64 = "%int64_sub"
  external ( * ) : int64 -> int64 -> int64 = "%int64_mul"
  external ( mod ) : int64 -> int64 -> int64 = "%int64_mod"
  external ( > ) : int64 -> int64 -> bool = "%greaterthan"
  external ( land ) : int64 -> int64 -> int64 = "%int64_and"
  external ( lsr ) : int64 -> int -> int64 = "%int64_lsr"
end

module Int = struct
  include Int

  external ( >= ) : int -> int -> bool = "%greaterequal"
  external ( land ) : int -> int -> int = "%andint"
  external ( mod ) : int -> int -> int = "%modint"
end

open Bigarray

type ba_int32 = C.ba_int32
type ba_f64 = C.ba_f64
type backend = C_intf.backend = Auto | Plain | Sse2 | Avx2 | Avx512fvl | Neon

let backend_equal = C.backend_equal
let backend_to_string = C.backend_to_string
let available_backends = C.available_backends
let set_backend_limit = C.set_backend_limit

module Raw_dump = struct
  type t = {
    s : int64 array;
    bitstream : int32 array;
    floatstream : floatarray;
    int_idx : int;
    float_idx : int;
  }
end

(* bitstream and floatstream are both CAML_BA_EXTERNAL bigarrays that are
   aliases into the c_state struct. The C finalizer frees that struct, so the
   bigarrays must not outlive c_state. Keeping them in the same record ensures
   that they don't, as long as no function ever keeps a reference to one of the
   bigarrays. Nothing here keeps a reference to either bigarray.
   for_testing_raw_dump makes copies into arrays. *)
type state = {
  c_state : C.cstate;
  bitstream : ba_int32;
  floatstream : ba_f64;
  mutable int_idx : int;
  mutable float_idx : int;
}

let int_length = C.int_length ()
let float_length = C.float_length ()
let state_backend s = C.state_backend s.c_state

let reseed s =
  C.reseed s.c_state;
  (* When we reseed, we don't build the block right away, we just mark both as
     exhausted instead. The user might be using only ints or only floats, so no
     need to waste work generating something that isn't needed. *)
  s.int_idx <- int_length;
  s.float_idx <- float_length

let init_inner cs =
  let { C.c_state; bitstream; floatstream } = cs in
  {
    c_state;
    bitstream;
    floatstream;
    int_idx = int_length;
    float_idx = float_length;
  }

let init_deterministic ?(backend = C.Auto) seed =
  let cs = C.init_deterministic ~backend seed in
  init_inner cs

let init_int64array ?(backend = C.Auto) seed_array =
  let cs = C.init_intarray ~backend seed_array in
  init_inner cs

let init_string ?(backend = C.Auto) seed_string =
  let cs = C.init_string ~backend seed_string in
  init_inner cs

let init ?(backend = C.Auto) () =
  let cs = C.init ~backend () in
  init_inner cs

(* Test helpers. *)

(* 4 xoshiro256 state words (s0..s3) x 4 lanes. *)
let state_word_count = 16

let for_testing_raw_dump state =
  let s = Array.make state_word_count 0L in
  let bitstream = Array.make int_length 0l in
  let floatstream = Float.Array.make float_length 0. in
  C.test_get_state state.c_state s bitstream floatstream;
  {
    Raw_dump.s;
    bitstream;
    floatstream;
    int_idx = state.int_idx;
    float_idx = state.float_idx;
  }

let lane0_words (dump : Raw_dump.t) =
  [| dump.s.(0); dump.s.(4); dump.s.(8); dump.s.(12) |]

(* Seed recovery. *)

(* mix_entropy is a bijection on 32 byte inputs, so inverting lane 0 gives us a
   seed string that rebuilds this state. If there is no unread output in the
   caches, then that seed string completely determines all future outputs, so
   we check for that condition and fail if it doesn't hold. *)
let recover_seed_bytes state =
  if state.int_idx <> int_length || state.float_idx <> float_length then
    invalid_arg
      "Xoshiro_simd.State.recover_seed_bytes: state has already been drawn from";
  let dump = for_testing_raw_dump state in
  let words = Mix_entropy_inverse.compute (lane0_words dump) in
  let seed = Bytes.create 32 in
  for i = 0 to 3 do
    Bytes.set_int64_le seed (i * 8) words.(i)
  done;
  Bytes.unsafe_to_string seed

let for_testing_mix_entropy_inverse = Mix_entropy_inverse.compute

(* Random draw implementations. *)

let max_int31 = 0x3fff_ffff

let[@inline] bits32 state =
  if state.int_idx = int_length then (
    C.refill_block state.c_state;
    state.int_idx <- 0);
  let i32 = Array1.unsafe_get state.bitstream state.int_idx in
  state.int_idx <- state.int_idx + 1;
  i32

let[@inline] bits30 state = Int32.to_int (bits32 state) land max_int31

(* Debiased modulo sampling. This is the algorithm that Java and Stdlib.Random
   use. Lemire was faster on all tested platforms. Dev-only, kept for
   benchmarking. *)
let rec int_30bit_loop state bound =
  let r = bits30 state in
  let v = r mod bound in
  if r - v > max_int31 - bound + 1 then int_30bit_loop state bound else v

let[@inline] int_java state bound =
  if bound > max_int31 || bound <= 0 then
    invalid_arg "Xoshiro_simd.int: bound must be between 1 and 2^30 - 1"
  else int_30bit_loop state bound

(* On 64 bit platforms, this produces 0xffff_ffff. On 32 bit platforms, it
   produces nonsense but compiles, unlike a literal 0xffff_ffff, and it isn't
   used. *)
let mask32 = (1 lsl 32) - 1

(* Lemire's algorithm is used for OCaml integer ranges, as it the fastest known
   algorithm for that purpose. This implementation is only correct on 64 bit
   platforms because some intermediate values need more than 31 bits. Keeping
   everything in int64s and int32s is correct but far too slow on 32 bit
   platforms, so those platforms get a C implementation. 64 bit platforms get
   this one instead because the C call overhead makes the C version slower. The
   output is identical on both.
*)
let[@inline] int_lemire_native state bound =
  if bound > max_int31 || bound <= 0 then
    invalid_arg "Xoshiro_simd.int: bound must be between 1 and 2^30 - 1"
  else begin
    let open I64 in
    let x = of_int32 (bits32 state) land 0xffff_ffffL in
    let m = x * of_int bound in
    let l = to_int (m land 0xffff_ffffL) in
    if Int.(l >= bound) then to_int (m lsr 32)
    else begin
      (* The subtraction shortcuts found in O'Neill's Lemire implementation are
         gone because limiting the bound to max_int31 makes those branches dead
         code. *)
      let two_32_minus_bound = Int.(-bound land mask32) in
      let t = Int.(two_32_minus_bound mod bound) in
      if l >= t then to_int (m lsr 32)
      else
        let rec loop () =
          let x = of_int32 (bits32 state) land 0xffff_ffffL in
          let m = x * of_int bound in
          if to_int (m land 0xffff_ffffL) >= t then to_int (m lsr 32)
          else loop ()
        in
        loop ()
    end
  end

(* For the C Lemire implementation, we handle the rejection path in OCaml, which
   greatly simplifies the implementation and is rare enough (under 1/4 for any
   bound) that there should be very little performance impact. *)
let rec int_lemire_c_loop state n =
  let v = C.lemire_c (bits32 state) n in
  if v = -1 then int_lemire_c_loop state n else v

let[@inline] int_lemire_c state bound =
  if bound > max_int31 || bound <= 0 then
    invalid_arg "Xoshiro_simd.int: bound must be between 1 and 2^30 - 1"
  else int_lemire_c_loop state bound

let for_testing_lemire_reduce32_wide bound x =
  match C.lemire_c x bound with -1 -> None | v -> Some v

let int = if Sys.word_size = 64 then int_lemire_native else int_lemire_c

let[@inline] signed_bits31 state = Int32.to_int (bits32 state)

(* This is similar to bits32, but a draw takes two 32 bit words, so we refill if
   fewer than two are left. That keeps the refill from landing between the two
   halves. *)
let[@inline] bits64 state =
  if state.int_idx >= int_length - 1 then (
    C.refill_block state.c_state;
    state.int_idx <- 0);
  let start = state.int_idx in
  let lo = Array1.unsafe_get state.bitstream start in
  let hi = Array1.unsafe_get state.bitstream (start + 1) in
  state.int_idx <- start + 2;
  Int64.logor
    (Int64.logand (Int64.of_int32 lo) 0xffffffffL)
    (Int64.shift_left (Int64.of_int32 hi) 32)

let max_full_int = -1 lsr 1

(* Same debiased modulo sampling as int_30bit_loop. *)
let rec full_int_loop state bound =
  let r = Int64.to_int (bits64 state) lsr 1 in
  let v = r mod bound in
  if r - v > max_full_int - bound + 1 then full_int_loop state bound else v

let[@inline] full_int state bound =
  if bound <= 0 then invalid_arg "Xoshiro_simd.full_int: negative or 0 bound"
  else if bound <= max_int31 then int state bound
  else full_int_loop state bound

let[@inline] uniform_float state =
  if state.float_idx = float_length then (
    C.refill_float_block state.c_state;
    state.float_idx <- 0);
  let f = Array1.unsafe_get state.floatstream state.float_idx in
  state.float_idx <- state.float_idx + 1;
  f

let rec uniform_float_strict state =
  let f = uniform_float state in
  if f = 0. then uniform_float_strict state else f

let rec intrange_32b_loop s n =
  let open I32 in
  let r = shift_right_logical (bits32 s) 1 in
  let v = r mod n in
  if r - v > max_int - n + 1l then intrange_32b_loop s n else v

let intrange_int32 state bound32 =
  if bound32 <= 0l then invalid_arg "Xoshiro_simd.int32: negative or 0 bound"
  else intrange_32b_loop state bound32

let rec intrange_64b_loop s n =
  let open I64 in
  let r = shift_right_logical (bits64 s) 1 in
  let v = r mod n in
  if r - v > max_int - n + 1L then intrange_64b_loop s n else v

let intrange_int64 state bound64 =
  if bound64 <= 0L then invalid_arg "Xoshiro_simd.int64: negative or 0 bound"
  else intrange_64b_loop state bound64

let[@inline] float state bound = uniform_float state *. bound

let[@inline] bool state = Int32.compare (bits32 state) 0l < 0

let float_strict state bound =
  let a = Float.abs bound in
  if not (a > 0. && a <= Float.max_float) then (
    if Float.is_nan bound then
      invalid_arg "Xoshiro_simd.float_strict: NaN bound";
    if Float.is_infinite bound then
      invalid_arg "Xoshiro_simd.float_strict: infinite bound";
    invalid_arg "Xoshiro_simd.float_strict: zero bound");
  let r = ref (uniform_float state *. bound) in
  while !r = 0. do
    r := uniform_float state *. bound
  done;
  !r

(* The following fill functions bypass the small internal buffers entirely and
   generate straight into the destination array. No int_idx/float_idx
   bookkeeping applies. *)

let[@inline] get_bytes state bytes len =
  C.get_bytes_internal state.c_state bytes len

let[@inline] get_bytes_bigarray state ba = C.get_ba_internal state.c_state ba

let[@inline] fill_float32_bigarray state ba =
  C.fill_32bit_float_ba_internal state.c_state ba

let[@inline] fill_float64_bigarray state ba =
  C.fill_double_ba_internal state.c_state ba

let fill_float64_range_bigarray state ba ~min ~max =
  if Float.is_nan min then
    invalid_arg "Xoshiro_simd.fill_float64_range_bigarray: min is NaN";
  if Float.is_nan max then
    invalid_arg "Xoshiro_simd.fill_float64_range_bigarray: max is NaN";
  (* Not Float.is_infinite (max -. min) because on i386, the subtraction
     can stay in an 80 bit x87 register and be finite there. Comparing with
     max_float catches that and infinity too. *)
  let span = Float.abs (max -. min) in
  if span > Float.max_float then
    invalid_arg "Xoshiro_simd.fill_float64_range_bigarray: range is infinite";
  if Float.is_nan span then
    invalid_arg "Xoshiro_simd.fill_float64_range_bigarray: range is NaN";
  if min > max then
    invalid_arg
      "Xoshiro_simd.fill_float64_range_bigarray: min must not be greater than \
       max";
  let scale = max -. min in
  C.fill_double_range_ba_internal state.c_state ba scale min

let[@inline] round_to_float32 x = Int32.float_of_bits (Int32.bits_of_float x)

let fill_float32_range_bigarray state ba ~min ~max =
  if Float.is_nan min then
    invalid_arg "Xoshiro_simd.fill_float32_range_bigarray: min is NaN";
  if Float.is_nan max then
    invalid_arg "Xoshiro_simd.fill_float32_range_bigarray: max is NaN";
  if min > max then
    invalid_arg
      "Xoshiro_simd.fill_float32_range_bigarray: min must not be greater than \
       max";
  let max32 = round_to_float32 max in
  let min32 = round_to_float32 min in
  (* Scale from the rounded bounds, because rounding min and max - min
     separately can overshoot max32. *)
  let scale32 = round_to_float32 (max32 -. min32) in
  if Float.is_infinite min32 then
    invalid_arg
      "Xoshiro_simd.fill_float32_range_bigarray: min overflows float32 range";
  if Float.is_infinite scale32 then
    invalid_arg
      "Xoshiro_simd.fill_float32_range_bigarray: range must be representable \
       float32";
  C.fill_32bit_float_range_ba_internal state.c_state ba scale32 min32
