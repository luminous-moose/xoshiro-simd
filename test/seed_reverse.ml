open Base

let mix_entropy_inverse = Xoshiro_simd.for_testing_mix_entropy_inverse

(* Byte serialization matching store_seed_word_le64's layout, so an
   inverted seed (a plain int64 array) can be turned into bytes usable
   with init_string. *)

let le64_bytes_of_int64 v =
  let buf = Bytes.create 8 in
  for i = 0 to 7 do
    let shift = i * 8 in
    let byte =
      Int64.(to_int_trunc (bit_and (shift_right_logical v shift) 0xffL))
    in
    Bytes.set buf i (Char.of_int_exn byte)
  done;
  buf

let seed_bytes_of_words words =
  let buf = Bytes.create 32 in
  Array.iteri words ~f:(fun idx w ->
      Bytes.blit ~src:(le64_bytes_of_int64 w) ~src_pos:0 ~dst:buf
        ~dst_pos:(idx * 8) ~len:8);
  buf

let seed_string_for_state (state_words : int64 array) : string =
  let seed_words = mix_entropy_inverse state_words in
  Bytes.to_string (seed_bytes_of_words seed_words)
