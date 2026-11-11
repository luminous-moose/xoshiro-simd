(* This code creates bit_reverse_table.h, a lookup table for each byte for
   xoshiro_bitreverse64 in bit_reverse.c to use. *)

let reverse_byte (b : int) : int =
  let bit7 = (b lsl 7) land 0xff in
  let bit6 = ((b lsr 1) lsl 6) land 0x7f in
  let bit5 = ((b lsr 2) lsl 5) land 0x3f in
  let bit4 = ((b lsr 3) lsl 4) land 0x1f in
  let bit3 = ((b lsr 4) lsl 3) land 0x0f in
  let bit2 = ((b lsr 5) lsl 2) land 0x07 in
  let bit1 = ((b lsr 6) lsl 1) land 0x03 in
  let bit0 = b lsr 7 in
  bit7 lor bit6 lor bit5 lor bit4 lor bit3 lor bit2 lor bit1 lor bit0

let () =
  print_string "#include <stdint.h>\n\n";
  print_string "static const uint8_t xoshiro_byte_bitreverse_table[256] = {\n";
  for b = 0 to 255 do
    Printf.printf "0x%02x,%s" (reverse_byte b)
      (if b mod 16 = 15 then "\n" else " ")
  done;
  print_string "};\n"
