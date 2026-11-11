open Base

(* Windows' C runtime (mingw/msvcrt) pads %e/%g exponents to a minimum of
   3 digits ("e-038"), where every other libc this project tests against
   (glibc, the BSDs, musl) uses a minimum of 2 ("e-38"). OCaml's float
   formatting calls straight through to the platform's snprintf, so the
   same Printf.sprintf call produces different text depending on host --
   not a xoshiro-simd bug. Normalize down to the 2-digit form so a single
   expect-test literal matches on every platform. *)
let normalize_exponent s =
  match String.index s 'e' with
  | None -> s
  | Some e -> (
      let digits_start = e + 2 in
      let len = String.length s in
      let is_digit c = match c with '0' .. '9' -> true | _ -> false in
      let digits_end = ref digits_start in
      while !digits_end < len && is_digit s.[!digits_end] do
        digits_end := !digits_end + 1
      done;
      let max_strip = Int.max 0 (!digits_end - digits_start - 2) in
      let rec leading_zeros i =
        if i >= max_strip || not (Char.equal s.[digits_start + i] '0') then i
        else leading_zeros (i + 1)
      in
      match leading_zeros 0 with
      | 0 -> s
      | strip ->
          String.sub s ~pos:0 ~len:digits_start
          ^ String.sub s ~pos:(digits_start + strip)
              ~len:(len - digits_start - strip))

let sprintf_g precision f =
  normalize_exponent (Printf.sprintf "%.*g" precision f)
