(* Shared byte-layout for harness.ml's fuzz input, factored out so `decode`
   can turn a saved AFL crash/queue file back into readable arguments
   without re-implementing (and risking drifting from) the harness's own
   parsing. AFL crash files carry no metadata of their own -- they're just
   the raw bytes that were on stdin, so this is the only way to recover
   what a saved file actually exercised. *)

type t =
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Full_int of int
  | Int_in_range of { min : int; max : int }
  | Int32_in_range of { min : int32; max : int32 }
  | Int64_in_range of { min : int64; max : int64 }
  | Nativeint of nativeint
  | Nativeint_in_range of { min : nativeint; max : nativeint }
  | Get_bytes of int
  | Permutation of int
  | Float_range of { min : float; max : float }
  | Float_range_strict of { min : float; max : float }
  | Float of float
  | Float_strict of float
  | Uniform_float
  | Fill_float32_range of { min : float; max : float; n : int }
  | Fill_float64_range of { min : float; max : float; n : int }

let read_all_bytes ic =
  let buf = Buffer.create 64 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  Buffer.to_bytes buf

let byte b i = if i < Bytes.length b then Char.code (Bytes.get b i) else 0

let int64_at b off =
  let r = ref 0L in
  for i = 7 downto 0 do
    r := Int64.logor (Int64.shift_left !r 8) (Int64.of_int (byte b (off + i)))
  done;
  !r

(* Selectors 16/17 (the bulk range fills) take their element count from byte
   17, i.e. 0-255 elements: enough for several blocks and every remainder of
   the 8-element float32 and 4-element float64 blocks. *)

(* Selector 9/10 clamp to the same modulus the harness uses, so a decoded
   Get_bytes/Permutation always shows the size that was actually exercised.

   These limits (1_000_000 and 100_000) are not what makes executions slow:
   capping them at 16_384 and 4_096 changed the rate from about 256 to about
   310 executions per second per instance, within the spread between
   instances, and a harness whose run_one does nothing at all reaches only
   300-540 (2026-10-18). The cost is afl-persistent's SIGSTOP/SIGCONT round
   trip with afl-fuzz on every execution, so the way to more throughput is
   more instances (fuzz_parallel.sh), not smaller cases. *)
let parse data =
  if Bytes.length data < 1 then None
  else
    let a = int64_at data 1 in
    let b = int64_at data 9 in
    Some
      (match byte data 0 mod 18 with
      | 0 -> Int (Int64.to_int a)
      | 1 -> Int32 (Int64.to_int32 a)
      | 2 -> Int64 a
      | 3 -> Full_int (Int64.to_int a)
      | 4 -> Int_in_range { min = Int64.to_int a; max = Int64.to_int b }
      | 5 -> Int32_in_range { min = Int64.to_int32 a; max = Int64.to_int32 b }
      | 6 -> Int64_in_range { min = a; max = b }
      | 7 -> Nativeint (Int64.to_nativeint a)
      | 8 ->
          Nativeint_in_range
            { min = Int64.to_nativeint a; max = Int64.to_nativeint b }
      | 9 -> Get_bytes (Int64.to_int a mod 1_000_000)
      | 10 -> Permutation (Int64.to_int a mod 100_000)
      | 11 ->
          Float_range
            { min = Int64.float_of_bits a; max = Int64.float_of_bits b }
      | 12 ->
          Float_range_strict
            { min = Int64.float_of_bits a; max = Int64.float_of_bits b }
      | 13 -> Float (Int64.float_of_bits a)
      | 14 -> Float_strict (Int64.float_of_bits a)
      | 15 -> Uniform_float
      | 16 ->
          Fill_float32_range
            {
              min = Int64.float_of_bits a;
              max = Int64.float_of_bits b;
              n = byte data 17;
            }
      | _ ->
          Fill_float64_range
            {
              min = Int64.float_of_bits a;
              max = Int64.float_of_bits b;
              n = byte data 17;
            })

let to_string = function
  | Int bound -> Printf.sprintf "int bound=%d" bound
  | Int32 bound -> Printf.sprintf "int32 bound=%ld" bound
  | Int64 bound -> Printf.sprintf "int64 bound=%Ld" bound
  | Full_int bound -> Printf.sprintf "full_int bound=%d" bound
  | Int_in_range { min; max } ->
      Printf.sprintf "int_in_range min=%d max=%d" min max
  | Int32_in_range { min; max } ->
      Printf.sprintf "int32_in_range min=%ld max=%ld" min max
  | Int64_in_range { min; max } ->
      Printf.sprintf "int64_in_range min=%Ld max=%Ld" min max
  | Nativeint bound -> Printf.sprintf "nativeint bound=%nd" bound
  | Nativeint_in_range { min; max } ->
      Printf.sprintf "nativeint_in_range min=%nd max=%nd" min max
  | Get_bytes n -> Printf.sprintf "get_bytes n=%d" n
  | Permutation n -> Printf.sprintf "permutation n=%d" n
  | Float_range { min; max } ->
      Printf.sprintf "float_range min=%.17g (bits=%Lx) max=%.17g (bits=%Lx)" min
        (Int64.bits_of_float min) max (Int64.bits_of_float max)
  | Float_range_strict { min; max } ->
      Printf.sprintf
        "float_range_strict min=%.17g (bits=%Lx) max=%.17g (bits=%Lx)" min
        (Int64.bits_of_float min) max (Int64.bits_of_float max)
  | Float bound ->
      Printf.sprintf "float bound=%.17g (bits=%Lx)" bound
        (Int64.bits_of_float bound)
  | Float_strict bound ->
      Printf.sprintf "float_strict bound=%.17g (bits=%Lx)" bound
        (Int64.bits_of_float bound)
  | Uniform_float -> "uniform_float"
  | Fill_float32_range { min; max; n } ->
      Printf.sprintf
        "fill_float32_range_bigarray min=%.17g (bits=%Lx) max=%.17g (bits=%Lx) \
         n=%d"
        min (Int64.bits_of_float min) max (Int64.bits_of_float max) n
  | Fill_float64_range { min; max; n } ->
      Printf.sprintf
        "fill_float64_range_bigarray min=%.17g (bits=%Lx) max=%.17g (bits=%Lx) \
         n=%d"
        min (Int64.bits_of_float min) max (Int64.bits_of_float max) n
