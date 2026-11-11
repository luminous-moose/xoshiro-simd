type backend = Auto | Plain | Sse2 | Avx2 | Avx512fvl | Neon

external _backend_ids : unit -> int array = "xs256simdintf_backend_ids"

(* These values are derived from xs256_common.h's enum directly. The order and
   number of elements must match xs256simdintf_backend_ids. *)
let backend_ids = _backend_ids ()

let backend_to_int = function
  | Auto -> backend_ids.(0)
  | Plain -> backend_ids.(1)
  | Sse2 -> backend_ids.(2)
  | Avx2 -> backend_ids.(3)
  | Avx512fvl -> backend_ids.(4)
  | Neon -> backend_ids.(5)

let backend_of_int n =
  if n = backend_ids.(0) then Auto
  else if n = backend_ids.(1) then Plain
  else if n = backend_ids.(2) then Sse2
  else if n = backend_ids.(3) then Avx2
  else if n = backend_ids.(4) then Avx512fvl
  else if n = backend_ids.(5) then Neon
  else failwith "Xoshiro_simd: unknown backend"

let backend_to_string = function
  | Auto -> "Auto"
  | Plain -> "Plain"
  | Sse2 -> "Sse2"
  | Avx2 -> "Avx2"
  | Avx512fvl -> "Avx512fvl"
  | Neon -> "Neon"

let backend_equal = ( = )

type cstate
type ba_int32 = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
type ba_f64 = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
type created = { c_state : cstate; bitstream : ba_int32; floatstream : ba_f64 }

external _init_deterministic : int64 -> int -> cstate * ba_int32 * ba_f64
  = "xs256simdintf_internal_create_from_seed"

let init_deterministic ?(backend = Auto) seed =
  let c_state, bitstream, floatstream =
    _init_deterministic seed (backend_to_int backend)
  in
  { c_state; bitstream; floatstream }

external _init_intarray : int64 array -> int -> cstate * ba_int32 * ba_f64
  = "xs256simdintf_internal_create_from_array"

let init_intarray ?(backend = Auto) seed_array =
  let c_state, bitstream, floatstream =
    _init_intarray seed_array (backend_to_int backend)
  in
  { c_state; bitstream; floatstream }

external _init_string : string -> int -> cstate * ba_int32 * ba_f64
  = "xs256simdintf_internal_create_from_string"

let init_string ?(backend = Auto) seed_string =
  let c_state, bitstream, floatstream =
    _init_string seed_string (backend_to_int backend)
  in
  { c_state; bitstream; floatstream }

external _init : int -> cstate * ba_int32 * ba_f64
  = "xs256simdintf_create_initialized"

let init ?(backend = Auto) () =
  let c_state, bitstream, floatstream = _init (backend_to_int backend) in
  { c_state; bitstream; floatstream }

external _available_backends_raw : unit -> int list
  = "xs256simdintf_available_backends"

let available_backends () = List.map backend_of_int (_available_backends_raw ())

external _set_backend_limit : int -> unit = "xs256simdintf_set_backend_limit"
[@@noalloc]

let set_backend_limit backend =
  let id = backend_to_int backend in
  _set_backend_limit id

(* QWORDS and FLOATWORDS are defined once, in C: these functions allow them to
   be module-level constants here. *)
external int_length : unit -> int = "xs256simdintf_int_length"
external float_length : unit -> int = "xs256simdintf_float_length"

external _state_backend_raw : cstate -> int = "xs256simdintf_state_backend"

let state_backend s = backend_of_int (_state_backend_raw s)

external reseed : cstate -> unit = "xs256simdintf_reseed"

external refill_block : cstate -> unit = "xs256simdintf_refill_block"
[@@noalloc]

external refill_float_block : cstate -> unit
  = "xs256simdintf_refill_float_block"
[@@noalloc]

external get_bytes_internal : cstate -> bytes -> int -> unit
  = "xs256simdintf_get_bytes"
[@@noalloc]

external get_ba_internal : cstate -> ('a, 'b, 'c) Bigarray.Array1.t -> unit
  = "xs256simdintf_get_bytes_ba"
[@@noalloc]

external fill_32bit_float_ba_internal :
  cstate -> (float, Bigarray.float32_elt, 'c) Bigarray.Array1.t -> unit
  = "xs256simdintf_fill_32bit_float_ba"
[@@noalloc]

external fill_double_ba_internal :
  cstate -> (float, Bigarray.float64_elt, 'c) Bigarray.Array1.t -> unit
  = "xs256simdintf_fill_double_ba"
[@@noalloc]

external fill_32bit_float_range_ba_internal :
  cstate ->
  (float, Bigarray.float32_elt, 'c) Bigarray.Array1.t ->
  float ->
  float ->
  unit = "xs256simdintf_fill_32bit_float_range_ba"
[@@noalloc]

external fill_double_range_ba_internal :
  cstate ->
  (float, Bigarray.float64_elt, 'c) Bigarray.Array1.t ->
  float ->
  float ->
  unit = "xs256simdintf_fill_double_range_ba"
[@@noalloc]

external lemire_c : (int32[@unboxed]) -> (int[@untagged]) -> (int[@untagged])
  = "xs256simdintf_lemire_32bits_byte" "xs256simdintf_lemire_32bits"
[@@noalloc]

(* Testing only. *)
external test_get_state :
  cstate -> int64 array -> int32 array -> floatarray -> unit
  = "xs256simdintf_test_get_state"
