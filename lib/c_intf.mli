type backend = Auto | Plain | Sse2 | Avx2 | Avx512fvl | Neon

val backend_to_string : backend -> string
val backend_equal : backend -> backend -> bool

type cstate
type ba_int32 = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
type ba_f64 = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
type created = { c_state : cstate; bitstream : ba_int32; floatstream : ba_f64 }

val init_deterministic : ?backend:backend -> int64 -> created
val init_intarray : ?backend:backend -> int64 array -> created
val init_string : ?backend:backend -> string -> created
val init : ?backend:backend -> unit -> created
val available_backends : unit -> backend list
val set_backend_limit : backend -> unit

val int_length : unit -> int
val float_length : unit -> int

val state_backend : cstate -> backend
val reseed : cstate -> unit
val refill_block : cstate -> unit
val refill_float_block : cstate -> unit
val get_bytes_internal : cstate -> bytes -> int -> unit
val get_ba_internal : cstate -> ('a, 'b, 'c) Bigarray.Array1.t -> unit

val fill_32bit_float_ba_internal :
  cstate -> (float, Bigarray.float32_elt, 'c) Bigarray.Array1.t -> unit

val fill_double_ba_internal :
  cstate -> (float, Bigarray.float64_elt, 'c) Bigarray.Array1.t -> unit

val fill_32bit_float_range_ba_internal :
  cstate ->
  (float, Bigarray.float32_elt, 'c) Bigarray.Array1.t ->
  float ->
  float ->
  unit

val fill_double_range_ba_internal :
  cstate ->
  (float, Bigarray.float64_elt, 'c) Bigarray.Array1.t ->
  float ->
  float ->
  unit

(* This is external rather than a val to let callers call the C function
   directly. A val makes callers go through a wrapper taking a boxed int32 and
   relies on inlining for unboxing. *)
external lemire_c : (int32[@unboxed]) -> (int[@untagged]) -> (int[@untagged])
  = "xs256simdintf_lemire_32bits_byte" "xs256simdintf_lemire_32bits"
[@@noalloc]

val test_get_state : cstate -> int64 array -> int32 array -> floatarray -> unit
