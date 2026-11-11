module type S = sig
  type backend = C_intf.backend =
    | Auto
    | Plain
    | Sse2
    | Avx2
    | Avx512fvl
    | Neon
  type state

  val int_length : int
  val max_int31 : int
  val backend_equal : backend -> backend -> bool
  val backend_to_string : backend -> string
  val available_backends : unit -> backend list
  val state_backend : state -> backend
  val init : ?backend:C_intf.backend -> unit -> state
  val init_deterministic : ?backend:C_intf.backend -> int64 -> state
  val init_int64array : ?backend:C_intf.backend -> int64 array -> state
  val init_string : ?backend:C_intf.backend -> string -> state
  val reseed : state -> unit
  val bits30 : state -> int
  val signed_bits31 : state -> int
  val bits32 : state -> int32
  val bits64 : state -> int64
  val int : state -> int -> int

  val int_java : state -> int -> int

  val int_lemire_c : state -> int -> int

  val full_int : state -> int -> int
  val uniform_float : state -> float
  val uniform_float_strict : state -> float

  val intrange_int32 : state -> int32 -> int32
  val intrange_int64 : state -> int64 -> int64
  val float : state -> float -> float
  val float_strict : state -> float -> float
  val bool : state -> bool
  val get_bytes : state -> bytes -> int -> unit
  val get_bytes_bigarray : state -> ('a, 'b, 'c) Bigarray.Array1.t -> unit

  val fill_float32_bigarray :
    state -> (float, Bigarray.float32_elt, 'c) Bigarray.Array1.t -> unit

  val fill_float64_bigarray :
    state -> (float, Bigarray.float64_elt, 'c) Bigarray.Array1.t -> unit

  val fill_float32_range_bigarray :
    state ->
    (float, Bigarray.float32_elt, 'c) Bigarray.Array1.t ->
    min:float ->
    max:float ->
    unit

  val fill_float64_range_bigarray :
    state ->
    (float, Bigarray.float64_elt, 'c) Bigarray.Array1.t ->
    min:float ->
    max:float ->
    unit
end
