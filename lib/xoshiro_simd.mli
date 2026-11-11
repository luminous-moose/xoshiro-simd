(** Fast random number generator for OCaml that uses SIMD where available. Works
    on OCaml 4.13 and up. Uses xoshiro256++ for integers and xoshiro256+ for
    floats. Not cryptographically secure. For a given library version, output
    from a given seed is identical across backends and hosts, except that
    [nativeint] values and the [int]/[nativeint] Bigarray kinds differ between
    32 bit and 64 bit hosts, and on i386 only, float values might differ in the
    last bit. i386 is not a good platform for reproducible floats and this
    library isn't going to fix the output there. See the
    {{:https://github.com/luminous-moose/xoshiro-simd#readme} README} for
    details. *)

(** Type used to select which SIMD backend generates a state's xoshiro256
    output. [Auto] selects the best backend available on the current CPU, unless
    restricted via {!set_backend_limit}. [Plain] is a portable C fallback
    supported on all platforms. [Sse2] and [Avx2] require their respective CPU
    features, while [Avx512fvl] needs AVX512F, AVX512VL, and AVX512DQ, and stays
    on 256 bit registers. [Neon] is an ARM backend. More might be added in
    future versions. Every backend produces identical output (except for float
    outputs on i386).

    Passing a [~backend] other than [Auto] raises [Invalid_argument] if it isn't
    compiled in or the CPU doesn't support it. {!available_backends} lists the
    ones that will work. *)
type backend = Auto | Plain | Sse2 | Avx2 | Avx512fvl | Neon

val backend_equal : backend -> backend -> bool

(** [backend_to_string backend] is [backend]'s constructor name, e.g. ["Neon"].
*)
val backend_to_string : backend -> string

(** [available_backends ()] returns the backends supported by the current CPU,
    ordered from most to least capable. [Plain] is always included. Ignores
    {!set_backend_limit}. *)
val available_backends : unit -> backend list

(** [set_backend_limit backend] limits what [Auto] will select: it will never
    pick a backend more capable than [backend], though it can fall through to
    something less capable. This can be useful when, e.g. AVX512 is slower than
    AVX2 on a CPU due to downclocking. If [backend] isn't compiled into the
    current build (e.g. requesting an x86 backend in code that also runs on
    ARM), this call is silently ignored and any previously configured limit is
    left as-is. An explicit [~backend] is not affected by this limit. Call with
    [Auto] to remove the limit.

    Process-global and takes effect immediately for any state created afterward
    (existing states are unaffected). On OCaml 4, the module-level generator is
    created when the program starts, and on OCaml 5 when a domain first uses it
    (spawning a domain counts as use). To cover both versions, call {!self_init}
    afterwards, before spawning any domains. Safe to call from multiple domains.
    The most recent call wins. *)
val set_backend_limit : backend -> unit

(** Functions that take an explicit generator state. The draw functions behave
    like the module-level functions of the same name, except that they use and
    advance the given state instead of the domain-local generator. Drawing from
    an explicit state is faster than calling the module-level functions. *)
module State : sig
  (** {2 State Management} *)

  (** Mutable generator state. Not thread-safe: do not share one across domains
      without external synchronization (no different from
      [Stdlib.Random.State.t]). *)
  type t

  (** [init ()] creates a state seeded from 256 bits of OS entropy. The sequence
      produced will differ every call and every run. For reproducible sequences,
      use {!init_deterministic}. The optional [~backend] selects the SIMD
      backend and defaults to [Auto]. Raises [Failure] if the OS entropy read
      fails or returns all zero bits (probability 2^-256 from a working source).
  *)
  val init : ?backend:backend -> unit -> t

  (** [init_deterministic seed] returns a new state seeded as described in
      {!Xoshiro_simd.init_deterministic}. *)
  val init_deterministic : ?backend:backend -> int64 -> t

  (** [init_int64array seed] returns a new state seeded as described in
      {!Xoshiro_simd.init_int64array}. *)
  val init_int64array : ?backend:backend -> int64 array -> t

  (** [init_string seed] returns a new state seeded as described in
      {!Xoshiro_simd.init_string}. *)
  val init_string : ?backend:backend -> string -> t

  (** [reseed state] reseeds [state] from 256 bits of OS entropy, like
      {!Xoshiro_simd.reseed}, and raises the same exceptions. *)
  val reseed : t -> unit

  (** [split state] is {!Xoshiro_simd.split} using [state]. After a fork, each
      [State.t] in the child is a copy of the parent's: see
      {!Xoshiro_simd.split}. *)
  val split : t -> t

  (** [recover_seed_bytes state] returns 32 bytes such that
      [init_string ~backend:(state_backend state) (recover_seed_bytes state)]
      builds a state that behaves identically to [state] from this point
      forward. This works regardless of how [state] was originally constructed.
      For a state built by {!init_string} or {!init_int64array} from at most 32
      bytes, the result is that seed padded with zeros to 32 bytes, except if
      that seed would produce the all-zero state (see
      {!Xoshiro_simd.init_int64array}). For longer string or array seeds, the
      result will not equal the input, but will reconstruct the same state.

      Raises [Invalid_argument] if [state] has buffered output left from a
      scalar draw. Bulk fills don't buffer output. In practice, call this right
      after creating the state.

      The intended use is to capture a reproducible record of an {!init}
      (OS-entropy) seed, e.g. for logging in a statistical-testing harness so a
      run can be replayed exactly later. *)
  val recover_seed_bytes : t -> string

  (** [state_backend state] returns the SIMD backend in use by [state]. *)
  val state_backend : t -> backend

  (** {2 Scalar Draws} *)

  (** [bits state] is {!Xoshiro_simd.bits} using [state]. *)
  val bits : t -> int

  (** [bits32 state] is {!Xoshiro_simd.bits32} using [state]. *)
  val bits32 : t -> int32

  (** [bits64 state] is {!Xoshiro_simd.bits64} using [state]. *)
  val bits64 : t -> int64

  (** [int state bound] is {!val:Xoshiro_simd.int} using [state]. *)
  val int : t -> int -> int

  (** [int32 state bound] is {!val:Xoshiro_simd.int32} using [state]. *)
  val int32 : t -> int32 -> int32

  (** [int64 state bound] is {!val:Xoshiro_simd.int64} using [state]. *)
  val int64 : t -> int64 -> int64

  (** [full_int state bound] is {!Xoshiro_simd.full_int} using [state]. *)
  val full_int : t -> int -> int

  (** [int_in_range state ~min ~max] is {!Xoshiro_simd.int_in_range} using
      [state]. *)
  val int_in_range : t -> min:int -> max:int -> int

  (** [int32_in_range state ~min ~max] is {!Xoshiro_simd.int32_in_range} using
      [state]. *)
  val int32_in_range : t -> min:int32 -> max:int32 -> int32

  (** [int64_in_range state ~min ~max] is {!Xoshiro_simd.int64_in_range} using
      [state]. *)
  val int64_in_range : t -> min:int64 -> max:int64 -> int64

  (** [bool state] is {!val:Xoshiro_simd.bool} using [state]. *)
  val bool : t -> bool

  (** [nativebits state] is {!Xoshiro_simd.nativebits} using [state]. *)
  val nativebits : t -> nativeint

  (** [nativeint state bound] is {!val:Xoshiro_simd.nativeint} using [state]. *)
  val nativeint : t -> nativeint -> nativeint

  (** [nativeint_in_range state ~min ~max] is {!Xoshiro_simd.nativeint_in_range}
      using [state]. *)
  val nativeint_in_range : t -> min:nativeint -> max:nativeint -> nativeint

  (** [uniform_float state] is {!Xoshiro_simd.uniform_float} using [state]. *)
  val uniform_float : t -> float

  (** [float state bound] is {!val:Xoshiro_simd.float} using [state]. *)
  val float : t -> float -> float

  (** [float_strict state bound] is {!Xoshiro_simd.float_strict} using [state].
  *)
  val float_strict : t -> float -> float

  (** [float_range state ~min ~max] is {!Xoshiro_simd.float_range} using
      [state]. *)
  val float_range : t -> min:float -> max:float -> float

  (** [float_range_strict state ~min ~max] is {!Xoshiro_simd.float_range_strict}
      using [state]. *)
  val float_range_strict : t -> min:float -> max:float -> float

  (** {2 Bulk Fills} *)

  (** [get_insecure_bytes state n] is {!Xoshiro_simd.get_insecure_bytes} using
      [state]. *)
  val get_insecure_bytes : t -> int -> bytes

  (** [fill_insecure_bytes state bytes n] is {!Xoshiro_simd.fill_insecure_bytes}
      using [state]. *)
  val fill_insecure_bytes : t -> bytes -> int -> unit

  (** [fill_insecure_bigarray state ba] is
      {!Xoshiro_simd.fill_insecure_bigarray} using [state]. *)
  val fill_insecure_bigarray : t -> ('a, 'b, 'c) Bigarray.Array1.t -> unit

  (** [fill_float32_bigarray state ba] is {!Xoshiro_simd.fill_float32_bigarray}
      using [state]. *)
  val fill_float32_bigarray :
    t -> (float, Bigarray.float32_elt, 'c) Bigarray.Array1.t -> unit

  (** [fill_float64_bigarray state ba] is {!Xoshiro_simd.fill_float64_bigarray}
      using [state]. *)
  val fill_float64_bigarray :
    t -> (float, Bigarray.float64_elt, 'c) Bigarray.Array1.t -> unit

  (** [fill_float32_range_bigarray state ba ~min ~max] is
      {!Xoshiro_simd.fill_float32_range_bigarray} using [state]. *)
  val fill_float32_range_bigarray :
    t ->
    (float, Bigarray.float32_elt, 'c) Bigarray.Array1.t ->
    min:float ->
    max:float ->
    unit

  (** [fill_float64_range_bigarray state ba ~min ~max] is
      {!Xoshiro_simd.fill_float64_range_bigarray} using [state]. *)
  val fill_float64_range_bigarray :
    t ->
    (float, Bigarray.float64_elt, 'c) Bigarray.Array1.t ->
    min:float ->
    max:float ->
    unit

  (** {2 Choosing, Shuffling, and Sampling}

      These have the same size limit as the module-level functions: see
      {!Xoshiro_simd.sampling_size_limit}. *)

  (** [choice state arr] is {!Xoshiro_simd.choice} using [state]. *)
  val choice : t -> 'a array -> 'a

  (** [choice_opt state arr] is {!Xoshiro_simd.choice_opt} using [state]. *)
  val choice_opt : t -> 'a array -> 'a option

  (** [shuffle state arr] is {!Xoshiro_simd.shuffle} using [state]. *)
  val shuffle : t -> 'a array -> unit

  (** [shuffled state arr] is {!Xoshiro_simd.shuffled} using [state]. *)
  val shuffled : t -> 'a array -> 'a array

  (** [shuffled_list state l] is {!Xoshiro_simd.shuffled_list} using [state]. *)
  val shuffled_list : t -> 'a list -> 'a list

  (** [permutation state n] is {!Xoshiro_simd.permutation} using [state]. *)
  val permutation : t -> int -> int array

  (** [reservoir_sample state arr k] is {!Xoshiro_simd.reservoir_sample} using
      [state]. *)
  val reservoir_sample : t -> 'a array -> int -> 'a array

  (** {2 Weighted Choice} *)

  (** [weighted_bool state p] is {!Xoshiro_simd.weighted_bool} using [state]. *)
  val weighted_bool : t -> float -> bool

  (** See {!type:Xoshiro_simd.weighted_table_float}. *)
  type 'a weighted_table_float

  (** [weighted_choice_linear_float state arr] is
      {!Xoshiro_simd.weighted_choice_linear_float} using [state]. *)
  val weighted_choice_linear_float : t -> ('a * float) array -> 'a

  (** Same as {!val:Xoshiro_simd.weighted_table_float}. *)
  val weighted_table_float : ('a * float) array -> 'a weighted_table_float

  (** [weighted_choice_float state tbl] is {!Xoshiro_simd.weighted_choice_float}
      using [state]. *)
  val weighted_choice_float : t -> 'a weighted_table_float -> 'a

  (** See {!type:Xoshiro_simd.weighted_table_int}. *)
  type 'a weighted_table_int

  (** [weighted_choice_linear_int state arr] is
      {!Xoshiro_simd.weighted_choice_linear_int} using [state]. *)
  val weighted_choice_linear_int : t -> ('a * int) array -> 'a

  (** Same as {!val:Xoshiro_simd.weighted_table_int}. *)
  val weighted_table_int : ('a * int) array -> 'a weighted_table_int

  (** [weighted_choice_int state tbl] is {!Xoshiro_simd.weighted_choice_int}
      using [state]. *)
  val weighted_choice_int : t -> 'a weighted_table_int -> 'a

  (**/**)

  (* {2 Testing only}

     The functions below exist solely to support this library's own test suite.
     Do not use them: any of these functions could change or disappear between
     releases without any warning. *)

  (* This encodes copies of the internal state, not aliases. *)
  type raw_dump = {
    s : int64 array;
    bitstream : int32 array;
    floatstream : floatarray;
    int_idx : int;
    float_idx : int;
  }

  val for_testing_int_lemire_c : t -> int -> int
  [@@alert testing "Only for this library's own tests."]

  val for_testing_raw_dump : t -> raw_dump
  [@@alert testing "Only for this library's own tests."]

  val for_testing_weighted_choice_linear_float_filter :
    ('a * float) array -> float * ('a * float) list
  [@@alert testing "Only for this library's own tests."]

  val for_testing_weighted_choice_linear_float :
    ('a * float) list -> float -> 'a
  [@@alert testing "Only for this library's own tests."]

  val for_testing_weighted_choice_float : 'a weighted_table_float -> float -> 'a
  [@@alert testing "Only for this library's own tests."]

  val for_testing_weighted_choice_linear_int_filter :
    ('a * int) array -> int * ('a * int) list
  [@@alert testing "Only for this library's own tests."]

  val for_testing_weighted_choice_linear_int : ('a * int) list -> int -> 'a
  [@@alert testing "Only for this library's own tests."]

  val for_testing_weighted_choice_int : 'a weighted_table_int -> int -> 'a
  [@@alert testing "Only for this library's own tests."]

  (**/**)
end

(** {2 State Management} *)

(** Alias for {!State.t}. *)
type t = State.t

(** [reseed ()] reseeds the domain-local generator from 256 bits of OS entropy.
    Internally, this uses fast OS random syscalls like getrandom or getentropy
    if available. To also choose a different SIMD backend, use {!self_init},
    which creates a new domain-local state instead of mutating the existing one.

    As with {!self_init}, raises [Failure] if the OS entropy read fails or
    returns all zero bits, leaving the state unchanged.

    After a fork, call this in the child to give it a fresh, non-reproducible
    state. {!split} gives a reproducible state. *)
val reseed : unit -> unit

(** [self_init ()] replaces the domain's state with a new one seeded from OS
    entropy, like [Stdlib.Random.self_init]. The optional [~backend] defaults to
    [Auto], so this is how to apply {!set_backend_limit} to the module-level
    functions. Unlike {!reseed}, this can change the backend. {!State.init}
    returns the new state instead.

    Raises [Failure] if the OS entropy read fails or returns all zero bits
    (probability 2^-256 from a working source), leaving the state unchanged. *)
val self_init : ?backend:backend -> unit -> unit

(** [init_deterministic seed] sets this domain's state to a state seeded by
    [seed]. The same seed gives the same sequence on every host, with the
    exceptions listed above. As the input is an int64, only 2^64 different
    initial states are possible. *)
val init_deterministic : ?backend:backend -> int64 -> unit

(** [init_int64array seed] sets this domain's state to a state seeded by [seed].
    All output is fully determined by [seed], as with {!init_deterministic} and
    with the same caveats. This function uses all of the entropy in [seed], up
    to the generator's full 256 bit state. A seed shorter than 4 elements is
    padded with zeros to 4, so [[| 1L |]] and [[| 1L; 0L; 0L; 0L |]] give the
    same state, but [[| 1L; 0L; 0L; 0L; 0L |]] does not.

    Raises [Invalid_argument] if [seed] is empty. A seed that would create the
    all-zero state (probability of about 2^-256) gives the
    [init_deterministic 42L] state instead. *)
val init_int64array : ?backend:backend -> int64 array -> unit

(** [init_string seed] sets this domain's state to a state seeded by [seed].
    Like {!init_int64array}, but the seed is a string. The seed is read as
    little-endian 8 byte words, the last one padded with zero bytes, and then
    used as in {!init_int64array}. So trailing zero bytes change the state only
    if they add a word beyond the fourth. Raises [Invalid_argument] if [seed] is
    empty. *)
val init_string : ?backend:backend -> string -> unit

(** [split ()] returns a new, independent state derived from the domain-local
    generator, using the same SIMD backend as that generator. Use it to get an
    explicit state without seeding one yourself.

    It is equivalent to seeding a fresh state from 32 bytes of the domain-local
    generator's own output. This draws from and advances the domain-local
    generator like any other output, so repeated calls never return the same
    state except with probability on the order of 2^-256. If the generator was
    seeded deterministically, the results are reproducible.

    Creating a new domain automatically gives the domain a state split from its
    parent, but forking does not. When forking, to avoid identical output from
    the children, you must split in the parent once per child and have each
    child use its split state with [State.*] functions, or [reseed ()] in the
    child. *)
val split : unit -> t

(** {2 Scalar Draws} *)

(** [bits ()] returns 30 random bits as a nonnegative integer. *)
val bits : unit -> int

(** [bits32 ()] returns 32 random bits as an [int32]. *)
val bits32 : unit -> int32

(** [bits64 ()] returns 64 random bits as an [int64]. *)
val bits64 : unit -> int64

(** [int bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive), using Lemire's method. For the same seed, output is identical
    on all hosts.

    Raises [Invalid_argument] if [bound <= 0] or [bound >= 2^30]. *)
val int : int -> int

(** [int32 bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive).

    Raises [Invalid_argument] if [bound <= 0]. *)
val int32 : int32 -> int32

(** [int64 bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive).

    Raises [Invalid_argument] if [bound <= 0]. *)
val int64 : int64 -> int64

(** [full_int bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive). [bound] may be any positive integer.

    If [bound] is less than 2^30, then [full_int bound] equals [int bound] on
    every host. Larger bounds, on 64 bit systems only, use debiased modulo like
    [Stdlib.Random.int], which costs at least one division per draw.

    Raises [Invalid_argument] if [bound <= 0]. *)
val full_int : int -> int

(** [int_in_range ~min ~max] returns a random integer between [min] (inclusive)
    and [max] (inclusive). Both [min] and [max] may be negative.

    For any [min] and [max] that are valid on both 32 bit and 64 bit systems,
    the output is identical on both. Ranges of 2^30 or more are slower.

    For wider ranges than a native int can hold, use
    [int32_in_range]/[int64_in_range] (identical on every host) or
    [nativeint_in_range].

    Raises [Invalid_argument] if [min > max]. *)
val int_in_range : min:int -> max:int -> int

(** [int32_in_range ~min ~max] returns a random integer between [min]
    (inclusive) and [max] (inclusive). Both [min] and [max] may be negative.

    Raises [Invalid_argument] if [min > max]. *)
val int32_in_range : min:int32 -> max:int32 -> int32

(** [int64_in_range ~min ~max] returns a random integer between [min]
    (inclusive) and [max] (inclusive). Both [min] and [max] may be negative.

    Raises [Invalid_argument] if [min > max]. *)
val int64_in_range : min:int64 -> max:int64 -> int64

(** [bool ()] returns [true] or [false] with equal probability. *)
val bool : unit -> bool

(** [nativebits ()] returns 32 or 64 random bits depending on the platform word
    size. *)
val nativebits : unit -> nativeint

(** [nativeint bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive).

    Raises [Invalid_argument] if [bound <= 0]. *)
val nativeint : nativeint -> nativeint

(** [nativeint_in_range ~min ~max] returns a random integer between [min]
    (inclusive) and [max] (inclusive). Both [min] and [max] may be negative.

    Raises [Invalid_argument] if [min > max]. *)
val nativeint_in_range : min:nativeint -> max:nativeint -> nativeint

(** [uniform_float ()] returns a uniformly distributed float with 53 bits of
    precision between 0.0 and 1.0. 0.0 is a possible result. 1.0 is never
    returned. Use [float_strict 1.0] where 0 must be excluded, e.g. before a
    logarithm. *)
val uniform_float : unit -> float

(** [float bound] returns a random float between 0 and [bound]. 0.0 can be
    returned exactly. [bound] will not be returned exactly except when
    [abs bound <= Float.min_float] (the smallest normal float, about 2.2e-308).
    A negative [bound] works the same way. If [bound] is 0 or NaN, returns
    [bound]. If [bound] is infinity, the result is infinity or NaN. Use
    {!float_strict} to exclude both ends. *)
val float : float -> float

(** [float_strict bound] returns a random float strictly between 0 and [bound]
    (both exclusive), via rejection sampling.

    If [abs bound <= Float.min_float], which is the smallest normal float (about
    2.2e-308), the result can equal [bound] exactly.

    Raises [Invalid_argument] if [bound] is NaN, infinite or zero. *)
val float_strict : float -> float

(** [float_range ~min ~max] returns a random float between [min] and [max].
    [min] can be returned exactly, with probability at least 2^-53. When fewer
    than 2^53 float values lie between [min] and [max], results are rounded to
    the nearest representable float, which makes [min] more likely and can let
    [max] occur. For example, a range of 1.0 to 2.0 can return 2.0. If
    [min = max] the result is [min].

    Raises [Invalid_argument] if [min > max], if either [min] or [max] is NaN,
    or if [max -. min] overflows to infinity. Use {!float_range_strict} if you
    need a guarantee that the result is strictly between [min] and [max]. *)
val float_range : min:float -> max:float -> float

(** [float_range_strict ~min ~max] returns a random float strictly between [min]
    and [max] (both exclusive), via rejection sampling.

    Raises [Invalid_argument] if [min > max], if either [min] or [max] is NaN,
    if [max -. min] overflows to infinity, or if [min] and [max] have no float
    values strictly between them. *)
val float_range_strict : min:float -> max:float -> float

(** {2 Bulk Fills} *)

(** [get_insecure_bytes n] returns [n] random bytes.

    For a fixed seed, this produces byte-for-byte identical output on every
    host, regardless of byte order.

    Each call generates whole 32 byte blocks and discards the rest, so several
    calls don't give the same bytes as one call unless every size except the
    last is a multiple of 32.

    Raises [Invalid_argument] if [n < 0] or [n > Sys.max_string_length] (just
    under 16 MiB on 32 bit platforms). *)
val get_insecure_bytes : int -> bytes

(** [fill_insecure_bytes bytes n] fills the first [n] bytes of [bytes] with
    random bytes in place, leaving the rest of the buffer untouched. Allocates
    nothing. Same byte-for-byte cross-host reproducibility and 32 byte block
    rounding as {!get_insecure_bytes}.

    Raises [Invalid_argument] if [n < 0] or [n] exceeds the length of [bytes].
*)
val fill_insecure_bytes : bytes -> int -> unit

(** [fill_insecure_bigarray ba] fills [ba] with random bytes in place. The
    entire backing memory of [ba] is overwritten regardless of element kind.

    For a fixed seed, every element is the same value on every host, except for
    the [int] and [nativeint] kinds, whose width is the host word size.

    Each call generates whole 32 byte blocks and discards the rest, so small
    fills will be less efficient than larger fills.

    You can use this function to fill float bigarrays, but the results will be
    random bit patterns with NaNs and infinities possible: use
    {!fill_float32_bigarray} or {!fill_float64_bigarray} to fill float arrays.
*)
val fill_insecure_bigarray : ('a, 'b, 'c) Bigarray.Array1.t -> unit

(** [fill_float32_bigarray ba] fills [ba] with uniformly distributed 32 bit
    floats between 0.0 (inclusive) and 1.0 (exclusive), using 24 bits of
    precision per value (float32's full mantissa precision). *)
val fill_float32_bigarray :
  (float, Bigarray.float32_elt, 'c) Bigarray.Array1.t -> unit

(** [fill_float64_bigarray ba] fills [ba] with uniformly distributed 64 bit
    floats between 0.0 (inclusive) and 1.0 (exclusive), using 53 bits of
    precision per value. *)
val fill_float64_bigarray :
  (float, Bigarray.float64_elt, 'c) Bigarray.Array1.t -> unit

(** [fill_float32_range_bigarray ba ~min ~max] fills [ba] with values between
    [min] and [max], using 24 bits of precision per value. Both [min] and [max]
    are rounded to float32 first, and the following refers to the rounded
    values. [min] can occur exactly, with probability at least 2^-24 per
    element. When fewer than 2^24 float32 values lie between [min] and [max],
    results are rounded to the nearest representable float32, which makes [min]
    more likely and can let [max] occur. For example, a range of 1.0 to 2.0 can
    return 2.0. If [min = max] every element is [min]. There is no strict
    version: use a [min] of at least the smallest positive float32 (about
    1.4e-45) to exclude 0. [max] can still occur.

    Raises [Invalid_argument] if [min > max], if either [min] or [max] is NaN,
    or if [min], [max], or their difference is infinite after rounding to
    float32. *)
val fill_float32_range_bigarray :
  (float, Bigarray.float32_elt, 'c) Bigarray.Array1.t ->
  min:float ->
  max:float ->
  unit

(** [fill_float64_range_bigarray ba ~min ~max] fills [ba] with values between
    [min] and [max]. [min] can occur exactly, with probability at least 2^-53
    per element. When fewer than 2^53 float64 values lie between [min] and
    [max], results are rounded to the nearest representable float64, which makes
    [min] more likely and can let [max] occur. For example, a range of 1.0 to
    2.0 can return 2.0. If [min = max] every element is [min]. There is no
    strict version: use a positive [min] to exclude 0. [max] can still occur.

    Raises [Invalid_argument] if [min > max], if either [min] or [max] is NaN,
    or if [max -. min] overflows to infinity. *)
val fill_float64_range_bigarray :
  (float, Bigarray.float64_elt, 'c) Bigarray.Array1.t ->
  min:float ->
  max:float ->
  unit

(** {2:sampling_size_limit Choosing, Shuffling, and Sampling}

    Every function below is built on {!int} internally, so all of these raise
    [Invalid_argument] if the input array (or [n] for {!permutation}) has more
    than 2^30 - 1 elements. *)

(** [choice arr] returns a uniformly chosen random element of [arr].

    Raises [Invalid_argument] if [arr] is empty or if [arr] has more than 2^30 -
    1 elements. *)
val choice : 'a array -> 'a

(** [choice_opt arr] returns [Some x] where [x] is a uniformly chosen random
    element of [arr], or [None] if [arr] is empty.

    Raises [Invalid_argument] if [arr] has more than 2^30 - 1 elements. *)
val choice_opt : 'a array -> 'a option

(** [shuffle arr] shuffles [arr] in place using the Fisher-Yates algorithm. If n
    is the length of [arr] and n! > 2^256 (that is, n >= 58), then most
    permutations will be impossible.

    Raises [Invalid_argument] if [arr] has more than 2^30 - 1 elements. *)
val shuffle : 'a array -> unit

(** [shuffled arr] returns a freshly allocated shuffled copy of [arr]. [arr] is
    not modified. See also {!shuffle}.

    Raises [Invalid_argument] if [arr] has more than 2^30 - 1 elements. *)
val shuffled : 'a array -> 'a array

(** [shuffled_list l] returns a uniformly random permutation of [l] as a new
    list. See also {!shuffle}.

    Raises [Invalid_argument] if [l] has more than 2^30 - 1 elements. *)
val shuffled_list : 'a list -> 'a list

(** [permutation n] returns a uniformly random permutation of the integers [0]
    to [n - 1]. If n! > 2^256 (that is, n >= 58), then most permutations will be
    impossible.

    Raises [Invalid_argument] if [n < 0] or [n > 2^30 - 1]. *)
val permutation : int -> int array

(** [reservoir_sample arr k] returns [k] elements chosen uniformly at random
    without replacement from [arr], in a random order. If n is the length of
    [arr] and n!/(n-k)! > 2^256, then some of those possible samples can never
    be produced.

    Allocates a temporary copy of [arr] ([O(n)] space, not [O(k)]).

    Raises [Invalid_argument] if [k < 0], [k > Array.length arr], or if [arr]
    has more than 2^30 - 1 elements. *)
val reservoir_sample : 'a array -> int -> 'a array

(** {2 Weighted Choice} *)

(** [weighted_bool p] returns [true] with probability [p] and [false] with
    probability [1 - p]. [p] should be between 0 and 1 inclusive: [p <= 0]
    always returns false, and [p >= 1] always returns true. *)
val weighted_bool : float -> bool

(** A precomputed weight table for use with {!weighted_choice_float}. Create one
    with {!val:weighted_table_float}. Tables are immutable and may be used with
    different states. *)
type 'a weighted_table_float = 'a State.weighted_table_float

(** [weighted_choice_linear_float arr] draws one element from [arr] with
    probability proportional to its weight, in O(n) time. Items with weight 0
    are never selected. For repeated sampling from the same distribution prefer
    {!val:weighted_table_float} and {!weighted_choice_float}.

    Raises [Invalid_argument] if [arr] is empty, any weight is negative,
    infinite, or NaN, or all weights sum to zero or infinity. *)
val weighted_choice_linear_float : ('a * float) array -> 'a

(** [weighted_table_float arr] precomputes a cumulative-weight table from [arr].
    Building the table takes O(n log n) time, as it sorts the weights first to
    reduce rounding error. Each {!weighted_choice_float} call then runs in O(log
    n). Items with weight 0 are never selected.

    Raises [Invalid_argument] if [arr] is empty, any weight is negative,
    infinite, or NaN, or all weights sum to zero or infinity. *)
val weighted_table_float : ('a * float) array -> 'a weighted_table_float

(** [weighted_choice_float tbl] draws one element from [tbl] with probability
    proportional to its weight, in O(log n) time. Build [tbl] with
    {!val:weighted_table_float}. *)
val weighted_choice_float : 'a weighted_table_float -> 'a

(** A precomputed weight table for use with {!weighted_choice_int}. Create one
    with {!val:weighted_table_int}. Tables are immutable and may be used with
    different states. *)
type 'a weighted_table_int = 'a State.weighted_table_int

(** [weighted_choice_linear_int arr] draws one element from [arr] with
    probability proportional to its (integer) weight, in O(n) time. Items with
    weight 0 are never selected. For repeated sampling from the same
    distribution prefer {!val:weighted_table_int} and {!weighted_choice_int}.

    Raises [Invalid_argument] if [arr] is empty, any weight is negative, all
    weights sum to zero, or all weights sum to > 2^30 - 1. *)
val weighted_choice_linear_int : ('a * int) array -> 'a

(** [weighted_table_int arr] precomputes a cumulative-weight table from [arr].
    Building the table takes O(n) time: each subsequent {!weighted_choice_int}
    call then runs in O(log n). Items with weight 0 are never selected.

    Raises [Invalid_argument] if [arr] is empty, any weight is negative, all
    weights sum to zero, or all weights sum to > 2^30 - 1. *)
val weighted_table_int : ('a * int) array -> 'a weighted_table_int

(** [weighted_choice_int tbl] draws one element from [tbl] with probability
    proportional to its weight, in O(log n) time. Build [tbl] with
    {!val:weighted_table_int}. *)
val weighted_choice_int : 'a weighted_table_int -> 'a

(**/**)

(* {2 Testing only}

   These exist solely to support this library's own test suite. Do not use,
   even in your own tests, because these functions could change or disappear
   between releases without any warning. *)

val for_testing_dls_variant : unit -> string
[@@alert testing "Only for this library's own tests."]

val for_testing_spawn_and_join : (unit -> 'a) -> 'a option
[@@alert testing "Only for this library's own tests."]

(* This lets us test the C Lemire reduction step with a chosen raw int32. *)
val for_testing_lemire_reduce32_wide : int -> int32 -> int option
[@@alert testing "Only for this library's own tests."]

val for_testing_mix_entropy_inverse : int64 array -> int64 array
[@@alert testing "Only for this library's own tests."]
