module type S = sig
  type backend = C_intf.backend =
    | Auto
    | Plain
    | Sse2
    | Avx2
    | Avx512fvl
    | Neon
  type t

  val backend_equal : backend -> backend -> bool
  val backend_to_string : backend -> string
  val available_backends : unit -> backend list
  val state_backend : t -> backend
  val init : ?backend:backend -> unit -> t
  val init_deterministic : ?backend:backend -> int64 -> t
  val init_int64array : ?backend:backend -> int64 array -> t
  val init_string : ?backend:backend -> string -> t
  val reseed : t -> unit
  val bits : t -> int
  val bits30 : t -> int
  val bits32 : t -> int32
  val bits64 : t -> int64
  val uniform_float : t -> float
  val uniform_float_strict : t -> float
  val intrange_int32 : t -> int32 -> int32
  val intrange_int64 : t -> int64 -> int64
  val int : t -> int -> int
  val int_java : t -> int -> int
  val int_lemire_c : t -> int -> int
  val int32 : t -> int32 -> int32
  val int64 : t -> int64 -> int64
  val full_int : t -> int -> int
  val float : t -> float -> float
  val float_strict : t -> float -> float
  val float_range : t -> min:float -> max:float -> float
  val float_range_strict : t -> min:float -> max:float -> float
  val rejection_sample : t -> (t -> 'a) -> 'a -> 'a -> 'a
  val full_int_bits : t -> int
  val int_in_range : t -> min:int -> max:int -> int
  val int32_in_range : t -> min:int32 -> max:int32 -> int32
  val int64_in_range : t -> min:int64 -> max:int64 -> int64
  val bool : t -> bool
  val nativebits : t -> nativeint
  val nativeint : t -> nativeint -> nativeint
  val nativeint_in_range : t -> min:nativeint -> max:nativeint -> nativeint
  val get_insecure_bytes : t -> int -> bytes
  val fill_insecure_bytes : t -> bytes -> int -> unit
  val fill_insecure_bigarray : t -> ('a, 'b, 'c) Bigarray.Array1.t -> unit

  val fill_float32_bigarray :
    t -> (float, Bigarray.float32_elt, 'c) Bigarray.Array1.t -> unit

  val fill_float64_bigarray :
    t -> (float, Bigarray.float64_elt, 'c) Bigarray.Array1.t -> unit

  val fill_float32_range_bigarray :
    t ->
    (float, Bigarray.float32_elt, 'c) Bigarray.Array1.t ->
    min:float ->
    max:float ->
    unit

  val fill_float64_range_bigarray :
    t ->
    (float, Bigarray.float64_elt, 'c) Bigarray.Array1.t ->
    min:float ->
    max:float ->
    unit

  val split : t -> t
  val choice : t -> 'a array -> 'a
  val choice_opt : t -> 'a array -> 'a option
  val shuffle : t -> 'a array -> unit
  val shuffled : t -> 'a array -> 'a array
  val shuffled_list : t -> 'a list -> 'a list
  val permutation : t -> int -> int array
  val reservoir_sample : t -> 'a array -> int -> 'a array
  val weighted_bool : t -> float -> bool

  type 'a weighted_table_float

  val weighted_choice_linear_float : t -> ('a * float) array -> 'a
  val weighted_table_float : ('a * float) array -> 'a weighted_table_float
  val weighted_choice_float : t -> 'a weighted_table_float -> 'a

  type 'a weighted_table_int

  val weighted_choice_linear_int : t -> ('a * int) array -> 'a
  val weighted_table_int : ('a * int) array -> 'a weighted_table_int
  val weighted_choice_int : t -> 'a weighted_table_int -> 'a

  val for_testing_weighted_choice_linear_float_filter :
    ('a * float) array -> float * ('a * float) list
  val for_testing_weighted_choice_linear_float :
    ('a * float) list -> float -> 'a
  val for_testing_weighted_choice_float : 'a weighted_table_float -> float -> 'a

  val for_testing_weighted_choice_linear_int_filter :
    ('a * int) array -> int * ('a * int) list
  val for_testing_weighted_choice_linear_int : ('a * int) list -> int -> 'a
  val for_testing_weighted_choice_int : 'a weighted_table_int -> int -> 'a
end

module I64 = struct
  external ( + ) : int64 -> int64 -> int64 = "%int64_add"
  external ( - ) : int64 -> int64 -> int64 = "%int64_sub"
  external ( mod ) : int64 -> int64 -> int64 = "%int64_mod"
  external ( > ) : int64 -> int64 -> bool = "%greaterthan"
  external ( land ) : int64 -> int64 -> int64 = "%int64_and"
  external ( lsr ) : int64 -> int -> int64 = "%int64_lsr"
  let of_int = Int64.of_int
  let to_int = Int64.to_int
end

module [@inline] Make (R : Rand_core_intf.S) : S with type t = R.state = struct
  type backend = R.backend = Auto | Plain | Sse2 | Avx2 | Avx512fvl | Neon
  type t = R.state
  type 'a weighted_table_float = ('a * float) array
  type 'a weighted_table_int = ('a * int) array

  let backend_equal = R.backend_equal
  let backend_to_string = R.backend_to_string
  let available_backends = R.available_backends
  let state_backend = R.state_backend
  let init = R.init
  let init_deterministic = R.init_deterministic
  let init_int64array = R.init_int64array
  let init_string = R.init_string
  let reseed = R.reseed
  let bits = R.bits30
  let bits30 = R.bits30
  let bits32 = R.bits32
  let bits64 = R.bits64
  let uniform_float = R.uniform_float
  let uniform_float_strict = R.uniform_float_strict
  let intrange_int32 = R.intrange_int32
  let intrange_int64 = R.intrange_int64
  let int = R.int
  let int_java = R.int_java
  let int_lemire_c = R.int_lemire_c
  let int32 = intrange_int32
  let int64 = intrange_int64
  let full_int = R.full_int
  let float = R.float
  let float_strict = R.float_strict
  let bool = R.bool

  let float_range state ~min ~max =
    (* One fast comparison to determine if the arguments are valid, and the
       chain below only picks the message. *)
    if not (min <= max && max -. min <= Float.max_float) then (
      if Float.is_nan min then
        invalid_arg "Xoshiro_simd.float_range: min is NaN";
      if Float.is_nan max then
        invalid_arg "Xoshiro_simd.float_range: max is NaN";
      let span = Float.abs (max -. min) in
      (* Guard against 80 bit floats on i386 that would be infinite as doubles. *)
      if span > Float.max_float then
        invalid_arg "Xoshiro_simd.float_range: range is infinite";
      if Float.is_nan span then
        invalid_arg "Xoshiro_simd.float_range: range is NaN";
      if min > max then
        invalid_arg "Xoshiro_simd.float_range: min must not be greater than max");
    let diff = max -. min in
    let r = float state diff in
    r +. min

  let float_range_strict state ~min ~max =
    if not (Float.succ min < max && max -. min <= Float.max_float) then (
      if Float.is_nan min then
        invalid_arg "Xoshiro_simd.float_range_strict: min is NaN";
      if Float.is_nan max then
        invalid_arg "Xoshiro_simd.float_range_strict: max is NaN";
      if Float.abs (max -. min) > Float.max_float then
        invalid_arg "Xoshiro_simd.float_range_strict: range is infinite";
      if min > max then
        invalid_arg
          "Xoshiro_simd.float_range_strict: min must not be greater than max";
      if max = Float.succ min || max = min then
        invalid_arg "Xoshiro_simd.float_range_strict: unsatisfiable range");
    let diff = max -. min in
    let r = ref ((uniform_float state *. diff) +. min) in
    while !r <= min || !r >= max do
      r := (uniform_float state *. diff) +. min
    done;
    !r

  let rec rejection_sample state rand min max =
    let sample = rand state in
    if sample < min || sample > max then rejection_sample state rand min max
    else sample

  let full_int_bits =
    if Sys.word_size = 32 then R.signed_bits31
    else fun s ->
      let i64 = R.bits64 s in
      (* Int64.to_int crops off the leading bit. *)
      Int64.to_int i64

  let int_in_range state ~min ~max : int =
    if min > max then
      invalid_arg "Xoshiro_simd.int_in_range: min must not be greater than max";
    (* If we can't represent the span as a positive native integer, then on 32
       bit platforms, we emulate the 64 bit path of full_int to ensure identical
       output on both platforms. On 64 bit, encountering this means we have a
       range wider than max_int, so we use rejection sampling. *)
    let span = max - min + 1 in
    if span <= 0 then
      if Sys.word_size = 32 then
        let r =
          let open I64 in
          let i64span = of_int max - of_int min + 1L in
          let rec full_int_loop state bound =
            (* This copies full_int_loop from rand_core, but with int64s. *)
            let r = (bits64 state land 0x7fff_ffff_ffff_ffffL) lsr 1 in
            let v = r mod bound in
            if r - v > 0x3fff_ffff_ffff_ffffL - bound + 1L then
              full_int_loop state bound
            else v
          in
          full_int_loop state i64span |> to_int
        in
        r + min
      else rejection_sample state full_int_bits min max
    else
      let r = full_int state span in
      r + min

  let int32_in_range state ~min ~max =
    if min > max then
      invalid_arg
        "Xoshiro_simd.int32_in_range: min must not be greater than max";
    let span = Int32.succ (Int32.sub max min) in
    if span <= 0l then rejection_sample state R.bits32 min max
    else
      let r = int32 state span in
      Int32.add r min

  let int64_in_range state ~min ~max =
    if min > max then
      invalid_arg
        "Xoshiro_simd.int64_in_range: min must not be greater than max";
    let span = Int64.succ (Int64.sub max min) in
    if span <= 0L then rejection_sample state R.bits64 min max
    else
      let r = int64 state span in
      Int64.add r min

  let nativebits =
    if Sys.word_size = 32 then fun state -> Nativeint.of_int32 (R.bits32 state)
    else fun state -> Int64.to_nativeint (R.bits64 state)

  let nativeint =
    if Sys.word_size = 32 then fun state bound ->
      Nativeint.of_int32 (int32 state (Nativeint.to_int32 bound))
    else fun state bound ->
      Int64.to_nativeint (int64 state (Int64.of_nativeint bound))

  let nativeint_in_range =
    if Sys.word_size = 32 then fun state ~min ~max ->
      Nativeint.of_int32
        (int32_in_range state ~min:(Nativeint.to_int32 min)
           ~max:(Nativeint.to_int32 max))
    else fun state ~min ~max ->
      Int64.to_nativeint
        (int64_in_range state ~min:(Int64.of_nativeint min)
           ~max:(Int64.of_nativeint max))

  (* "insecure" to remind the user that this is not a CSPRNG: random bytes are
     a common case of needing a CSPRNG. *)
  let get_insecure_bytes state n =
    if n < 0 || n > Sys.max_string_length then
      invalid_arg
        "Xoshiro_simd.get_insecure_bytes: n must be between 0 and \
         Sys.max_string_length";
    let bytes = Bytes.create n in
    R.get_bytes state bytes n;
    bytes

  let fill_insecure_bytes state bytes n =
    if n < 0 || n > Bytes.length bytes then
      invalid_arg
        "Xoshiro_simd.fill_insecure_bytes: n must be between 0 and the \
         buffer's length";
    R.get_bytes state bytes n

  let fill_insecure_bigarray = R.get_bytes_bigarray
  let fill_float32_bigarray = R.fill_float32_bigarray
  let fill_float64_bigarray = R.fill_float64_bigarray
  let fill_float32_range_bigarray = R.fill_float32_range_bigarray
  let fill_float64_range_bigarray = R.fill_float64_range_bigarray

  (* Split seeds the new state with 32 bytes from the parent, which changes the
     state of the parent, so any number of splits can be done in any order with
     negligible probability of a state collision. *)
  let split state =
    let new_seed = get_insecure_bytes state 32 in
    init_string ~backend:(state_backend state) (Bytes.unsafe_to_string new_seed)

  (* Higher level random functions. *)

  let choice state arr =
    let len = Array.length arr in
    if len > R.max_int31 then invalid_arg "Xoshiro_simd.choice: array too large";
    if len = 0 then invalid_arg "Xoshiro_simd.choice: empty array";
    arr.(int state len)

  let choice_opt state arr =
    let len = Array.length arr in
    if len > R.max_int31 then
      invalid_arg "Xoshiro_simd.choice_opt: array too large";
    if len = 0 then None else Some arr.(int state len)

  let array_swap arr i1 i2 =
    let t = arr.(i1) in
    arr.(i1) <- arr.(i2);
    arr.(i2) <- t

  (* Fisher-Yates shuffle. *)
  let shuffle state arr =
    let len = Array.length arr in
    if len > R.max_int31 then
      invalid_arg "Xoshiro_simd.shuffle: array too large";
    for i = 0 to len - 2 do
      let maxn = len - i in
      let randn = int state maxn in
      array_swap arr i (i + randn)
    done

  let shuffled state arr =
    let arr = Array.copy arr in
    shuffle state arr;
    arr

  let shuffled_list state l =
    let arr = Array.of_list l in
    shuffle state arr;
    Array.to_list arr

  let permutation state len =
    if len < 0 then
      invalid_arg "Xoshiro_simd.permutation: len must not be negative";
    if len > R.max_int31 then
      invalid_arg "Xoshiro_simd.permutation: len too large";
    let arr = Array.init len (fun i -> i) in
    shuffle state arr;
    arr

  let reservoir_sample state arr k =
    let input_length = Array.length arr in
    if k < 0 then
      invalid_arg "Xoshiro_simd.reservoir_sample: k must be non-negative";
    if k > input_length then
      invalid_arg
        "Xoshiro_simd.reservoir_sample: k must not exceed array length";
    if input_length > R.max_int31 then
      invalid_arg "Xoshiro_simd.reservoir_sample: array too large";
    let copy = Array.copy arr in
    let stop = Int.min (k - 1) (input_length - 2) in
    for i = 0 to stop do
      let maxn = input_length - i in
      let randn = int state maxn in
      array_swap copy i (i + randn)
    done;
    Array.sub copy 0 k

  let weighted_bool state p =
    let roll = uniform_float state in
    roll < p

  (* Weighted choice functions. *)

  let weighted_choice_linear_internal l r =
    let rec loop = function
      | [] -> assert false
      | [ (item, _) ] -> item
      | (item, sum) :: tl -> if r < sum then item else loop tl
    in
    loop l

  let kahan_add x sum c =
    let y = x -. c in
    let t = sum +. y in
    let c = t -. sum -. y in
    let sum = t in
    (sum, c)

  (* Weights of 0 are valid inputs to weighted_choice_float and
     weighted_table_float, as long as there's some positive float weight in
     there. Filtering them out here keeps the search algorithm simple. *)
  let weighted_choice_float_filter arr =
    let len = Array.length arr in
    if len = 0 then invalid_arg "Xoshiro_simd: empty array";
    let rec loop sum c i acc =
      if i = len then (
        if sum = 0. then invalid_arg "Xoshiro_simd: total sum cannot be 0";
        (sum, List.rev acc))
      else
        let weight = snd arr.(i) in
        if weight < 0. || Float.(is_infinite weight || is_nan weight) then
          invalid_arg
            "Xoshiro_simd: weights must all be nonnegative, non-infinite, \
             non-nan";
        if weight = 0. then loop sum c (i + 1) acc
        else
          let sum, c = kahan_add weight sum c in
          if not (Float.is_finite sum) then
            invalid_arg "Xoshiro_simd: total sum must be a representable float";
          let acc = (fst arr.(i), sum) :: acc in
          loop sum c (i + 1) acc
    in
    loop 0. 0. 0 []

  let weighted_choice_linear_float state arr =
    let total, filtered = weighted_choice_float_filter arr in
    let r = float state total in
    weighted_choice_linear_internal filtered r

  (* We sort the given array by weight before summing. The Kahan compensation
     done in weighted_choice_float_filter keeps the total accurate, but any
     tiny weights coming after large weights can still lose precision. By
     sorting, bin sizes are kept as close as possible to the specified weight,
     to make the selection process as fair as possible. *)
  let weighted_table_float arr =
    let sorted = Array.copy arr in
    Array.stable_sort (fun (_, f1) (_, f2) -> Float.compare f1 f2) sorted;
    let _, l = weighted_choice_float_filter sorted in
    Array.of_list l

  (* weighted_choice_float_filter filters out all 0 weights, so this function
     only searches over nonzero weights and can never return an item with 0
     weight. *)
  let weighted_choice_internal prefix_sum r =
    let len = Array.length prefix_sum in
    let rec bin_search lo hi =
      if lo = hi then fst prefix_sum.(hi)
      else
        let i = ((hi - lo) / 2) + lo in
        let sum = snd prefix_sum.(i) in
        if r >= sum then bin_search (i + 1) hi else bin_search lo i
    in
    bin_search 0 (len - 1)

  let weighted_choice_float state prefix_sum =
    let len = Array.length prefix_sum in
    let total = snd prefix_sum.(len - 1) in
    let r = float state total in
    weighted_choice_internal prefix_sum r

  (* Int weighted choice functions. *)

  let weighted_choice_int_filter arr =
    let len = Array.length arr in
    if len = 0 then invalid_arg "Xoshiro_simd: empty array";
    let rec loop sum i acc =
      if i = len then (
        if sum = 0 then invalid_arg "Xoshiro_simd: total sum cannot be 0";
        (sum, List.rev acc))
      else
        let weight = snd arr.(i) in
        if weight < 0 then
          invalid_arg "Xoshiro_simd: weights must all be nonnegative";
        if weight = 0 then loop sum (i + 1) acc
        else
          let sum = weight + sum in
          if sum < 0 then
            invalid_arg "Xoshiro_simd: total sum must be at most 2^30 - 1";
          let acc = (fst arr.(i), sum) :: acc in
          loop sum (i + 1) acc
    in
    loop 0 0 []

  let weighted_choice_linear_int state arr =
    let sum, filtered = weighted_choice_int_filter arr in
    if sum > R.max_int31 then
      invalid_arg
        "Xoshiro_simd.weighted_choice_linear_int: total sum must be at most \
         2^30 - 1";
    let r = int state sum in
    weighted_choice_linear_internal filtered r

  let weighted_table_int arr =
    let sum, l = weighted_choice_int_filter arr in
    if sum > R.max_int31 then
      invalid_arg
        "Xoshiro_simd.weighted_table_int: total sum must be at most 2^30 - 1";
    Array.of_list l

  let weighted_choice_int state prefix_sum =
    let len = Array.length prefix_sum in
    let total = snd prefix_sum.(len - 1) in
    let r = int state total in
    weighted_choice_internal prefix_sum r

  (* Testing exports. *)

  let for_testing_weighted_choice_linear_float_filter =
    weighted_choice_float_filter

  let for_testing_weighted_choice_linear_float = weighted_choice_linear_internal
  let for_testing_weighted_choice_float = weighted_choice_internal
  let for_testing_weighted_choice_linear_int_filter = weighted_choice_int_filter
  let for_testing_weighted_choice_linear_int = weighted_choice_linear_internal
  let for_testing_weighted_choice_int = weighted_choice_internal
end

module type Stateless = sig
  type backend = C_intf.backend =
    | Auto
    | Plain
    | Sse2
    | Avx2
    | Avx512fvl
    | Neon
  type t

  val get_state : unit -> t
  val set_state : t -> unit
  val backend_equal : backend -> backend -> bool
  val backend_to_string : backend -> string
  val init : ?backend:backend -> unit -> unit
  val init_deterministic : ?backend:backend -> int64 -> unit
  val init_int64array : ?backend:backend -> int64 array -> unit
  val init_string : ?backend:backend -> string -> unit
  val reseed : unit -> unit
  val bits : unit -> int
  val bits30 : unit -> int
  val bits32 : unit -> int32
  val bits64 : unit -> int64
  val uniform_float : unit -> float
  val uniform_float_strict : unit -> float
  val int : int -> int
  val int_java : int -> int
  val int32 : int32 -> int32
  val int64 : int64 -> int64
  val full_int : int -> int
  val float : float -> float
  val float_strict : float -> float
  val float_range : min:float -> max:float -> float
  val float_range_strict : min:float -> max:float -> float
  val int_in_range : min:int -> max:int -> int
  val int32_in_range : min:int32 -> max:int32 -> int32
  val int64_in_range : min:int64 -> max:int64 -> int64
  val bool : unit -> bool
  val nativebits : unit -> nativeint
  val nativeint : nativeint -> nativeint
  val nativeint_in_range : min:nativeint -> max:nativeint -> nativeint
  val get_insecure_bytes : int -> bytes
  val fill_insecure_bytes : bytes -> int -> unit
  val fill_insecure_bigarray : ('a, 'b, 'c) Bigarray.Array1.t -> unit

  val fill_float32_bigarray :
    (float, Bigarray.float32_elt, 'c) Bigarray.Array1.t -> unit

  val fill_float64_bigarray :
    (float, Bigarray.float64_elt, 'c) Bigarray.Array1.t -> unit

  val fill_float32_range_bigarray :
    (float, Bigarray.float32_elt, 'c) Bigarray.Array1.t ->
    min:float ->
    max:float ->
    unit

  val fill_float64_range_bigarray :
    (float, Bigarray.float64_elt, 'c) Bigarray.Array1.t ->
    min:float ->
    max:float ->
    unit

  val split : unit -> t
  val for_testing_dls_variant : unit -> string
  val for_testing_spawn_and_join : (unit -> 'a) -> 'a option
  val choice : 'a array -> 'a
  val choice_opt : 'a array -> 'a option
  val shuffle : 'a array -> unit
  val shuffled : 'a array -> 'a array
  val shuffled_list : 'a list -> 'a list
  val permutation : int -> int array
  val reservoir_sample : 'a array -> int -> 'a array
  val weighted_bool : float -> bool

  type 'a weighted_table_float

  val weighted_choice_linear_float : ('a * float) array -> 'a
  val weighted_table_float : ('a * float) array -> 'a weighted_table_float
  val weighted_choice_float : 'a weighted_table_float -> 'a

  type 'a weighted_table_int

  val weighted_choice_linear_int : ('a * int) array -> 'a
  val weighted_table_int : ('a * int) array -> 'a weighted_table_int
  val weighted_choice_int : 'a weighted_table_int -> 'a
end

module [@inline] MakeStateless
    (M : S)
    (D : sig
      val get_state : unit -> M.t
      val set_state : M.t -> unit
    end) :
  Stateless
    with type t = M.t
     and type backend = M.backend
     and type 'a weighted_table_float = 'a M.weighted_table_float
     and type 'a weighted_table_int = 'a M.weighted_table_int = struct
  type backend = M.backend = Auto | Plain | Sse2 | Avx2 | Avx512fvl | Neon
  type t = M.t
  type 'a weighted_table_float = 'a M.weighted_table_float
  type 'a weighted_table_int = 'a M.weighted_table_int

  let[@inline] get_state () = D.get_state ()
  let[@inline] set_state s = D.set_state s
  let backend_equal = M.backend_equal
  let backend_to_string = M.backend_to_string
  let init ?backend () = set_state (M.init ?backend ())

  let init_deterministic ?backend seed =
    set_state (M.init_deterministic ?backend seed)

  let init_int64array ?backend seed_array =
    set_state (M.init_int64array ?backend seed_array)

  let init_string ?backend seed_string =
    set_state (M.init_string ?backend seed_string)

  let reseed () = M.reseed (get_state ())
  let bits () = M.bits (get_state ())
  let bits30 () = M.bits30 (get_state ())
  let bits32 () = M.bits32 (get_state ())
  let bits64 () = M.bits64 (get_state ())
  let uniform_float () = M.uniform_float (get_state ())
  let uniform_float_strict () = M.uniform_float_strict (get_state ())

  let int bound = M.int (get_state ()) bound
  let int_java bound = M.int_java (get_state ()) bound
  let int32 bound = M.int32 (get_state ()) bound
  let int64 bound = M.int64 (get_state ()) bound
  let full_int bound = M.full_int (get_state ()) bound
  let float bound = M.float (get_state ()) bound
  let float_strict bound = M.float_strict (get_state ()) bound
  let float_range ~min ~max = M.float_range (get_state ()) ~min ~max

  let float_range_strict ~min ~max =
    M.float_range_strict (get_state ()) ~min ~max

  let int_in_range ~min ~max = M.int_in_range (get_state ()) ~min ~max
  let int32_in_range ~min ~max = M.int32_in_range (get_state ()) ~min ~max
  let int64_in_range ~min ~max = M.int64_in_range (get_state ()) ~min ~max
  let bool () = M.bool (get_state ())
  let nativebits () = M.nativebits (get_state ())
  let nativeint bound = M.nativeint (get_state ()) bound

  let nativeint_in_range ~min ~max =
    M.nativeint_in_range (get_state ()) ~min ~max

  let get_insecure_bytes len = M.get_insecure_bytes (get_state ()) len
  let fill_insecure_bytes bytes n = M.fill_insecure_bytes (get_state ()) bytes n
  let fill_insecure_bigarray ba = M.fill_insecure_bigarray (get_state ()) ba
  let fill_float32_bigarray ba = M.fill_float32_bigarray (get_state ()) ba
  let fill_float64_bigarray ba = M.fill_float64_bigarray (get_state ()) ba

  let fill_float32_range_bigarray ba ~min ~max =
    M.fill_float32_range_bigarray (get_state ()) ba ~min ~max

  let fill_float64_range_bigarray ba ~min ~max =
    M.fill_float64_range_bigarray (get_state ()) ba ~min ~max

  let split () = M.split (get_state ())
  let for_testing_dls_variant () = Dls_compat.variant_name
  let for_testing_spawn_and_join f = Dls_compat.for_testing_spawn_and_join f
  let choice arr = M.choice (get_state ()) arr
  let choice_opt arr = M.choice_opt (get_state ()) arr
  let shuffle arr = M.shuffle (get_state ()) arr
  let shuffled arr = M.shuffled (get_state ()) arr
  let shuffled_list l = M.shuffled_list (get_state ()) l
  let permutation len = M.permutation (get_state ()) len
  let reservoir_sample arr k = M.reservoir_sample (get_state ()) arr k
  let weighted_bool p = M.weighted_bool (get_state ()) p
  let weighted_choice_linear_float arr =
    M.weighted_choice_linear_float (get_state ()) arr
  let weighted_table_float arr = M.weighted_table_float arr
  let weighted_choice_float tbl = M.weighted_choice_float (get_state ()) tbl
  let weighted_choice_linear_int arr =
    M.weighted_choice_linear_int (get_state ()) arr
  let weighted_table_int arr = M.weighted_table_int arr
  let weighted_choice_int tbl = M.weighted_choice_int (get_state ()) tbl
end
