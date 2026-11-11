module Raw_dump : sig
  type t = {
    s : int64 array;
    bitstream : int32 array;
    floatstream : floatarray;
    int_idx : int;
    float_idx : int;
  }
end

type state

include Rand_core_intf.S with type state := state

val set_backend_limit : backend -> unit

val for_testing_raw_dump : state -> Raw_dump.t

val for_testing_lemire_reduce32_wide : int -> int32 -> int option

val for_testing_mix_entropy_inverse : int64 array -> int64 array

val recover_seed_bytes : state -> string
