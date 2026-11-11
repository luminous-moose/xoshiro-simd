include Rand_default

let available_backends = Rand_core.available_backends
let set_backend_limit = Rand_core.set_backend_limit

module State = struct
  include Rand_default.State

  let recover_seed_bytes = Rand_core.recover_seed_bytes

  let for_testing_int_lemire_c = int_lemire_c

  type raw_dump = Rand_core.Raw_dump.t = {
    s : int64 array;
    bitstream : int32 array;
    floatstream : floatarray;
    int_idx : int;
    float_idx : int;
  }

  let for_testing_raw_dump = Rand_core.for_testing_raw_dump
end

let for_testing_lemire_reduce32_wide =
  Rand_core.for_testing_lemire_reduce32_wide

let for_testing_mix_entropy_inverse = Rand_core.for_testing_mix_entropy_inverse

let self_init = init
