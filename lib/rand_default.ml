module State = Rand_impl.Make (Rand_core)

(* The DLS key is created here, at this module's true top level, so it's read as
   a global. This improved performance on some CPUs. *)
module Dls_key = struct
  let key = Dls_compat.new_key ~split_from_parent:State.split State.init
  let get_state () = Dls_compat.get key
  let set_state s = Dls_compat.set key s
end

include Rand_impl.MakeStateless (State) (Dls_key)
