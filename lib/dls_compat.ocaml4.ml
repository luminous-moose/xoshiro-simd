type 'a key = { mutable v : 'a }

let new_key ?split_from_parent f =
  ignore split_from_parent;
  { v = f () }

let[@inline] get k = k.v

let[@inline] set k v = k.v <- v
let variant_name = "ocaml4"
let for_testing_spawn_and_join (_ : unit -> 'a) = None
