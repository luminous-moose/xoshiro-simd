type 'a key = 'a Domain.DLS.key

let new_key = Domain.DLS.new_key
let get = Domain.DLS.get
let set = Domain.DLS.set
let variant_name = "ocaml5"
let for_testing_spawn_and_join f = Some (Domain.join (Domain.spawn f))
