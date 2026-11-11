(* This interface has different implementations for OCaml 4 and 5 and is backed
   by Domain.DLS on OCaml 5 and a single global on OCaml 4. With this interface
   we can use domains on OCaml 5 and have the code compile on OCaml 4.
*)

type 'a key

val new_key : ?split_from_parent:('a -> 'a) -> (unit -> 'a) -> 'a key

val get : 'a key -> 'a
val set : 'a key -> 'a -> unit

(* This is either "ocaml4" or "ocaml5", so a test can check the dune logic that
   compiles the right implementation for each compiler version. *)
val variant_name : string

(* for_testing_spawn_and_join f, on OCaml 4, does nothing and returns None, and
   on OCaml 5, it runs f to completion in a new domain and returns Some result.
   This lets us test the domain-crossing behavior without referencing Domain
   directly. *)
val for_testing_spawn_and_join : (unit -> 'a) -> 'a option
