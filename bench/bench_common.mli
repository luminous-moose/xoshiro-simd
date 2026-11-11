type test = { name : string; available : unit -> bool; test : unit -> unit }

val run_and_print : string -> test list -> unit

val stats : float list -> float * float * float
