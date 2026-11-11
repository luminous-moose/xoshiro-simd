(** Spawn a subprocess and capture its stdout and stderr separately. Shared by
    the [run_<arch>_tests.ml] dispatchers. *)

type result = { exit_code : int; stdout : string; stderr : string }

(** [run cmd args] runs [cmd] (looked up in [PATH]) with [args], with stdin
    inherited. [Error msg] if it could not be spawned or was killed or stopped
    by a signal; a nonzero exit is still [Ok]. *)
val run : string -> string list -> (result, string) Result.t
