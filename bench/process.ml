open Base

type result = { exit_code : int; stdout : string; stderr : string }

let buffer_size = 4096

(* Unix.open_process can't give you stdout and stderr separately without
   deadlocking on a full pipe, so this spawns directly and multiplexes
   both fds with select. *)
let run cmd args : (result, string) Result.t =
  let args_array = Array.of_list (cmd :: args) in
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let stderr_r, stderr_w = Unix.pipe ~cloexec:true () in
  match
    try Ok (Unix.create_process cmd args_array Unix.stdin stdout_w stderr_w)
    with Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)
  with
  | Error e ->
      List.iter [ stdout_r; stdout_w; stderr_r; stderr_w ] ~f:Unix.close;
      Error e
  | Ok pid -> (
      Unix.close stdout_w;
      Unix.close stderr_w;
      let stdout_buf = Buffer.create buffer_size in
      let stderr_buf = Buffer.create buffer_size in
      let tmp = Bytes.create buffer_size in
      let stdout_open = ref true and stderr_open = ref true in
      let read_into fd buf is_open =
        match Unix.read fd tmp 0 buffer_size with
        | 0 ->
            Unix.close fd;
            is_open := false
        | n -> Buffer.add_subbytes buf tmp ~pos:0 ~len:n
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
      in
      while !stdout_open || !stderr_open do
        let fds =
          (if !stdout_open then [ stdout_r ] else [])
          @ if !stderr_open then [ stderr_r ] else []
        in
        let ready, _, _ = Unix.select fds [] [] (-1.) in
        List.iter ready ~f:(fun fd ->
            if Poly.equal fd stdout_r then read_into fd stdout_buf stdout_open
            else read_into fd stderr_buf stderr_open)
      done;
      match snd (Unix.waitpid [] pid) with
      | Unix.WEXITED exit_code ->
          Ok
            {
              exit_code;
              stdout = Buffer.contents stdout_buf;
              stderr = Buffer.contents stderr_buf;
            }
      | Unix.WSIGNALED signal ->
          Error (Printf.sprintf "%s killed by signal %d" cmd signal)
      | Unix.WSTOPPED signal ->
          Error (Printf.sprintf "%s stopped by signal %d" cmd signal))
