(* Turns a saved AFL crash/queue file (or stdin) back into the harness call
   it exercised. Usage:
     decode.exe findings/default/crashes/id:000000,...
     decode.exe findings/default/crashes/id:*
     decode.exe < some_file *)

let decode_channel name ic =
  let data = Fuzz_input.read_all_bytes ic in
  match Fuzz_input.parse data with
  | None -> Printf.printf "%s: empty input\n" name
  | Some case -> Printf.printf "%s: %s\n" name (Fuzz_input.to_string case)

let () =
  match Array.to_list Sys.argv with
  | [ _ ] -> decode_channel "<stdin>" stdin
  | _ :: paths ->
      List.iter
        (fun path ->
          let ic = open_in_bin path in
          decode_channel path ic;
          close_in ic)
        paths
  | [] -> ()
