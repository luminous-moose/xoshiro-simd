open Base
include Base.List

let to_string ~f list =
  map ~f list |> String.concat ~sep:"; " |> Printf.sprintf "[%s]"

let collate list ~size =
  let rec loop outacc itemsize itemacc = function
    | [] -> rev (rev itemacc :: outacc)
    | item :: tl as l ->
        if itemsize = size then
          let group = rev itemacc in
          loop (group :: outacc) 0 [] l
        else loop outacc (itemsize + 1) (item :: itemacc) tl
  in
  loop [] 0 [] list
