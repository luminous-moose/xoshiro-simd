(* OCaml 5 only (see test/dune). Every spawned domain gets its own stream from
   the module-level generator, whatever the spawn order or nesting, and the
   whole tree is reproducible from the initial domain's seed (the README:
   "every domain's stream is reproducible, as long as each domain spawns its
   children in the same order").

   test_domain.ml checks one spawn against a split of a reference state. This
   one builds a tree: siblings spawned back to back, children spawned before and
   after their parent draws, a domain that spawns without ever drawing, and
   grandchildren. Siblings run concurrently, since a parent spawns all of them
   before joining any; that doesn't affect the output, because each domain's
   state is split from its parent synchronously at spawn time. Every draw, the
   initial domain's included, must be distinct, and a second run from the same
   seed must give every domain the same draws. *)

module X = Xoshiro_simd

type node = { name : string; actions : action list }
and action = Draw | Spawn of node

(* Draws are labelled by domain name and position, e.g. "a2.1". *)
let rec run node =
  let draws = ref [] and children = ref [] and n = ref 0 in
  List.iter
    (function
      | Draw ->
          incr n;
          let label = Printf.sprintf "%s.%d" node.name !n in
          draws := (label, X.get_insecure_bytes 32) :: !draws
      | Spawn child ->
          children := Domain.spawn (fun () -> run child) :: !children)
    node.actions;
  List.rev !draws @ List.concat_map Domain.join (List.rev !children)

let leaf name = { name; actions = [ Draw ] }

let tree =
  {
    name = "root";
    actions =
      [
        Draw;
        Spawn
          {
            name = "a";
            actions =
              [
                Spawn (leaf "a1");
                Spawn
                  { name = "a2"; actions = [ Spawn (leaf "a2x"); Draw; Draw ] };
                Draw;
              ];
          };
        Spawn (leaf "b");
        Draw;
        Spawn
          {
            name = "c";
            actions =
              [
                Spawn
                  {
                    name = "c1";
                    actions = [ Spawn (leaf "c1x"); Spawn (leaf "c1y") ];
                  };
              ];
          };
        Spawn (leaf "d");
        Draw;
      ];
  }

let fail fmt =
  Printf.ksprintf
    (fun s ->
      prerr_endline ("FAIL: " ^ s);
      exit 1)
    fmt

let run_from_seed () =
  X.init_deterministic 2026L;
  run tree

let () =
  let first = run_from_seed () in
  let rec check_distinct = function
    | [] -> ()
    | (label, bytes) :: rest ->
        (match List.find_opt (fun (_, b) -> Bytes.equal b bytes) rest with
        | Some (other, _) -> fail "%s and %s drew the same bytes" label other
        | None -> ());
        check_distinct rest
  in
  check_distinct first;
  let second = run_from_seed () in
  List.iter2
    (fun (label, a) (label', b) ->
      assert (String.equal label label');
      if not (Bytes.equal a b) then
        fail "%s drew different bytes on a second run from the same seed" label)
    first second
