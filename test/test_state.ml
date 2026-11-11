open Base
open! Stdio
module Xs_S = Xoshiro_simd.State
module Xs = Xoshiro_simd

let hex2byte h = Hex.to_bytes (`Hex h)
let seed = 42L

(* The README says a State.t can't be marshalled: the generator lives in a C
   custom block with no serializer. *)
let%test "a State.t cannot be marshalled" =
  match Stdlib.Marshal.to_string (Xs_S.init_deterministic seed) [] with
  | _ -> false
  | exception Invalid_argument _ -> true

(* --- reseed --- *)

let%test_unit "test reseed" =
  let state = Xs_S.init_deterministic seed in
  let reference = Xs_S.init_deterministic seed in
  let cprev = Xs_S.get_insecure_bytes state 16 in
  let cprev_ref = Xs_S.get_insecure_bytes reference 16 in
  assert (Bytes.equal cprev cprev_ref);
  (* What [reference] produces next is exactly what [state] would produce
     next too, if reseed left its seed untouched. *)
  let expected_without_reseed = Xs_S.get_insecure_bytes reference 16 in
  Xs_S.reseed state;
  let cnow = Xs_S.get_insecure_bytes state 16 in
  assert (not (Bytes.equal cnow expected_without_reseed))

let%test_unit
    "reseed forces a cache refill instead of replaying the buffered block" =
  (* The "test reseed" case above only checks that reseeding makes the next
     output differ from what continuing the old seed would have produced --
     that can't distinguish "truly reseeded" from "buggy: idx got reset to 0
     instead of being marked exhausted", since replaying the start of the
     still-buffered pre-reseed block also looks different from the old
     continuation. This test targets that gap directly: one draw fills the
     cache and consumes position 0; reseed; draw again. If reseed
     incorrectly resets idx to 0 rather than to the cache length, the second
     draw re-reads that same already-consumed position 0 and returns the
     exact same word as the first draw, because reseed never touched the
     buffered bytes themselves -- only a real refill would. *)
  let state = Xs_S.init_deterministic seed in
  let before = Xs_S.bits32 state in
  Xs_S.reseed state;
  let after = Xs_S.bits32 state in
  assert (not (Int32.equal before after))

let%test_unit
    "reseed forces a float cache refill instead of replaying the buffered block"
    =
  (* Same reasoning as the int-side test above, but for the float cache.
     reseed has two independent fields to invalidate (int_idx and float_idx) --
     bits32 above only exercises int_idx, so a fix that updated int_idx but
     missed float_idx (a real risk, since they're separate fields threaded
     through separately) would pass that test while still silently
     replaying pre-reseed floats here. *)
  let state = Xs_S.init_deterministic seed in
  let before = Xs_S.uniform_float state in
  Xs_S.reseed state;
  let after = Xs_S.uniform_float state in
  assert (not (Float.equal before after))

(* --- module-level reseed --- *)

let%test_unit "module-level reseed changes domain state" =
  Xs.init_deterministic seed;
  let before = Xs.get_insecure_bytes 16 in
  Xs.init_deterministic seed;
  Xs.reseed ();
  let after = Xs.get_insecure_bytes 16 in
  assert (not (Bytes.equal before after))

(* --- split --- *)

let%test_unit "split advances the parent" =
  let state = Xs_S.init_deterministic seed in
  let reference = Xs_S.init_deterministic seed in
  let cprev = Xs_S.get_insecure_bytes state 16 in
  let cprev_ref = Xs_S.get_insecure_bytes reference 16 in
  assert (Bytes.equal cprev cprev_ref);
  (* What [reference] produces next is exactly what [state] would produce
     next too, if split drew nothing from it. *)
  let expected_without_split = Xs_S.get_insecure_bytes reference 16 in
  ignore (Xs_S.split state);
  let cnow = Xs_S.get_insecure_bytes state 16 in
  assert (not (Bytes.equal cnow expected_without_split))

let%test_unit "two splits from the same parent produce different children" =
  let state = Xs_S.init_deterministic seed in
  let child1 = Xs_S.split state in
  let child2 = Xs_S.split state in
  let out1 = Xs_S.get_insecure_bytes child1 16 in
  let out2 = Xs_S.get_insecure_bytes child2 16 in
  assert (not (Bytes.equal out1 out2))

let%test_unit
    "split does not reproduce a previously-split state under interleaved draws"
    =
  (* The scenario that ruled out jump/long_jump for on-demand splitting: s1
     -> split -> s2; draw N from s2 and N from s1; s1 -> split -> s3. jump is
     an exact power of the same state-transition function as ordinary
     advancement, so jump and advancement commute -- under a jump-based
     scheme, s2-after-N-draws and s3 would be exactly the same state. split
     is built on mix_entropy instead, which isn't in the semigroup generated
     by the transition matrix, so this can't not happen here. *)
  let s1 = Xs_S.init_deterministic seed in
  let s2 = Xs_S.split s1 in
  for _ = 1 to 7 do
    ignore (Xs_S.bits64 s2);
    ignore (Xs_S.bits64 s1)
  done;
  let s3 = Xs_S.split s1 in
  let dump2 = Xs_S.for_testing_raw_dump s2 in
  let dump3 = Xs_S.for_testing_raw_dump s3 in
  assert (not (Array.equal Int64.equal dump2.s dump3.s))

let%test_unit "split is deterministic given identical prior draws" =
  let run () =
    let state = Xs_S.init_deterministic seed in
    ignore (Xs_S.bits64 state);
    Xs_S.split state
  in
  let d1 = Xs_S.for_testing_raw_dump (run ()) in
  let d2 = Xs_S.for_testing_raw_dump (run ()) in
  assert (Array.equal Int64.equal d1.s d2.s)

let%test_unit
    "split state matches manually seeding from the parent's own bits64 draws" =
  (* Confirms split has no hidden extra logic beyond drawing 32 bytes of the
     parent's own output and feeding them to init_string. The reference is
     built independently of get_insecure_bytes: four bits64 draws from an
     identically-seeded state, packed with Bytes.set_int64_le, which uses a
     fixed byte order regardless of host -- so the two constructions should
     land on the exact same state on every architecture, not just the one
     this is run on. *)
  let state = Xs_S.init_deterministic seed in
  let reference = Xs_S.init_deterministic seed in
  let child = Xs_S.split state in
  let manual_seed = Bytes.create 32 in
  for i = 0 to 3 do
    (* Stdlib.Bytes, not Base's: Base's Bytes doesn't expose set_int64_le. *)
    Stdlib.Bytes.set_int64_le manual_seed (i * 8) (Xs_S.bits64 reference)
  done;
  let manual_child =
    Xs_S.init_string ~backend:(Xs_S.state_backend state)
      (Bytes.to_string manual_seed)
  in
  let d1 = Xs_S.for_testing_raw_dump child in
  let d2 = Xs_S.for_testing_raw_dump manual_child in
  assert (Array.equal Int64.equal d1.s d2.s)

(* split, human-readable: eyeball a parent's state next to a child derived
   from it. *)

let print_lanes label (dump : Xs_S.raw_dump) =
  printf "%s:\n" label;
  for lane = 0 to 3 do
    printf "  lane %d:\n" lane;
    for w = 0 to 3 do
      printf "    0x%016Lx\n" dump.s.((w * 4) + lane)
    done
  done

let%expect_test "split produces a normal-looking 4-lane state" =
  let parent = Xs_S.init_deterministic ~backend:Xs.Plain seed in
  print_lanes "parent" (Xs_S.for_testing_raw_dump parent);
  [%expect
    {|
    parent:
      lane 0:
        0xbdd732262feb6e95
        0x28efe333b266f103
        0x47526757130f9f52
        0x581ce1ff0e4ae394
      lane 1:
        0x81746704fde896b5
        0x645e944932dae0ae
        0xf4776829231c282c
        0x2393f9798732dba1
      lane 2:
        0xac1efbd1c1f9edb7
        0xe3ef388185a3e19d
        0x8928517ea1524add
        0x5f0e2c50350e976f
      lane 3:
        0xeaf43ca1fd59b7b5
        0x1e2dc15cc4ad4ba0
        0x8e44bc177ee1d7e6
        0x9cf11261ebb86aac
    |}];
  let child = Xs_S.split parent in
  print_lanes "child" (Xs_S.for_testing_raw_dump child);
  [%expect
    {|
    child:
      lane 0:
        0x4b98153a9c549095
        0x644a255786bb723b
        0x19932569f7add375
        0x19cc73cc04e38fba
      lane 1:
        0x11e6a5c4b8fca9f0
        0x5c83b7c5a9975e00
        0x16d6e64fd312b097
        0x78aea601c9ffb080
      lane 2:
        0xd9683e080e00df02
        0xe261229a5666e9cf
        0x62e47699ea6d97e6
        0x8c7cb0afd02cbc23
      lane 3:
        0x3c755160bf3acf45
        0x62698de3a8f0936a
        0xab31bb23c5d71625
        0x12789220700c5108
    |}]

(* --- recover_seed_bytes --- *)

let bits64_seq state n = Array.init n ~f:(fun _ -> Xs_S.bits64 state)

let%test_unit
    "recover_seed_bytes lets init_string reconstruct identical future output" =
  let state = Xs_S.init_deterministic seed in
  let recovered = Xs_S.recover_seed_bytes state in
  let replay = Xs_S.init_string ~backend:(Xs_S.state_backend state) recovered in
  let from_state = bits64_seq state 32 in
  let from_replay = bits64_seq replay 32 in
  assert (Array.equal Int64.equal from_state from_replay)

let%test_unit "recover_seed_bytes works on a state built by init_string too" =
  let state = Xs_S.init_string "a somewhat longer human-readable seed string" in
  let recovered = Xs_S.recover_seed_bytes state in
  let replay = Xs_S.init_string ~backend:(Xs_S.state_backend state) recovered in
  let from_state = bits64_seq state 32 in
  let from_replay = bits64_seq replay 32 in
  assert (Array.equal Int64.equal from_state from_replay)

let%expect_test
    "recover_seed_bytes's recovered bytes need not equal the original seed \
     material" =
  (* mix_entropy at length 4 is a bijection on the full 256-bit space, so
     recover_seed_bytes always finds *a* 4-word preimage of the current
     state -- but for a seed string that isn't itself exactly 32 bytes (this
     one is 46), there's no reason for that preimage to be the original seed
     string's own bytes, only for it to reproduce the same state. *)
  let original = "a somewhat longer human-readable seed string" in
  let state = Xs_S.init_string original in
  let recovered = Xs_S.recover_seed_bytes state in
  assert (String.length recovered = 32);
  assert (not (String.equal recovered original));
  (* recovered is 32 arbitrary bytes, not text, so most of it isn't
     printable -- escape it to see what it actually contains. *)
  printf "%s\n" (String.escaped recovered);
  [%expect
    {| XB\195\t\138!\199\015N\198\192\231\199\226>V\142\0313\144\2427\216%\250\180\024\201C\005M\161 |}]

let%test_unit "recover_seed_bytes works on a freshly-split child" =
  let parent = Xs_S.init_deterministic seed in
  let child = Xs_S.split parent in
  let recovered = Xs_S.recover_seed_bytes child in
  let replay = Xs_S.init_string ~backend:(Xs_S.state_backend child) recovered in
  let from_child = bits64_seq child 32 in
  let from_replay = bits64_seq replay 32 in
  assert (Array.equal Int64.equal from_child from_replay)

let%test_unit "recover_seed_bytes is repeatable on a still-fresh state" =
  (* Reading recover_seed_bytes must not itself count as a draw -- calling it
     twice in a row on the same untouched state should give the same answer
     both times, and the second call must not raise. *)
  let state = Xs_S.init_deterministic seed in
  let first = Xs_S.recover_seed_bytes state in
  let second = Xs_S.recover_seed_bytes state in
  assert (String.equal first second)

(* Bulk fills (bytes and floats) advance the lane state directly and never
   touch the int/float caches, so a state whose caches were never filled is
   still fully described by its lane state after any number of them, and
   recover_seed_bytes must return bytes that reproduce it. What it recovers is
   the state from this point forward, not the seed the state started from. *)
let check_recovered_state_replays state =
  let recovered = Xs_S.recover_seed_bytes state in
  let replay = Xs_S.init_string ~backend:(Xs_S.state_backend state) recovered in
  assert (Array.equal Int64.equal (bits64_seq state 32) (bits64_seq replay 32));
  let floats s = Array.init 8 ~f:(fun _ -> Xs_S.float s 1.0) in
  assert (Array.equal Float.equal (floats state) (floats replay));
  assert (
    Bytes.equal
      (Xs_S.get_insecure_bytes state 100)
      (Xs_S.get_insecure_bytes replay 100))

let%test_unit "recover_seed_bytes replays after get_insecure_bytes fills" =
  List.iter [ 1; 32; 33; 100 ] ~f:(fun n ->
      let state = Xs_S.init_deterministic seed in
      ignore (Xs_S.get_insecure_bytes state n);
      check_recovered_state_replays state)

let%test_unit "recover_seed_bytes replays after fill_insecure_bigarray" =
  List.iter (Xs.available_backends ()) ~f:(fun backend ->
      let state = Xs_S.init_deterministic ~backend seed in
      Xs_S.fill_insecure_bigarray state
        (Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 37);
      check_recovered_state_replays state)

let%test_unit "recover_seed_bytes replays after float bulk fills" =
  List.iter (Xs.available_backends ()) ~f:(fun backend ->
      let state = Xs_S.init_deterministic ~backend seed in
      Xs_S.fill_float64_bigarray state
        (Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout 37);
      Xs_S.fill_float32_bigarray state
        (Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 37);
      check_recovered_state_replays state)

let%test_unit "recover_seed_bytes replays on every available backend" =
  List.iter (Xs.available_backends ()) ~f:(fun backend ->
      let state = Xs_S.init_deterministic ~backend seed in
      ignore (Xs_S.get_insecure_bytes state 45);
      check_recovered_state_replays state)

let%test_unit "recover_seed_bytes raises when a bulk fill follows a draw" =
  (* The draw leaves unread words in the cache, which the fill can't account
     for, so the state is no longer described by the lane state alone. *)
  let state = Xs_S.init_deterministic seed in
  ignore (Xs_S.bits64 state);
  ignore (Xs_S.get_insecure_bytes state 32);
  match Xs_S.recover_seed_bytes state with
  | _ -> assert false
  | exception Invalid_argument _ -> ()

let%test_unit "recover_seed_bytes raises once any output has been drawn" =
  let state = Xs_S.init_deterministic seed in
  ignore (Xs_S.bits64 state);
  match Xs_S.recover_seed_bytes state with
  | _ -> assert false
  | exception Invalid_argument _ -> ()

(* --- GC accounting of the C state block --- *)

(* Each state owns about 2 KiB of C memory. The custom block's GC accounting
   should treat that like any other 2 KiB allocation, not force a major
   collection every few hundred states regardless of heap size: with a large
   live heap, that made each split or init cost a sizable fraction of a
   millisecond. The baseline allocates the same amount of plain OCaml memory,
   so the test doesn't depend on the GC's pacing details. *)
let%test_unit "creating states does not force extra major collections" =
  let live = Array.init 2_000_000 ~f:(fun i -> Some i) in
  let count_majors f =
    Stdlib.Gc.full_major ();
    let before = (Stdlib.Gc.quick_stat ()).major_collections in
    for _ = 1 to 4000 do
      f ()
    done;
    (Stdlib.Gc.quick_stat ()).major_collections - before
  in
  let parent = Xs_S.init_deterministic seed in
  let state_majors =
    count_majors (fun () ->
        ignore (Sys.opaque_identity (Xs_S.split parent) : Xs_S.t))
  in
  let baseline_majors =
    count_majors (fun () ->
        ignore (Sys.opaque_identity (Bytes.create 2400) : Bytes.t))
  in
  ignore (Sys.opaque_identity live : int option array);
  if state_majors > baseline_majors + 3 then
    Printf.failwithf "4000 splits: %d major collections, baseline %d"
      state_majors baseline_majors ()
