(* OCaml-driven battery harness: exercises the real public Xoshiro_simd
   API directly (State.bits64, State.uniform_float,
   State.fill_float64_bigarray, State.fill_float32_bigarray), not internal C
   entry points -- testing the actual public surface is a strictly stronger
   guarantee of library quality than calling internal implementation
   directly, and it's the surface an actual caller of this library gets.

   Each refill below is a single call into the real public API per batch
   (a plain OCaml loop for the scalar-draw streams, one call for the
   bulk-fill streams), then one call into C to hand the whole batch to the
   selected battery -- not a call into C per individual random value. *)

open Xoshiro_simd

external run_testu01_int64 :
  (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  (unit -> unit) ->
  string ->
  string ->
  bool ->
  unit = "xoshiro_ocaml_run_testu01_int64"

external run_testu01_float64 :
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  (unit -> unit) ->
  string ->
  string ->
  unit = "xoshiro_ocaml_run_testu01_float64"

external run_testu01_float32 :
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  (unit -> unit) ->
  string ->
  string ->
  bool ->
  unit = "xoshiro_ocaml_run_testu01_float32"

(* Two of these three now take 6 arguments (past OCaml's 5-argument native-call
   limit), so they need the bytecode-stub name alongside the native one;
   see ocaml_battery_stubs.cpp's *_bytecode wrappers. *)

external run_practrand_int64 :
  (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  (unit -> unit) ->
  int ->
  float ->
  string ->
  bool ->
  unit
  = "xoshiro_ocaml_run_practrand_int64_bytecode"
    "xoshiro_ocaml_run_practrand_int64"

external run_practrand_float64 :
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  (unit -> unit) ->
  int ->
  float ->
  string ->
  unit = "xoshiro_ocaml_run_practrand_float64"

external run_practrand_float32 :
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  (unit -> unit) ->
  int ->
  float ->
  string ->
  bool ->
  unit
  = "xoshiro_ocaml_run_practrand_float32_bytecode"
    "xoshiro_ocaml_run_practrand_float32"

type stream = Int64_stream | Float53_scalar | Float53_bulk | Float24_bulk

let stream_name = function
  | Int64_stream -> "xoshiro256pp_int64"
  | Float53_scalar -> "xoshiro256pp_float_53bit_scalar"
  | Float53_bulk -> "xoshiro256pp_float_53bit_bulk"
  | Float24_bulk -> "xoshiro256pp_float_24bit_bulk"

let is_float = function Int64_stream -> false | _ -> true

let stdlib_state =
  Random.init 42;
  Random.get_state ()

let compute_mask numbits =
  let bit = 1 lsl numbits in
  bit - 1

(* Deliberately biased draw, to probe how weak a defect PractRand/TestU01
   can actually detect. `mask` (see compute_mask) fires the bias with
   probability 1/2^N for whatever N was passed to --bias, decided by an
   independent roll rather than the draw's own bits -- deciding from the
   draw's own bits would make "bit k is 1" correlate with "these other bits
   are 0," a structural defect distinct from (and easier for some tests to
   spot via unrelated means than) the intended plain single-bit bias. When
   the roll fires, `randbit` selects between always biasing the same fixed
   bit (bit 2) or a freshly chosen random bit each time. *)
let biased_bits64 state ~mask ~randbit =
  let v = State.bits64 state in
  let roll = Random.State.bits stdlib_state in
  if roll land mask = 0 then
    let bit =
      if randbit then
        let shift = Random.State.int stdlib_state 64 in
        Int64.shift_left 1L shift
      else 4L
    in
    Int64.logor v bit
  else v

type battery = Small | Crush | Big | Practrand of int (* tlmax in bytes *)

(* Parses byte-count specs like "128M", "4T" (powers of 1024) for --tlmax. *)
let parse_size_spec s =
  let n = String.length s in
  if n = 0 then invalid_arg "empty size spec"
  else
    let last = s.[n - 1] in
    let mult, digits =
      match Char.uppercase_ascii last with
      | 'K' -> (1024, String.sub s 0 (n - 1))
      | 'M' -> (1024 * 1024, String.sub s 0 (n - 1))
      | 'G' -> (1024 * 1024 * 1024, String.sub s 0 (n - 1))
      | 'T' -> (1024 * 1024 * 1024 * 1024, String.sub s 0 (n - 1))
      | _ -> (1, s)
    in
    mult * int_of_string digits

(* ------------------------------------------------------------------ *)
(* Harness logic: everything that runs once arguments are parsed. Takes
   plain values in, never touches Cmdliner. *)
(* ------------------------------------------------------------------ *)

let validate_batch batch =
  if batch mod 64 <> 0 then (
    Printf.eprintf "--batch must be a multiple of 64 (got %d)\n" batch;
    exit 1)

let validate_seed_choice seed seed_string seed_hex =
  match (seed, seed_string, seed_hex) with
  | Some _, None, None
  | None, Some _, None
  | None, None, Some _
  | None, None, None ->
      ()
  | _ ->
      Printf.eprintf
        "--seed, --seed-string, and --seed-hex are mutually exclusive\n";
      exit 1

let validate_bias stream bias_zero_bits =
  match bias_zero_bits with
  | Some n when n > 30 ->
      Printf.eprintf
        "--bias must be at most 30 (got %d) -- the trigger roll only has 30 \
         random bits (Random.State.bits), so larger N wouldn't actually be \
         rarer, just silently capped at the N=30 rate\n"
        n;
      exit 1
  | Some _ when is_float stream ->
      Printf.eprintf
        "--bias only applies to --stream int64, not %s -- the float streams' \
         refill closures (see run_testu01/run_practrand) call \
         State.uniform_*/fill_float*_bigarray directly and never route through \
         make_int64_refill, so a float run would silently test unbiased output \
         instead of the biased draw you asked for\n"
        (stream_name stream);
      exit 1
  | _ -> ()

(* --full's reversal has to be per-draw-bit to test what motivates it
   (TestU01/PractRand's bit-position sensitivity) -- not a reversal of
   bitbin's packed words, which splice several draws together
   (xoshiro_bitbin_53bit/_24bit, see ocaml_bridge.c). 53 is prime, so its
   packing offset (53*n mod 64) cycles through all 64 positions: every
   mantissa bit lands at every output position equally, forward and
   reversed are statistically identical, and there's nothing --full could
   catch there -- so float64 is excluded below. 24 doesn't have that property
   (gcd(24,64)=8, only 8 positions visited), so Float24_bulk instead reverses
   each draw's own mantissa before bitbinning (xoshiro_reverse_float24_values)
   and is exempted below. *)
let validate_full stream full =
  match stream with
  | (Float53_scalar | Float53_bulk) when full ->
      Printf.eprintf
        "--full only applies to --stream int64 and float24bulk, not %s -- its \
         output words are several draws' worth of bits already packed together \
         (see xoshiro_bitbin_53bit), so reversing those packed words isn't the \
         per-draw bit reversal --full is supposed to test; it would silently \
         run a much weaker check than you asked for\n"
        (stream_name stream);
      exit 1
  | _ -> ()

let make_int64_refill ~batch ~bias_zero_bits ~bias_random_bit ba state =
  match bias_zero_bits with
  | Some n ->
      let mask = compute_mask n in
      let get_bits64 = biased_bits64 ~mask ~randbit:bias_random_bit in
      fun () ->
        for i = 0 to batch - 1 do
          Bigarray.Array1.set ba i (get_bits64 state)
        done
  | None -> fun () -> Xoshiro_simd.State.fill_insecure_bigarray state ba

(* smallpract/mediumpract/bigpract: fixed-size PractRand runs calibrated
   to take about as long as SmallCrush/Crush/BigCrush do, at this
   machine's measured ~164 MB/s PractRand rate (int64 stream). small and
   big are matched to known reference times (~5s, ~2.5h); medium is
   matched to an actually-measured Crush run (16m58s). big is
   deliberately rounded down to a clean 1T rather than chasing 2.5h
   precisely: BigCrush itself only ever consumes ~2^38 numbers (~274GB),
   so 1T is already ~3.65x more data than BigCrush's own budget even
   though its wall-clock (~1.9h) undershoots the 2.5h target -- not short
   on rigor, just faster per byte than TestU01's suite. --tlmax is
   ignored for these three; they're fully self-contained. *)
let to_battery ~tlmax_spec battery =
  match battery with
  | `Small -> Small
  | `Crush -> Crush
  | `Big -> Big
  | `Practrand -> Practrand (parse_size_spec tlmax_spec)
  | `SmallPract -> Practrand (parse_size_spec "800M")
  | `MediumPract -> Practrand (parse_size_spec "155G")
  | `BigPract -> Practrand (parse_size_spec "1T")

(* Returns the state to draw from, plus a closure that reconstructs a fresh
   state starting from the identical seed material -- used under --full to
   run the bit-reversed pass against the same underlying stream as the
   forward pass, rather than a continuation of wherever the forward pass's
   draws left it. *)
let build_state ~seed ~seed_string ~seed_hex =
  match (seed, seed_string, seed_hex) with
  | Some s, None, None ->
      (State.init_deterministic s, fun () -> State.init_deterministic s)
  | None, Some s, None -> (State.init_string s, fun () -> State.init_string s)
  | None, None, Some h ->
      let bytes = Hex.to_string (`Hex h) in
      (State.init_string bytes, fun () -> State.init_string bytes)
  | None, None, None ->
      let state = State.init () in
      (* Must run before any draw from [state] -- see recover_seed_bytes's
         doc comment. *)
      let recovered = State.recover_seed_bytes state in
      Printf.printf
        "OS-entropy seed for this run (replay with --seed-hex %s)\n%!"
        (Hex.show (Hex.of_string recovered));
      (state, fun () -> State.init_string recovered)
  | _ -> assert false (* ruled out by validate_seed_choice above *)

(* [name] gets a "_bitrev" suffix under [~reverse], so battery/PractRand
   output (and any saved results) unambiguously shows which orientation a
   run belongs to. *)
let reversed_name ~reverse name = if reverse then name ^ "_bitrev" else name

let run_testu01 ~batch ~state ~name ~int64_refill ~reverse stream battery_str =
  let name = reversed_name ~reverse name in
  match stream with
  | Int64_stream ->
      let ba = Bigarray.Array1.create Bigarray.int64 Bigarray.c_layout batch in
      let refill = int64_refill ba state in
      refill ();
      run_testu01_int64 ba refill battery_str name reverse
  | Float53_scalar ->
      let ba =
        Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout batch
      in
      let refill () =
        for i = 0 to batch - 1 do
          Bigarray.Array1.set ba i (State.uniform_float state)
        done
      in
      refill ();
      run_testu01_float64 ba refill battery_str name
  | Float53_bulk ->
      let ba =
        Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout batch
      in
      let refill () = State.fill_float64_bigarray state ba in
      refill ();
      run_testu01_float64 ba refill battery_str name
  | Float24_bulk ->
      let ba =
        Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout batch
      in
      let refill () = State.fill_float32_bigarray state ba in
      refill ();
      run_testu01_float32 ba refill battery_str name reverse

let run_practrand ~batch ~state ~name ~int64_refill ~threshold ~reverse stream
    tlmax =
  let name = reversed_name ~reverse name in
  match stream with
  | Int64_stream ->
      let ba = Bigarray.Array1.create Bigarray.int64 Bigarray.c_layout batch in
      let refill = int64_refill ba state in
      refill ();
      run_practrand_int64 ba refill tlmax threshold name reverse
  | Float53_scalar ->
      let ba =
        Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout batch
      in
      let refill () =
        for i = 0 to batch - 1 do
          Bigarray.Array1.set ba i (State.uniform_float state)
        done
      in
      refill ();
      run_practrand_float64 ba refill tlmax threshold name
  | Float53_bulk ->
      let ba =
        Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout batch
      in
      let refill () = State.fill_float64_bigarray state ba in
      refill ();
      run_practrand_float64 ba refill tlmax threshold name
  | Float24_bulk ->
      let ba =
        Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout batch
      in
      let refill () = State.fill_float32_bigarray state ba in
      refill ();
      run_practrand_float32 ba refill tlmax threshold name reverse

(* ------------------------------------------------------------------ *)
(* Command-line interface: argument definitions and main function.    *)
(* ------------------------------------------------------------------ *)

open Cmdliner

let stream_arg =
  let alts =
    [
      ("int64", Int64_stream);
      ("float53scalar", Float53_scalar);
      ("float53bulk", Float53_bulk);
      ("float24bulk", Float24_bulk);
    ]
  in
  Arg.(
    required
    & pos 0 (some (enum alts)) None
    & info [] ~docv:"STREAM"
        ~doc:("Which stream to test: " ^ Arg.doc_alts_enum alts ^ "."))

let battery_alts =
  [
    ("small", `Small);
    ("crush", `Crush);
    ("big", `Big);
    ("practrand", `Practrand);
    ("smallpract", `SmallPract);
    ("mediumpract", `MediumPract);
    ("bigpract", `BigPract);
  ]

let battery_arg =
  Arg.(
    required
    & pos 1 (some (enum battery_alts)) None
    & info [] ~docv:"BATTERY"
        ~doc:
          ("Which battery to run: "
          ^ Arg.doc_alts_enum battery_alts
          ^ ". smallpract/mediumpract/bigpract are fixed-size PractRand runs \
             (--tlmax is ignored) calibrated to take about as long as \
             SmallCrush/Crush/BigCrush do."))

let seed_arg =
  Arg.(
    value
    & opt (some int64) None
    & info [ "seed" ] ~docv:"SEED"
        ~doc:
          "Deterministic seed (via State.init_deterministic); default: OS \
           entropy. Mutually exclusive with --seed-string/--seed-hex.")

let seed_string_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "seed-string" ] ~docv:"SEED"
        ~doc:
          "Seed via State.init_string, taken verbatim as the seed material. \
           Mutually exclusive with --seed/--seed-hex.")

let seed_hex_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "seed-hex" ] ~docv:"HEX"
        ~doc:
          "Seed via State.init_string, decoded from hex first -- the form \
           printed by this harness's own OS-entropy-seed recovery notice, for \
           replaying a prior OS-entropy run exactly. Mutually exclusive with \
           --seed/--seed-string.")

let tlmax_arg =
  Arg.(
    value & opt string "128M"
    & info [ "tlmax" ] ~docv:"SIZE"
        ~doc:
          "PractRand size cutoff, e.g. 4T, 128M. Only used with the practrand \
           battery.")

let batch_arg =
  Arg.(
    value & opt int 65536
    & info [ "batch" ] ~docv:"N"
        ~doc:"Refill batch size (elements). Must be a multiple of 64.")

let threshold_arg =
  Arg.(
    value & opt float 0.01
    & info [ "threshold" ] ~docv:"ALPHA"
        ~doc:
          "Target family-wise false-positive rate for PractRand's outlier \
           pass: roughly, the chance that any single statistic flags purely by \
           chance across the whole run. The per-test p-value cutoff is derived \
           from this via Bonferroni correction, scaled by how many statistics \
           PractRand actually ran (that count grows with --tlmax, so a fixed \
           per-test cutoff wouldn't give a stable rate), floored at 1e-4 \
           (PractRand's own author's documented \"suspicious\" convention) so \
           it never becomes more conservative than that. Only used with the \
           practrand battery.")

let bias_arg =
  Arg.(
    value
    & opt (some int) None
    & info [ "bias" ] ~docv:"N"
        ~doc:
          "Deliberately bias the int64 stream, to probe how weak a defect \
           PractRand/TestU01 can actually detect: with probability 1/2^N \
           (power-of-two only, by construction), force one output bit to 1. \
           The trigger is an independent coin flip (Stdlib's Random, not the \
           draw's own bits), so a biased draw's other 63 bits stay exactly as \
           uncorrelated with the forced bit as an unbiased draw's would be. \
           Only applies to --stream int64 -- the float streams' refill \
           closures never route through the biased code path, so combining \
           --bias with a float --stream is rejected rather than silently \
           testing unbiased output. Omit for unbiased output. Maximum 30: the \
           trigger roll comes from Random.State.bits, which OCaml documents as \
           returning only 30 random bits, so mask bits beyond that position \
           are always already zero in the roll and stop adding any real \
           restriction -- larger N would silently stop getting rarer instead \
           of erroring, so it's rejected outright instead.")

let bias_random_bit_arg =
  Arg.(
    value & flag
    & info [ "bias-random-bit" ]
        ~doc:
          "With --bias, target a randomly chosen output bit each time a draw \
           is biased, instead of always the same fixed bit. Spreads the effect \
           across all 64 bit positions, which dilutes the per-bit-position \
           signal roughly 64x versus a fixed bit at the same trigger rate -- \
           much harder for per-position statistics to detect.")

let main stream battery seed seed_string seed_hex tlmax_spec batch threshold
    bias_zero_bits bias_random_bit full =
  validate_batch batch;
  validate_seed_choice seed seed_string seed_hex;
  validate_bias stream bias_zero_bits;
  validate_full stream full;
  let int64_refill =
    make_int64_refill ~batch ~bias_zero_bits ~bias_random_bit
  in
  let battery = to_battery ~tlmax_spec battery in
  let state, replay_state = build_state ~seed ~seed_string ~seed_hex in
  let name = stream_name stream in
  let run ~reverse state =
    match battery with
    | Small ->
        run_testu01 ~batch ~state ~name ~int64_refill ~reverse stream "small"
    | Crush ->
        run_testu01 ~batch ~state ~name ~int64_refill ~reverse stream "crush"
    | Big -> run_testu01 ~batch ~state ~name ~int64_refill ~reverse stream "big"
    | Practrand tlmax ->
        run_practrand ~batch ~state ~name ~int64_refill ~threshold ~reverse
          stream tlmax
  in
  if full then Printf.printf "=== forward ===\n%!";
  run ~reverse:false state;
  if full then (
    Printf.printf "\n=== bit-reversed ===\n%!";
    run ~reverse:true (replay_state ()))

let full_arg =
  Arg.(
    value & flag
    & info [ "full" ]
        ~doc:
          "Run the selected battery twice back-to-back against the identical \
           underlying stream: once forward, once with every 64-bit output word \
           bit-reversed before it reaches the battery. TestU01/PractRand's \
           tests aren't symmetric in bit position -- some are far more \
           sensitive to defects in specific bits than others -- so this is a \
           cheap second opinion alongside the forward-only run.")

let () =
  let term =
    Term.(
      const main $ stream_arg $ battery_arg $ seed_arg $ seed_string_arg
      $ seed_hex_arg $ tlmax_arg $ batch_arg $ threshold_arg $ bias_arg
      $ bias_random_bit_arg $ full_arg)
  in
  let cmd = Cmd.v (Cmd.info "ocaml_battery") term in
  exit (Cmd.eval cmd)
