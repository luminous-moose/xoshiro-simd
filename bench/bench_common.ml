type test = { name : string; available : unit -> bool; test : unit -> unit }

let pp_duration seconds =
  let tenths = int_of_float (Float.round (seconds *. 10.0)) in
  let minutes = tenths / 600 in
  let secs = float_of_int (tenths mod 600) /. 10.0 in
  if minutes > 0 then Printf.sprintf "%dm%04.1fs" minutes secs
  else Printf.sprintf "%.1fs" secs

let test_reps = 16

(* Bechamel's per-test time budget and sample cap. The sample cap is set to 6000
   here to increase it from the default 3000. *)
let quota_seconds = 1.0
let sample_limit = 6000

let kahan_add x sum c =
  let y = x -. c in
  let t = sum +. y in
  let c = t -. sum -. y in
  let sum = t in
  (sum, c)

let kahan_sum list =
  let rec loop sum c = function
    | [] -> sum
    | hd :: tl ->
        let sum, c = kahan_add hd sum c in
        loop sum c tl
  in
  loop 0. 0. list

(* The stats gathered are the mean, stddev, and standard error of the mean for
   each test. *)
let stats vs =
  let n = float_of_int (List.length vs) in
  let mean = kahan_sum vs /. n in
  if n < 2.0 then (mean, 0.0, 0.0)
  else
    let squared_deviations =
      List.map
        (fun v ->
          let d = v -. mean in
          d *. d)
        vs
    in
    let stddev = sqrt (kahan_sum squared_deviations /. (n -. 1.0)) in
    (mean, stddev, stddev /. sqrt n)

let testnames_of commonname tests =
  let open Bechamel in
  let btests =
    List.map
      (fun t ->
        let name = t.name in
        let btest = Test.make ~name (Staged.stage t.test) in
        btest)
      tests
  in
  Test.names (Test.make_grouped ~name:commonname btests)

(* We only ever measure time in these benchmarks. Bechamel supports more
   measures and returns a list. The list will always have one item in this code.
 *)
let ols_estimate (t : Bechamel.Analyze.OLS.t) =
  match Bechamel.Analyze.OLS.estimates t with
  | Some [ v ] -> v
  | _ -> failwith "measure_one: expected exactly one OLS estimate"

let measure_one test =
  let open Bechamel in
  let open Toolkit in
  let instance = Instance.monotonic_clock in
  let cfg =
    Benchmark.cfg ~limit:sample_limit ~quota:(Time.second quota_seconds) ()
  in
  let ols =
    Analyze.ols ~r_square:false ~bootstrap:0 ~predictors:[| Measure.run |]
  in
  let btest = Test.make ~name:test.name (Staged.stage test.test) in
  let raw_results = Benchmark.all cfg [ instance ] btest in
  let analyzed = Analyze.all ols instance raw_results in
  let ols_value = Hashtbl.find analyzed test.name in
  let value = ols_estimate ols_value in
  value

let worker_flag = "--worker"

(* --only-test test makes the orchestrator run only a single named test instead
   of all of them. It can be used to find repeatable bias introduced by the
   binary the test is in. *)
let only_test_flag = "--only-test"

(* --sleep n sleeps for n seconds between test runs. Useful when running
   benchmarks on a machine that has marginal cooling and easily reaches its
   downclocking temperature, which many laptops do. The default is 0. *)
let sleep_flag = "--sleep"

let known_flags = [ worker_flag; only_test_flag; sleep_flag ]

let find_flag_value flag argv =
  let n = Array.length argv in
  let rec loop i =
    if i >= n - 1 then None
    else if String.equal argv.(i) flag then Some argv.(i + 1)
    else loop (i + 1)
  in
  loop 1

let has_flag flag argv = Array.exists (String.equal flag) argv

(* Check for anything that looks like a flag but doesn't match our flag list, so
   we can catch a typo or incorrect flag passed in. *)
let check_known_flags argv =
  Array.iteri
    (fun i arg ->
      if
        i > 0
        && String.length arg >= 2
        && String.equal (String.sub arg 0 2) "--"
        && not (List.mem arg known_flags)
      then (
        Printf.eprintf "unknown flag: %s\n%!" arg;
        exit 1))
    argv

let parse_stdout s =
  let last_line =
    match String.rindex_opt s '\n' with
    | None -> s
    | Some i -> String.sub s (i + 1) (String.length s - i - 1)
  in
  float_of_string last_line

(* The worker runs the test and prints the measured mean to stdout, always on
   the last line. %.17g round-trips a double exactly so we can preserve the full
   precision of the result. *)
let run_worker test =
  let mean_time = measure_one test in
  Printf.printf "\n%.17g" mean_time

(* The orchestrator is the mode used from the initial commandline call, and
   handles spawning the subprocesses with execve, so ASLR randomizes the address
   space for each one and the effects of that can be averaged out. *)
let run_orchestrator commonname (tests : test list) =
  let testnames = testnames_of commonname tests in
  let testnames_arr = Array.of_list testnames in

  let name_width =
    List.fold_left (fun acc n -> max acc (String.length n)) 0 testnames
  in
  let n_tests = Array.length testnames_arr in
  let sleep_seconds =
    match find_flag_value sleep_flag Sys.argv with
    | None -> 0.0
    | Some s -> (
        match float_of_string_opt s with
        | Some v when Float.is_finite v && v >= 0.0 -> v
        | _ ->
            Printf.eprintf
              "%s: %s needs a non-negative number of seconds, got %S\n%!"
              commonname sleep_flag s;
            exit 1)
  in
  let n_runs = n_tests * test_reps in
  let estimated_seconds =
    (quota_seconds *. float_of_int n_runs)
    +. (sleep_seconds *. float_of_int (max 0 (n_runs - 1)))
  in
  if sleep_seconds > 0.0 then
    Printf.printf
      "%s: estimated run time %s (%d tests x %d reps x %.1fs quota, %.1fs \
       sleep between runs)\n\
       %!"
      commonname
      (pp_duration estimated_seconds)
      n_tests test_reps quota_seconds sleep_seconds
  else
    Printf.printf
      "%s: estimated run time %s (%d tests x %d reps x %.1fs quota)\n%!"
      commonname
      (pp_duration estimated_seconds)
      n_tests test_reps quota_seconds;
  let samples = Hashtbl.create n_tests in
  let exe = Sys.executable_name in
  let forwarded_flags =
    match find_flag_value only_test_flag Sys.argv with
    | None -> []
    | Some name -> [ only_test_flag; name ]
  in
  let started = ref 0 in
  for _ = 1 to test_reps do
    List.iteri
      (fun i test ->
        if !started > 0 && sleep_seconds > 0.0 then Unix.sleepf sleep_seconds;
        incr started;
        match
          Process.run exe ([ worker_flag; string_of_int i ] @ forwarded_flags)
        with
        | Error e ->
            Printf.eprintf "%s: failed to start a bench worker: %s\n%!"
              commonname e;
            exit 1
        | Ok { exit_code = 0; stdout; _ } ->
            let mean_time = parse_stdout stdout in
            let name = testnames_arr.(i) in
            Hashtbl.add samples name mean_time
        | Ok { exit_code; stderr; _ } ->
            Printf.eprintf "%s: a bench worker exited %d:\n%s\n%!" commonname
              exit_code stderr;
            exit 1)
      tests
  done;
  Printf.printf "%-*s  %9s  %10s  %8s\n" name_width "Test" "Mean (ns)"
    "StdDev (ns)" "SEM (ns)";
  let pp_result name =
    let vs = Hashtbl.find_all samples name in
    let mean, stddev, sem = stats vs in
    Printf.printf "%-*s  %9.2f  %10.2f  %8.2f\n" name_width name mean stddev sem
  in
  List.iter pp_result testnames

(* This is called in both the top level process and the worker process. So we
   check which one we're in, as well as handling other commandline flags. *)
let run_and_print commonname (tests : test list) =
  let argv = Sys.argv in
  check_known_flags argv;
  let btests =
    match find_flag_value only_test_flag argv with
    | None ->
        List.filter_map (fun t -> if t.available () then Some t else None) tests
    | Some name -> (
        match List.filter (fun t -> String.equal t.name name) tests with
        | [] ->
            Printf.eprintf "%s: no test named %s\n%!" commonname name;
            exit 1
        | filtered ->
            List.iter
              (fun t ->
                if not (t.available ()) then (
                  Printf.eprintf "%s: test named %s is not available\n%!"
                    commonname name;
                  exit 1))
              filtered;
            filtered)
  in
  match find_flag_value worker_flag argv with
  | Some index_str ->
      let test = List.nth btests (int_of_string index_str) in
      run_worker test
  | None ->
      run_orchestrator commonname btests
