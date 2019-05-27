open Bench_desc_j
open Bench_instance_j

let find_solver benchmark name =
  try
    List.find (fun (c: solver_config) -> String.equal c.name name) benchmark.solvers_config
  with Not_found ->
    System.eprintf_and_exit ("Solver named `" ^ name ^ "` is not described in the configuration file. Please see `benchmark/data/benchmarks.json` for example of solver configuration.")

let make_absolute benchmark info =
  let absolute_solver = find_solver benchmark "absolute" in
  List.flatten (
  List.map (fun domain ->
  List.map (fun strategy ->
    `AbSoluteKind ({ version=absolute_solver.version;
      domain=domain.name;
      strategy=strategy; })
  ) domain.strategies
  ) info.domains)

let make_minizinc_kind benchmark (info: mzn_kind) =
  let solver_configs = List.map (fun name -> find_solver benchmark name) info.solvers in
  List.flatten (
  List.map (fun solver -> List.flatten (
  List.map (fun model ->
  List.map (fun strategy ->
    `MznKind ({ solver; strategy; model; })
  ) info.strategies
  ) info.models)
  ) solver_configs)

let make_decomposed_mzn_kind benchmark info =
  let solver_configs = List.map (fun name -> find_solver benchmark name) info.solvers in
  List.flatten (
  List.map (fun solver ->
  List.map (fun strategy ->
    `DecomposedKind ({ solver; strategy; })
  ) info.strategies
  ) solver_configs)

let make_solver benchmark = function
| `AbSolute(info) -> make_absolute benchmark info
| `MiniZinc(info) -> make_minizinc_kind benchmark info
| `DecomposedMzn(info) -> make_decomposed_mzn_kind benchmark info

let make_solver_instances benchmark =
  List.flatten (List.map (make_solver benchmark) benchmark.solvers_kind)

let make_instances benchmark problem_set =
  let solver_instances = make_solver_instances benchmark in
  List.map (fun s ->
    { problem_set_path=problem_set.path;
      timeout=problem_set.timeout;
      csv=benchmark.csv;
      solver_instance=s; }) solver_instances

let gen_benches benchmark =
  List.flatten (List.map (make_instances benchmark) benchmark.problem_sets)

let finalize_bench benchmark bench =
  let path = System.concat_dir benchmark.input_dir bench.problem_set_path in
  {bench with problem_set_path=path}

let create_solver_dir bench =
  match bench.solver_instance with
  | `AbSoluteKind(instance) -> "absolute-" ^ instance.version
  | `MznKind(instance) -> instance.solver.name ^ "-" ^ instance.solver.version
  | `DecomposedKind(instance) -> instance.solver.name ^ "-" ^ instance.solver.version

let create_result_filename bench =
  match bench.solver_instance with
    | `AbSoluteKind(instance) -> instance.domain ^ "-" ^ instance.strategy
    | `MznKind(instance) -> (Filename.remove_extension (Filename.basename instance.model)) ^ "-" ^ instance.strategy.short
    | `DecomposedKind(instance) -> "box-" ^ instance.strategy.short

let register_bench benchmark bench =
  let solver_dir = create_solver_dir bench in
  let path = List.fold_left System.concat_dir benchmark.output_dir
    [bench.problem_set_path; solver_dir] in
  let _ = System.call_command ("mkdir -p " ^ path) in
  let result_filename = create_result_filename bench in
  let bench_instance_file = System.concat_dir path (result_filename ^ ".json") in
  let bench = finalize_bench benchmark bench in
  let data = Yojson.Safe.prettify (string_of_bench_instance bench) in
  System.string_to_file bench_instance_file data;
  let bench_instance_output = System.concat_dir path (result_filename ^ ".csv") in
  Printf.printf "%s %s %s\n" benchmark.bench_exec bench_instance_file bench_instance_output

let config_from_json json_data =
  try
    benchmark_of_string json_data
  with
  | Atdgen_runtime__Oj_run.Error(msg)
  | Yojson.Json_error(msg) ->
      System.eprintf_and_exit (Printf.sprintf
        "The benchmarks description file contains an error:\n\n\
         %s\n\n\
        [help] Be careful to the case: \"int\" is not the same as \"Int\".\n\
        [help] You can find a full example of the JSON format in benchmark/data/benchmarks.json." msg)

let () =
  (* Printexc.record_backtrace true; *)
  let benchmark = config_from_json (System.get_bench_desc ()) in
  let benches = gen_benches benchmark in
  Printf.printf "%d bench files generated.\n" (List.length benches);
  List.iter (register_bench benchmark) benches
