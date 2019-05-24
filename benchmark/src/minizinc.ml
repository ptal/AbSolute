open Printf
open System
open Rcpsp
open Rcpsp_data
(* open Rcpsp_model *)
(* open Csp *)
open Bench_instance_j

let dzn_file = "tmp.dzn"
let mzn_file = "tmp.mzn"
let fzn_file = "tmp.fzn"
let output_file = "out.txt"

let minizinc_options config solver =
  "--output-time " ^
  (Printf.sprintf "--time-limit %d " (config.timeout * 1000)) ^
  "--solution-separator \"\" " ^
  "--unsat-msg \"unsat\" " ^
  "--unbounded-msg \"unbounded\" " ^
  "--unknown-msg \"unknown\" " ^
  "--error-msg \"error\" " ^
  "--search-complete-msg \"complete\" " ^
  "--no-output-comments " ^
  "--output-to-file " ^ output_file ^ " " ^
  "--solver " ^ solver

let create_file data name =
  let oc = open_out name in
  fprintf oc "%s\n" data;
  close_out oc

let create_dzn_file data = create_file data dzn_file
let create_mzn_file data = create_file data mzn_file

(* let update_with_optimum measure numbers =
  if numbers <> [] then
    let latest_bound = List.nth numbers ((List.length numbers) - 1) in
    Measurement.{ measure with optimum = Some (Bound_rat.of_int latest_bound) }
  else measure

let update_time config measure text =
  (* Remove empty lines. *)
  let text = List.flatten (List.map (fun t -> if (String.length t) = 0 then [] else [t]) text) in
  (* The last line is the status. *)
  let status = (List.nth text ((List.length text) - 1)) in
  (* Unfortunately, MiniZinc does not print the elapsed time for unsatisfiable instance.
     In this case, we fall back on the time measured here (less precise). *)
  let measure =
    if (List.length text) > 1 then
      let line_with_time = List.hd text in
      let elapsed_time = Scanf.sscanf line_with_time "%% time elapsed: %d.%d s "
        (fun a b -> time_of_ms (a*1000 + b*10)) in
      Measurement.{measure with stats={measure.stats with elapsed=elapsed_time}}
    else measure in
  if String.equal status "unknown" then
    measure, status
  else
    Measurement.update_time config measure.stats measure, status

let create_minizinc_measure stats config problem_path result =
  let measure = Measurement.init stats problem_path (`BoxedOctagon `Integer) 1. in
  let lines = String.split_on_char '\n' result in
  let (numbers, text) = List.partition
    (fun l -> try ignore(int_of_string l); true with Failure _ -> false) lines in
  let numbers = List.map int_of_string numbers in
  let measure = update_with_optimum measure numbers in
  let (measure, status) = update_time config measure text in
  (measure, status)

let bench_minizinc info config problem_path solver model dzn_file =
  let solver_command = prepare_solver_command config model dzn_file in
  let mzn_command = "minizinc " ^ (minizinc_options config solver) ^ " " ^ model ^ " "
    ^ dzn_file ^ " >> " ^ output_file ^ " 2> /dev/null" in
  (* Printf.printf "%s\n" mzn_command; *)
  let _ = call_command ("rm " ^ output_file ^ " 2> /dev/null; touch " ^ output_file) in
  let stats = State.init_global_stats () in
  let _ = call_command mzn_command in
  let stats = {stats with elapsed=Mtime_clock.count stats.start} in
  let result = file_to_string output_file in
  let (measMeasurement.ure, status) = create_minizinc_measure stats config problem_path result in
  if String.equal status "error" then begin
    Measurement.print_exception problem_path "MiniZinc error";
    Measurement.add_erroneous_measure info end
  else begin
    Measurement.print_as_csv config measure;
    Measurement.add_measure info measure
  end

let benchmark_suite_minizinc config solver model =
  Printf.printf "      << %s(%s) >>\n\n" solver model;
  let info = Measurement.empty_info in
  let problems = list_of_problems config in
  Measurement.print_csv_header config;
  let info = List.fold_left (fun info problem_path ->
      let rcpsp = make_rcpsp config problem_path in
      if (List.length rcpsp.projects) > 1 then failwith "MiniZinc model for multi-project RCPSP is not yet supported.";
      let project = List.hd rcpsp.projects in
      let data = make_dzn_data rcpsp project in
      create_dzn_file data;
      bench_minizinc info config problem_path solver model dzn_file
    ) info problems in
  Measurement.print_bench_results model info

(* Flat MiniZinc model: model generated from the Rcpsp_model structure where data is written as constraints.
   This model does not contain global constraint. *)
Measurement.
let mzn_of_bconstraint c = "constraint " ^ string_of_bconstraint c ^ ";\n"

let mzn_of_reified (b, conjunction) =
  let (first, tail) = (List.hd conjunction, List.tl conjunction) in
  "constraint " ^ b ^
  " <-> (" ^ (string_of_bconstraint first) ^
  (List.fold_left (fun a c -> a ^ " /\\ " ^ (string_of_bconstraint c)) "" tail) ^
  ");\n"

let lookup_solver_config solvers solver_name = List.find_opt (fun s -> s.name = solver_name) solvers

let benchmark_suite_flatmzn config flatmzn =
  match lookup_solver_config config.solvers_config flatmzn.name with
  | None -> System.print_warning ("Solver `" ^ flatmzn.name ^ "` is not registered in the file \"benchmark/config/solvers.json\".")
  | Some solver_config ->
      Measurement.print_solver solver_config "flat" strategy;

    let name = solver ^ "(" ^ search_options ^ ")" in
    Printf.printf "      << %s >>\n\n" name;
    let info = Measurement.empty_info in
    let problems = list_of_problems config in
    Measurement.print_csv_header config;
    let info = List.fold_left (fun info problem_path ->
        let model = Rcpsp_model.create_rcpsp (make_rcpsp config problem_path) in
        let mzn_model = make_mzn_model model search_options in
        create_mzn_file mzn_model;
        bench_minizinc info config problem_path solver mzn_file ""
      ) info problems in
    Measurement.print_bench_results name info *)

let solver_output_to_entries output =
  let data = file_to_string output in
  let lines = String.split_on_char '\n' data in
  let mzn_entries =
    lines |>
    List.filter (fun l -> if String.length l > 0 then l.[0] = '%' else false) |>
    List.map (String.split_on_char ' ') |>
    List.filter (fun l -> (List.length l) = 2) |>
    List.map (fun l ->
      let entry = List.nth l 1 in
      let entry = String.split_on_char '=' entry in
      (List.nth entry 0, List.nth entry 1)
    ) in
  try
    let obj = Scanf.sscanf (List.nth lines 0) "objective = %d;" string_of_int in
    mzn_entries@[("objective", obj)]
  with _ -> mzn_entries

let print_result_as_csv bench problem_path output =
  let entries = solver_output_to_entries output in
  let stats = State.init_global_stats () in
  let stats = State.{ stats with
    nodes=(int_of_string (List.assoc "nodes" entries));
    fails=(int_of_string (List.assoc "failures" entries));
    sols=(int_of_string (List.assoc "solutions" entries));
  } in
  let measure = Measurement.init stats problem_path in
  let time = int_of_float ((float_of_string (List.assoc "solveTime" entries)) *. 1000000.) in
  let stats = State.{stats with elapsed=(time_of 1000 time)} in
  let measure = Measurement.update_time bench stats measure in
  let measure = Measurement.{ measure with
    optimum=Some (Bound_rat.of_string (List.assoc "objective" entries)) } in
  Measurement.print_as_csv bench measure

let run_mzn_bench bench (mzn_instance: mzn_instance) problem_path fzn_file =
  let time = string_of_int (bench.timeout * 1000) in
  let command = mzn_instance.solver.Bench_desc_j.exec ^
    " -time " ^ time ^
    " -s " ^ (* Print statistics. *)
    fzn_file ^
    " > " ^ output_file in
  let _ = call_command command in
  print_result_as_csv bench problem_path output_file

let string_of_list to_string l = List.fold_left (fun s e -> s ^ (to_string e) ^ ", ") "" l

let list_to_mzn name l =
  name ^ " = [" ^
  (string_of_list string_of_int l) ^
  "];\n"

let string_of_2D_list name l =
  name ^ " = [|\n" ^
  (List.fold_left (fun a r -> a ^ (string_of_list string_of_int r) ^ "\n  |") "" l) ^
  "];\n"

let string_of_resources project =
  let rr = List.map
    (fun r_idx -> List.map (fun j -> List.nth j.resources_usage r_idx) project.jobs)
    project.resources_idx in
  string_of_2D_list "rr" rr

let string_of_difference_constraints project =
  let dc = List.flatten (List.map (fun (p:precedence) ->
      List.map2 (fun w s -> [p.job_index; w; s]) p.weights p.job_successors
    ) project.precedence_relations) in
  string_of_2D_list "dcons" dc

let make_dzn_data rcpsp project =
  (Printf.sprintf "n_res = %d;\n" (List.length rcpsp.resources_capacities)) ^
  (list_to_mzn "rcap" (List.map (List.nth rcpsp.resources_capacities) project.resources_idx)) ^
  (Printf.sprintf "n_tasks = %d;\n" project.jobs_number) ^
  (list_to_mzn "dur" (List.map (fun j -> j.duration) project.jobs)) ^
  (string_of_resources project) ^
  (Printf.sprintf "n_dc = %d;\n" (List.fold_left (+) 0 (List.map (fun j -> j.successors) project.precedence_relations))) ^
  (string_of_difference_constraints project)

let create_search_annot strategy = "solve::" ^ strategy.Bench_desc_j.mzn_annot ^ ";"

let create_mzn_model mzn_instance =
  let model = file_to_string mzn_instance.model in
  let search = create_search_annot mzn_instance.strategy in
  model ^ "\n" ^ search

let create_fzn_file solver mzn_file dzn_file =
  let command = "mzn2fzn --no-optimize -G " ^ solver.Bench_desc_j.globals ^ " -o " ^ fzn_file ^ " -d " ^ dzn_file ^ " " ^ mzn_file in
  ignore (call_command command)

let bench_mzn_instance bench mzn_instance problem_path =
  let model = create_mzn_model mzn_instance in
  create_mzn_file model;
  let rcpsp = read_rcpsp problem_path in
  if (List.length rcpsp.projects) > 1 then System.eprintf_and_exit "MiniZinc model for multi-project RCPSP is not yet supported.";
  let project = List.hd rcpsp.projects in
  let data = make_dzn_data rcpsp project in
  create_dzn_file data;
  create_fzn_file mzn_instance.solver mzn_file dzn_file;
  run_mzn_bench bench mzn_instance problem_path fzn_file

let bench_minizinc bench mzn_instance =
  Measurement.print_csv_header bench;
  let problems = list_of_problems bench in
  List.iter (bench_mzn_instance bench mzn_instance) problems