open Printf
open System
open Rcpsp
open Rcpsp_data
open Rcpsp_model
open Csp
open Bench_instance_j

let dzn_file = "tmp.dzn"
let mzn_file = "tmp.mzn"
let fzn_file = "tmp.fzn"
let output_file = "out.txt"
let error_file = "err.txt"

let make_unique_file_name name =
  let rec aux name i =
    let base = Filename.remove_extension name in
    let ext = Filename.extension name in
    let path = "/tmp/" ^ base ^ (string_of_int i) ^ ext in
    if Sys.file_exists path then
      aux name (i+1)
    else
     path
  in
  aux name 0

let create_file data name =
  let oc = open_out name in
  fprintf oc "%s\n" data;
  close_out oc

let create_dzn_file data dzn_file = create_file data dzn_file
let create_mzn_file data mzn_file = create_file data mzn_file

(** Clean some default values added by solvers (e.g. Chuffed put `objective=-1`) in case of unsatisfiability. *)
let clean_up_entries mzn_entries lines =
  let unsat_msg = "=====UNSATISFIABLE=====" in
  let unsat_msg_len = String.length unsat_msg in
  let is_unsat_msg l =
    if String.length l >= unsat_msg_len then
      let unsat_part = String.sub (String.trim l) 0 unsat_msg_len in
      unsat_part = unsat_msg
    else false in
  if List.exists is_unsat_msg lines then begin
    let mzn_entries = List.flatten (List.map (fun (name, value) ->
      if name = "solutions" then [(name, "0")] else
      if name = "objective" then [] else
      [(name, value)]) mzn_entries) in
    if List.exists (fun (name, _) -> name = "solutions") mzn_entries then
      mzn_entries
    else mzn_entries@[("solutions","0")]
  end
  else mzn_entries

let mzn_solver_output_to_entries lines =
  let is_mzn_entry l = if String.length l > 0 then l.[0] = '%' else false in
  let is_not_mzn_entry l = not (is_mzn_entry l) in
  let mzn_entries =
    lines |>
    List.filter is_mzn_entry |>
    List.map (String.split_on_char ' ') |>
    List.filter (fun l -> (List.length l) = 2) |>
    List.map (fun l ->
      let entry = List.nth l 1 in
      let entry = String.split_on_char '=' entry in
      (List.nth entry 0, List.nth entry 1)
    ) in
  let mzn_entries =
    try
      let lines = List.filter is_not_mzn_entry lines in
      let obj = Scanf.sscanf (List.nth lines 0) "%s = %d;" (fun _ i -> string_of_int i) in
      mzn_entries@[("objective", obj)]
    with _ -> mzn_entries in
  clean_up_entries mzn_entries lines

let choco_solver_output_to_entries lines =
  eprintf_and_exit "Choco solver is not yet supported."

let solver_output_to_entries (solver: solver_config) output =
  let data = file_to_string output in
  let lines = String.split_on_char '\n' data in
  let name = solver.Bench_desc_j.name in
  if name = "choco" then
    choco_solver_output_to_entries lines
  (* In this case it is unsatisfiable at top-level, which means that Chuffed does not output any stat, not even time. *)
  else if name = "chuffed" && List.length lines = 2 then
    clean_up_entries [("solveTime","0.01")] lines
  else
    mzn_solver_output_to_entries lines

let get_entry name entries =
  match List.assoc_opt name entries with
  | Some x -> int_of_string x
  | None -> -1

let print_result_as_csv bench solver problem_path output =
  let entries = solver_output_to_entries solver output in
  let stats = State.init_global_stats () in
  let stats = State.{ stats with
    nodes=(get_entry "nodes" entries);
    fails=(get_entry "failures" entries);
    sols=(get_entry "solutions" entries);
  } in
  let measure = Measurement.init stats problem_path in
  let time = int_of_float ((float_of_string (List.assoc "solveTime" entries)) *. 1000000.) in
  let stats = State.{stats with elapsed=(time_of 1000 time)} in
  let measure = Measurement.update_time bench stats measure in
  let measure =
    match List.assoc_opt "objective" entries with
    | Some obj ->
        let b = Bound_rat.of_string obj in
        Measurement.{ measure with optimum=Some b}
    | None -> measure in
  Measurement.print_as_csv bench measure

let solver_time_option (solver: solver_config) time =
  if solver.Bench_desc_j.name = "choco" then
    " -tl " ^ time
  else
    " -t " ^ time

let run_mzn_bench bench solver problem_path fzn_file =
  let time = string_of_int (bench.timeout * 1000) in
  let output_file = make_unique_file_name output_file in
  let error_file = make_unique_file_name error_file in
  let command = solver.Bench_desc_j.exec ^
    (solver_time_option solver time) ^ " " ^
    fzn_file ^
    " > " ^ output_file ^ " 2> " ^ error_file in
  let _ = call_command command in
  print_result_as_csv bench solver problem_path output_file

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

let create_fzn_file solver mzn_file dzn_file fzn_file =
  let command = "mzn2fzn --no-optimize -I " ^ solver.Bench_desc_j.globals ^ " -o " ^ fzn_file ^ " " ^ mzn_file ^ " " ^ dzn_file in
  ignore (call_command command)

let bench_mzn_instance bench mzn_instance problem_path =
  let model = create_mzn_model mzn_instance in
  let mzn_file = make_unique_file_name mzn_file in
  create_mzn_file model mzn_file;
  let rcpsp = read_rcpsp problem_path in
  if (List.length rcpsp.projects) > 1 then System.eprintf_and_exit "MiniZinc model for multi-project RCPSP is not yet supported.";
  let project = List.hd rcpsp.projects in
  let data = make_dzn_data rcpsp project in
  let dzn_file = make_unique_file_name dzn_file in
  let fzn_file = make_unique_file_name fzn_file in
  create_dzn_file data dzn_file;
  create_fzn_file mzn_instance.solver mzn_file dzn_file fzn_file;
  run_mzn_bench bench mzn_instance.solver problem_path fzn_file

let bench_minizinc bench mzn_instance =
  Measurement.print_csv_header bench;
  let problems = list_of_problems bench in
  List.iter (bench_mzn_instance bench mzn_instance) problems

(* Decomposed MiniZinc model: model generated from the Rcpsp_model structure where data is written as constraints.
   This model does not contain global constraint. *)
let mzn_of_bconstraint c = "constraint " ^ string_of_constraint c ^ ";\n"

let mzn_of_reified (b, conjunction) =
  let (first, tail) = (List.hd conjunction, List.tl conjunction) in
  "constraint " ^ b ^
  " <-> (" ^ (string_of_constraint first) ^
  (List.fold_left (fun a c -> a ^ " /\\ " ^ (string_of_constraint c)) "" tail) ^
  ");\n"

let make_mzn_model model mzn_annot =
  (List.fold_left (fun a v -> a ^ "var bool: " ^ v ^ ";\n") "" model.box_vars) ^
  (List.fold_left (fun a v -> a ^ "var int: " ^ v ^ ";\n") "" model.octagonal_vars) ^
  (List.fold_left (fun a c -> a ^ (mzn_of_bconstraint c)) "" model.constraints) ^
  (List.fold_left (fun a r -> a ^ (mzn_of_reified r)) "" model.reified_bconstraints) ^
  (Printf.sprintf "output [\"objective = \", show(%s), \"\\n\"];\n" model.makespan) ^
  (Printf.sprintf "solve::int_search([%s], %s) minimize %s;\n"
    (string_of_list (fun x -> x) model.octagonal_vars) mzn_annot model.makespan)

let bench_decomposed_instance bench (instance: decomposed_instance) problem_path =
  let rcpsp = create_rcpsp (read_rcpsp problem_path) in
  let model = make_mzn_model rcpsp instance.strategy.Bench_desc_j.mzn_annot in
  let mzn_file = make_unique_file_name mzn_file in
  let fzn_file = make_unique_file_name fzn_file in
  create_mzn_file model mzn_file;
  create_fzn_file instance.solver mzn_file "" fzn_file;
  run_mzn_bench bench instance.solver problem_path fzn_file

let bench_decomposed_mzn bench instance =
  Measurement.print_csv_header bench;
  let problems = list_of_problems bench in
  List.iter (bench_decomposed_instance bench instance) problems