open Scanf
open Rcpsp_data

let read_field_value file =
  let s = bscanf file "%[^\n]\n" (fun x -> x) in
  String.trim (List.nth (String.split_on_char ':' s) 1)

let read_int_field_value file =
  let value = read_field_value file in
  int_of_string value

let read_resource_field_value file =
  let value = read_field_value file in
  let value = String.trim (List.nth (String.split_on_char ' ' value) 0) in
  int_of_string value

let read_resources_info file =
  ignore_lines file 1;
  let renewable = read_resource_field_value file in
  let nonrenewable = read_resource_field_value file in
  let doubly_constrained = read_resource_field_value file in
  let r = {renewable=renewable; nonrenewable=nonrenewable; doubly_constrained=doubly_constrained} in
  check_resources_info r

let read_rcpsp_info file =
  let _nb_projects = read_int_field_value file in
  (* Include dummy source and sink. *)
  let jobs_number = read_int_field_value file in
  let horizon = read_int_field_value file in
  let resources_info = read_resources_info file in
  ignore_lines file 3;
  let (project_idx, _) =
    (* The unused fields are: rel_date, due_date, tard_cost, mpm_time. *)
    bscanf file " %d %d %d %d %d %d\n" (fun a b _ _ _ _ -> (a-1, b)) in
  let project =
    { project_idx=project_idx;
      jobs_number=jobs_number;
      horizon=horizon;
      precedence_relations=[];
      jobs=[];
      resources_idx=(Tools.range 0 (resources_info.renewable - 1)); } in
  { resources_capacities = [];
    projects = [project] }

let read_precedence file =
  let prec = bscanf file " %d %d %d " (fun a b c -> {
    job_index=a;
    mode=b;
    successors=c;
    job_successors=[];
    weights=[];
  }) in
  let job_successors = read_trailing_int_list file prec.successors in
  {prec with job_successors=job_successors}

let read_precedence_relations file project =
  ignore_lines file 3;
  let precedence_relations = List.map (fun _ -> read_precedence file) (Tools.range 1 project.jobs_number) in
  { project with precedence_relations=precedence_relations}

let read_job file project _ =
  let job = bscanf file " %d %d %d " (fun a b c -> {
    job_index=a;
    mode=b;
    duration=c;
    resources_usage=[]
  }) in
  let job = {job with resources_usage=read_trailing_int_list file (List.length project.resources_idx)} in
  let precedence_relations = List.map (fun (prec:precedence) ->
    if prec.job_index = job.job_index then
      let durations = List.map (fun _ -> job.duration) (prec.job_successors) in
      {prec with weights=durations}
    else prec
  ) project.precedence_relations in
  { project with
    precedence_relations=precedence_relations;
    jobs=project.jobs@[job]; }

let read_jobs file project =
  ignore_lines file 4;
  List.fold_left (read_job file) project (Tools.range 1 project.jobs_number)

let read_resource_availabilities file rcpsp =
  ignore_lines file 3;
  let resources_numbers = List.length (List.hd rcpsp.projects).resources_idx in
  let resources_capacities = read_trailing_int_list file resources_numbers in
  { rcpsp with resources_capacities=resources_capacities }

let read_sm file =
  ignore_lines file 4;
  read_rcpsp_info file |>
  map_projects (fun project -> read_precedence_relations file project |> read_jobs file) |>
  read_resource_availabilities file

(* see preconditions on `problem_path` at `psplib_to_absolute`. *)
let read_sm_file (problem_path: string) : rcpsp =
  let file = Scanning.open_in problem_path in
  let rcpsp = read_sm file in
  Scanning.close_in file;
  rcpsp
