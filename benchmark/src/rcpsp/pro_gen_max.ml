open Scanf
open Rcpsp_data

(* NOTE: We index the jobs from 1 to N (to keep it uniform among formats).
   In this file format, there are initially indexed from 0. *)

let read_resources_info file =
  let r = bscanf file " %d %d %d" (fun a b c ->
    {renewable=a; nonrenewable=b; doubly_constrained=c}) in
  check_resources_info r

let read_pro_gen_info file =
  let jobs_number = bscanf file "%d " (fun x->x+2) in
  let resources_info = read_resources_info file in
  ignore_lines file 1;
  let project = {
    project_idx = 0;
    jobs_number=jobs_number;
    horizon=0;
    precedence_relations=[];
    jobs=[];
    resources_idx=(Tools.range 0 (resources_info.renewable-1));
  } in
  { resources_capacities=[];
    projects=[project] }

let read_trailing_weights file n =
  List.map (fun _ -> bscanf file " [%d]" (fun x->x)) (Tools.range 1 n)

let read_job file project _ =
  let job_index = bscanf file "%d " (fun a -> a+1) in
  let mode = bscanf file " %d " (fun a -> a) in (* Not sure it's the mode; always at "1" in the data set. *)
  let successor_number = bscanf file " %d" (fun a -> a) in
  let job_successors = List.map (fun j -> j + 1) (read_trailing_int_list file successor_number) in
  let weights = read_trailing_weights file successor_number in
  ignore_lines file 1;
  let job = {
    job_index=job_index;
    mode=mode;
    duration=0;
    resources_usage=[];
  } in
  let precedence = {
    job_index=job_index;
    mode=mode;
    successors=successor_number;
    job_successors=job_successors;
    weights=weights;
  } in
  { project with
      jobs=project.jobs@[job];
      precedence_relations=project.precedence_relations@[precedence] }

let read_jobs file rcpsp =
  let project = (List.hd rcpsp.projects) in
  let project = List.fold_left (read_job file) project (Tools.range 1 project.jobs_number) in
  { rcpsp with projects=[project] }

let read_job_info file rcpsp _ =
  let job_index = bscanf file "%d " (fun a -> a+1) in
  let _ = bscanf file " %d " (fun a -> a) in (* Not sure it's the mode; always at "1" in the data set. *)
  let duration = bscanf file " %d " (fun a -> a) in
  let project = List.hd rcpsp.projects in
  let resources_usage = read_trailing_int_list file (List.length project.resources_idx) in
  let jobs = List.map (fun job ->
    if job.job_index = job_index then
      {job with duration=duration; resources_usage=resources_usage}
    else job) project.jobs in
  { rcpsp with projects=[{project with jobs=jobs}] }

let read_resources_capacities file rcpsp =
  let resources_number = (List.length (List.hd rcpsp.projects).resources_idx) in
  { rcpsp with resources_capacities = read_trailing_int_list file resources_number }

let read_duration_and_resources file rcpsp =
  List.fold_left (read_job_info file) rcpsp (Tools.range 1 (List.hd rcpsp.projects).jobs_number) |>
  read_resources_capacities file

let read_pro_gen file =
  read_pro_gen_info file |>
  read_jobs file |>
  read_duration_and_resources file |>
  map_projects compute_horizon

(* see preconditions on `problem_path` at `psplib_to_absolute`. *)
let read_pro_gen_file (problem_path: string) : rcpsp =
  let file = Scanning.open_in problem_path in
  let rcpsp = read_pro_gen file in
  Scanning.close_in file;
  rcpsp
