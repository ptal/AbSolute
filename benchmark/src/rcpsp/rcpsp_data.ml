open Scanf

(* The information on the number and kind of resources available.
   We only store renewable resources for now. *)
type resources_info = {
  renewable: int;
  nonrenewable: int;
  doubly_constrained: int;
}

let check_resources_info r =
  if r.nonrenewable > 0 || r.doubly_constrained > 0 then
    failwith "ProGenMax model with nonrenewable or doubly constrained resources: not yet implemented."
  else
    r

type precedence = {
  job_index: int;
  mode: int;
  successors: int;
  job_successors: int list;
  (* for all successor job_j: job_i + weights_ij <= job_j *)
  weights: int list;
}

type job = {
  job_index: int;
  mode: int;
  duration: int;
  (* The length of `resources_usage` must match `rcpsp.resources_capacities`.
     The resources really used by the project are given in `project.resources`. *)
  resources_usage: int list
}

type project = {
  project_idx: int;
  jobs_number: int; (* including dummy source and sink. *)
  horizon: int;
  precedence_relations: precedence list;
  jobs: job list;
  resources_idx: int list; (* The indexes of the resources used by the job. *)
}

type rcpsp = {
  resources_capacities: int list;
  projects: project list;
}

let map_projects f rcpsp = { rcpsp with projects = (List.map f rcpsp.projects) }

(* Parsing/utility functions that are common across formats (Patterson, SM, ProGen/max).  *)

let ignore_lines file n =
  for _=1 to n do
    ignore(bscanf file "%[^\n]\n" (fun _ -> ()))
  done

let read_trailing_int_list file n =
  let numbers = List.map (fun _ -> bscanf file " %d " (fun x->x)) (Tools.range 1 n) in
  numbers

let number_of_resources rcpsp = List.length rcpsp.resources_capacities

let compute_horizon project =
  let horizon = List.fold_left (fun a j ->
    let weights = List.flatten (List.map (fun (p:precedence) ->
      if p.job_index = j.job_index then p.weights else []) project.precedence_relations) in
    let max_dur = List.fold_left max j.duration weights in
    a + max_dur
    ) 0 project.jobs in
  {project with horizon = horizon}
