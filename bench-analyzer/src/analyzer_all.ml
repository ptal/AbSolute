open Absolute_analyzer

type strategy_2 = {
  solver_name : string;
  strategy_name : string;
  all: (string, instance) Hashtbl.t;
  steps : (float*int) list; (*time * value*)
}

type instances_set_2 = {
  problem_name : string;
  instance_name : string;
  nb_instances: int;
  strategies : strategy_2 list;
}

let timeout = 600.

let add_key key _ li =
  key::li

let get_keys tbl =
  Hashtbl.fold (add_key) tbl []

let remove_last_char chars =
  let size = String.length chars in
  if size > 0 then
    String.sub chars 0 ((String.length chars) -1)
  else chars

let float_option_to_string t =
  match t with
  | Some(t) -> (string_of_float t)^"0"
  | None -> (string_of_float timeout)^"0"

let convert_solver (solver : solver) (strategy : strategy) =
  {solver_name = solver.name; strategy_name = strategy.name; all = strategy.all; steps = [];}

let append_solvers solvers =
  let rec aux (solvers : solver list) strategies =
    match solvers with
    | [] -> strategies
    | s::solvers ->
        let li = List.map (convert_solver s) s.strategies in
        aux solvers (List.append strategies li)
  in aux solvers []

let convert_instance problem (instances_set : instances_set) =
  let nb_instances = (Hashtbl.length (List.hd (List.hd instances_set.solvers).strategies).all) in
  {problem_name = problem.name; instance_name = instances_set.name; nb_instances = nb_instances;strategies = append_solvers instances_set.solvers}

let append_problems (database : problem list) =
  let rec append_problems_rec database instances =
  match database with
  [] -> instances
  |p::database -> let li = List.map (convert_instance p) p.instances_set in
  append_problems_rec database (List.append instances li)
in append_problems_rec database []