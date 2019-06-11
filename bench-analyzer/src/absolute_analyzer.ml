open Scanf

type optimum =
| Bounded of int * int (* [lb, ub] is the interval in which the optimum must be if it is satisfiable. *)
| Unsat

let no_lb = min_int
let no_ub = max_int

let optimum_dir = "optimum"
let optimum_file = optimum_dir ^ "/optimum.csv"

(** Solving information of an instance from a problems' set. *)
type instance = {
  problem: string;
  time: float option;
  bound: optimum;
  (* For nodes, solutions and fails, when the information is not available, we set it to 0. *)
  nodes: int;
  solutions: int;
  fails: int;
}

let empty_instance = {
  problem="";
  time=None;
  bound=Bounded(no_lb, no_ub);
  nodes=0;
  solutions=0;
  fails=0;
}

type strategy = {
  name: string;
  (* The name of the instance mapped to the time (if it did not timeout) and optimum found. *)
  all: (string, instance) Hashtbl.t;
  (* number of instances with at least one solution that is not proven optimal. *)
  feasible: int;
  (* number of instances with a proven optimum solution. *)
  optimum: int;
  (* number of instances proven unsatisfiable. *)
  unsat: int;
  (* difference between the lower bound obtained `lb` and the `best` known lower bound.
     Obtained with `(lb - best) / best`. *)
  delta_lb: float;
}

type solver = {
  name: string;
  strategies: strategy list;
}

type instances_set = {
  name: string;
  solvers: solver list;
  optimum: (string, optimum) Hashtbl.t
}

type problem = {
  name: string;
  instances_set: instances_set list;
}

type database = problem list

let check_solution name best_optimum obtained_optimum =
  match best_optimum, obtained_optimum with
  | Bounded(lb,_), Bounded(_,ub') when ub' < lb -> failwith (
      "wrong optimum upper bound computed on " ^ name ^ " - " ^
      "expected LB: " ^ (string_of_int lb) ^
      "obtained UB: " ^ (string_of_int ub'))
  | Bounded(_,ub), Bounded(lb',_) when lb' > ub -> failwith (
      "wrong optimum lower bound computed on " ^ name ^ " - " ^
      "expected UB: " ^ (string_of_int ub) ^
      "obtained LB: " ^ (string_of_int lb'))
  | Bounded _, Bounded _ -> ()
  | Bounded _, Unsat -> failwith ("obtained an unsatisfiable instance but a solution is known to exist on " ^ name)
  | Unsat, Bounded (_,ub) when ub <> no_ub -> failwith ("obtained a solution on an unsatisfiable instance on " ^ name)
  | Unsat, Bounded _ -> () (* OK because we did no find an upper bound to the problem yet. *)
  | Unsat, Unsat -> ()

let check_solutions_validity optimum strategy =
  Hashtbl.iter (fun name expected_opt ->
    try
      let instance = Hashtbl.find strategy.all name in
      check_solution name expected_opt instance.bound
    with Not_found ->
      Printf.printf "[error] Could not find instance %s\n" name)
    optimum

let compute_delta_lb best opt =
  match best with
  | Bounded(_, ub) -> ((float_of_int opt) -. (float_of_int ub)) /. (float_of_int ub)
  | Unsat -> failwith "expected unsatisfiable instance: unreachable (should be checked in `check_solution`)."

let process_instances optimum name instance (strat: strategy) =
  let best = Hashtbl.find optimum name in
  match instance.bound with
  | Bounded(lb,ub) when lb = ub -> {strat with optimum=(strat.optimum + 1)}
  | Bounded(_,ub) when ub <> no_ub -> { strat with
      feasible=(strat.feasible + 1);
      delta_lb=(strat.delta_lb +. (compute_delta_lb best ub))
    }
  | Bounded _ -> strat
  | Unsat -> {strat with unsat=(strat.unsat + 1)}

let process_strategy optimum (strategy : strategy) =
begin
  (* Printf.printf "%s\n" strategy.name; *)
  check_solutions_validity optimum strategy;
  let strategy = Hashtbl.fold (process_instances optimum) strategy.all strategy in
  if strategy.feasible > 0 then
    { strategy with delta_lb = strategy.delta_lb /. (float_of_int strategy.feasible) }
  else
    strategy
end

let process_solver optimum (solver:solver) =
  (* Printf.printf "%s/" solver.name; *)
  {solver with strategies=(List.map (process_strategy optimum) solver.strategies)}
let process_instances_set (instances_set : instances_set) =
  (* Printf.printf "%s/" instances_set.name; *)
  { instances_set with
  solvers=(List.map (process_solver instances_set.optimum) instances_set.solvers) }
let process_problem (problem : problem) =
  (* Printf.printf "%s/" problem.name; *)
   { problem with instances_set=(List.map process_instances_set problem.instances_set) }
let process_database db = List.map process_problem db

let print_strategy prob_name instance_name solver_name strategy =
  let total = float_of_int (Hashtbl.length strategy.all) in
  let to_percent x = (float_of_int x) /. total *. 100. in
  Printf.printf "%s, %s, %s, %s, %.2f, %.2f, %.2f, %.2f\n"
    prob_name instance_name solver_name strategy.name
    (to_percent (strategy.feasible + strategy.optimum))
    (to_percent strategy.optimum)
    (to_percent strategy.unsat)
    (strategy.delta_lb *. 100.)

let print_solver prob_name instance_name (solver : solver) =
  List.iter (print_strategy prob_name instance_name solver.name) solver.strategies
let print_instances_set prob_name (instances_set : instances_set) =
  List.iter (print_solver prob_name instances_set.name) instances_set.solvers
let print_problem problem = List.iter (print_instances_set problem.name) problem.instances_set
let print_database db =
  let _ = Printf.printf "problem, instance, solver, strategy, feas, opt, unsat, delta_lb\n" in
  List.iter print_problem db

let clean_split sep data =
  let data = String.split_on_char sep data in
  List.map String.trim data

(* The bound can be a single number, "unsat", "none" or an interval of the form "1..3", "..3" or "1..". *)
let parse_bound time data =
  let raw_bound = clean_split '.' data in
  let raw_bound = List.map String.trim raw_bound in
  if List.length raw_bound = 1 then
    let bound = List.nth raw_bound 0 in
    if String.equal bound "unsat" then Unsat
    else if String.equal bound "none" then Bounded (no_lb, no_ub)
    else
      let b = (int_of_string bound) in
      match time with
      | Some _ -> Bounded (b,b)
      | None -> Bounded (no_lb, b)
  else
    let (lb, ub) = (List.nth raw_bound 0), (List.nth raw_bound 2) in
    let lb =
      if String.length lb = 0 then no_lb
      else (int_of_string lb) in
    let ub =
      if String.length ub = 0 then no_ub
      else (int_of_string ub) in
    Bounded (lb, ub)

let content_of_dir dir =
  let files = Sys.readdir dir in
  Array.sort compare files;
  Array.to_list files

let remove_trailing_slash dir1 =
  let l = (String.length dir1) - 1 in
  if dir1.[l] = Filename.dir_sep.[0] then String.sub dir1 0 l else dir1

let concat_dir dir1 dir2 =
  let dir1 = remove_trailing_slash dir1 in
  dir1 ^ Filename.dir_sep ^ dir2

let subdirs dir =
  List.filter (fun d -> Sys.is_directory (concat_dir dir d)) (content_of_dir dir)
let subfiles dir =
  List.filter (fun d -> not (Sys.is_directory (concat_dir dir d))) (content_of_dir dir)

let rec file_to_lines file =
  let line = bscanf file "%[^\n]" (fun x -> x) in
  try
    if String.length line <> 0 then
      let _ = bscanf file "\n" (fun x -> x) in
      line :: (file_to_lines file)
    else
      []
  with End_of_file -> [line]

let is_digit = function '0' .. '9' -> true | _ -> false
let rec remove_trailing_letters s =
  let len = String.length s in
  if len = 0 then s
  else
    let last = String.get s (len - 1) in
    if is_digit last then s
    else remove_trailing_letters (String.sub s 0 (len - 1))

let parse_time raw_time =
  if String.equal raw_time "timeout" then None
  else
    let raw_time = remove_trailing_letters raw_time in
    Some (float_of_string raw_time)

let parse_csv_line entries line =
  let push_entry instance entry value =
    match entry with
    | "problem" -> {instance with problem=value}
    | "time" -> {instance with time=(parse_time value)}
    | "optimum" -> {instance with bound=(parse_bound instance.time value)}
    | "solutions" -> {instance with solutions=(int_of_string value)}
    | "nodes" -> {instance with nodes=(int_of_string value)}
    | "fails" -> {instance with fails=(int_of_string value)}
    | s -> failwith ("unsupported CSV field `" ^ s ^ "`")
  in
  let tokens = clean_split ',' line in
  let instance = List.fold_left2 push_entry empty_instance entries tokens in
  (* We call `push_entry` two times because some entries such as `optimum` depends on another entry.
     It helps to keep an order-independant parsing of the CSV fields. *)
  List.fold_left2 push_entry instance entries tokens

let parse_csv_header file =
  let header = bscanf file "%[^\n]\n" (fun l -> l) in
  clean_split ',' header

let read_strategy solver_path strategy_file =
  let strat_path = concat_dir solver_path strategy_file in
  let file = Scanning.open_in strat_path in
  begin
    (* CSV header contains the recorded fields. *)
    let entries = parse_csv_header file in
    let all_instances = Hashtbl.create 500 in
    let instances = List.map (parse_csv_line entries) (file_to_lines file) in
    List.iter (fun instance -> Hashtbl.add all_instances instance.problem instance) instances;
    {
      name=strategy_file;
      all=all_instances;
      (* All the following fields are filled when `process_database` is called. *)
      feasible=0;
      optimum=0;
      unsat=0;
      delta_lb=0.;
    }
  end

let read_solver iset_path solver_dir =
  let solver_path = concat_dir iset_path solver_dir in
  let strategy_files = List.filter (fun x -> String.equal (Filename.extension x) ".csv") (subfiles solver_path) in
  { name=solver_dir;
    strategies=(List.map (read_strategy solver_path) strategy_files) }

let parse_name_bound_line line =
  let tokens = clean_split ',' line in
  let bound = parse_bound (Some 0.) (List.nth tokens 1) in
  (List.nth tokens 0, bound)

let read_optimum_file path =
  let opt_path = concat_dir path optimum_file in
  (* Printf.printf "%s\n" opt_path; *)
  let file = Scanning.open_in opt_path in
  begin
    (* Header of CSV to ignore *)
    ignore(bscanf file "%[^\n]\n" (fun _ -> ()));
    let optimum = Hashtbl.create 500 in
    let lines = List.map parse_name_bound_line (file_to_lines file) in
    List.iter (fun (x,y) -> Hashtbl.add optimum x y) lines;
    optimum
  end

let read_instances_set pb_path instances_set_dir : instances_set =
  let iset_path = concat_dir pb_path instances_set_dir in
  let solvers_dir = List.filter
    (fun x -> not (String.equal x optimum_dir)) (subdirs iset_path) in
  { name=instances_set_dir;
    solvers=(List.map (read_solver iset_path) solvers_dir);
    optimum=(read_optimum_file iset_path) }

let read_problem db_dir pb_dir =
  let pb_path = concat_dir db_dir pb_dir in
  { name=pb_dir;
    instances_set=(List.map (read_instances_set pb_path) (subdirs pb_path)) }

let read_database db_dir =
  List.map (read_problem db_dir) (subdirs db_dir)
