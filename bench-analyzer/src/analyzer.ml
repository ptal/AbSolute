open Absolute_analyzer
open Instance_inclusion
open Cactus_plot
open Time_step
open Analyzer_all

type comp = {
  delta_feas : float;
  delta_opt : float;
  delta_unsat : float;
  delta_lb : float;
}

type comp_solver_strategy = {
  solver : solver;
  other : solver;
  solver_strategy : strategy;
  other_strategy : strategy;
  instances_inclusion : instances_inclusion;
}

type comp_strategies = {
  solver : solver;
  other : solver;
  solver_strategy : strategy;
  comp_solver_strategies : comp_solver_strategy list;
}

type comp_solver = {
  solver : solver;
  other : solver;
  comp_strategies : comp_strategies list;
}

type comp_solver_to_others = {
  solver : solver;
  others : solver list;
  comp_solver : comp_solver list;
}

type comp_instance = {
  instance : instances_set;
  comp_solvers : comp_solver_to_others list
}

type comp_problem = {
  problem : problem;
  comp_instances : comp_instance list;
}

type split_solvers = {
  solvers : solver list;
  others : solver list;
}

let compare_solver_strategy solver (other : solver) solver_strategy (other_strategy : strategy) =
  {solver = solver; other = other; solver_strategy = solver_strategy; other_strategy = other_strategy; instances_inclusion = (compute_set solver_strategy other_strategy)}

let compare_strategies solver other (solver_strategy :strategy) =
    {solver = solver; other = other; solver_strategy = solver_strategy; comp_solver_strategies = List.map (compare_solver_strategy solver other solver_strategy) other.strategies; }

let compare_solver (solver : solver) (other : solver) =
  if (solver.name <= other.name) then {solver = solver; other = other; comp_strategies = []}
  else
  {solver = solver; other = other; comp_strategies = List.map (compare_strategies solver other) solver.strategies;}

let compare_solver_to_others (others : solver list) solver =
  {solver = solver; others = others; comp_solver = List.map (compare_solver solver) others; }

let compare_instances_set (instance : instances_set) =
    {instance = instance;
    comp_solvers = List.map (compare_solver_to_others instance.solvers) instance.solvers;
    }

let compare_problem problem =
  {problem = problem; comp_instances = List.map (compare_instances_set) problem.instances_set }

let remove_last_char chars =
  let size = String.length chars in
  if size > 0 then
    String.sub chars 0 ((String.length chars) -1)
  else chars

let json_name nb =
  match nb with
  |1 -> "\"Instances inclusion\""
  |2 -> "\"Cactus plot\""
  |_ -> ""

  let to_json_solver_strategy nb problem (instance : instances_set) all (comp_solver_strategy : comp_solver_strategy) =
  let prob = "\""^problem.name^"\"" in
  let inst = "\""^instance.name^"\"" in
  let solver1 = "\""^comp_solver_strategy.solver.name^"\"" in
  let solver2 = "\""^comp_solver_strategy.other.name^"\"" in
  let strat1 = "\""^comp_solver_strategy.solver_strategy.name^"\"" in
  let strat2 = "\""^comp_solver_strategy.other_strategy.name^"\"" in
  match nb with
  |1 -> let set = comp_solver_strategy.instances_inclusion in
      let labels = "[\"inter\",\"exter\",\"only "^comp_solver_strategy.solver.name^" with "^comp_solver_strategy.solver_strategy.name^ "\",\"only "^comp_solver_strategy.other.name^" with "^comp_solver_strategy.other_strategy.name^" \"]" in
      let data = "["^(string_of_int (List.length set.inter))^","^(string_of_int (List.length set.exter))^","^(string_of_int (List.length set.only_s1))^","^(string_of_int (List.length set.only_s2))^"]"in
  all^("{\"problem\":"^prob^",\"instance\":"^inst^",\"solver1\":"^solver1^",\"solver2\":"^solver2^",\"strat1\":"^strat1^",\"strat2\":"^strat2^",\"labels\":"^labels^",\"data\":"^data^"},")
  |2 -> let points = hash_to_json_cactus comp_solver_strategy.solver_strategy comp_solver_strategy.other_strategy in
  all^("{\"problem\":"^prob^",\"instance\":"^inst^",\"solver1\":"^solver1^",\"solver2\":"^solver2^",\"strat1\":"^strat1^",\"strat2\":"^strat2^",\"points\":"^points^"},")
  |_ -> ""

let to_json_strategies nb problem instance all comp_strategies =
  List.fold_left (to_json_solver_strategy nb problem instance) all comp_strategies.comp_solver_strategies

let to_json_solver nb problem instance all comp_solver =
  List.fold_left (to_json_strategies nb problem instance) all comp_solver.comp_strategies

let to_json_solvers nb problem instance all comp_solvers =
  List.fold_left (to_json_solver nb problem instance) all comp_solvers.comp_solver

let to_json_instances_set nb problem all comp_instances=
  List.fold_left (to_json_solvers nb problem comp_instances.instance) all comp_instances.comp_solvers

let to_json_problem nb all comp_problem =
  List.fold_left (to_json_instances_set nb comp_problem.problem) all comp_problem.comp_instances

let to_json_comp_database comp_database =
  (*comp_database : comp_problem list*)
  let incl = ("{\"name\":"^(json_name 1)^",\"timeout\":"^((string_of_float timeout)^"0")^",\"instances\":["^(remove_last_char (List.fold_left (to_json_problem 1) "" comp_database))^"]}") in
  let cactus = ("{\"name\":"^(json_name 2)^",\"timeout\":"^((string_of_float timeout)^"0")^",\"instances\":["^(remove_last_char (List.fold_left (to_json_problem 2) "" comp_database))^"]}") in
  (incl^","^cactus)

let to_json_database database =
  let comp_database = List.map compare_problem database in
  let json_1 = to_json_comp_database comp_database in
  let database = append_problems database in
  let computed = List.map (exec_step one_step timeout) database in
  let json_2 = database_to_json_string computed 60. 30 in
  ("{\"analyses\":["^json_1^","^json_2^"]}")

let _ =
  Printexc.record_backtrace true;
  try
    let (database : database) = read_database "benchmark/database-ccipl/" in
    let (database : database) = process_database database in
    (* print_inclusion_stats_of_database database *)
    print_string (to_json_database database)
  with e ->
  begin
    Printexc.print_backtrace stdout;
    Printf.printf "Exception: %s\n" (Printexc.to_string e);
  end