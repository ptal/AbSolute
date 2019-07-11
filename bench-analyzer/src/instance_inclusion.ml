open Absolute_analyzer
open Analyzer_all

type instances_inclusion = {
  strategy1 : strategy;
  strategy2 : strategy;
  inter : string list;
  exter : string list;
  only_s1 : string list;
  only_s2 : string list;
}

let check_inclusion (strat1 : strategy) (strat2 : strategy) set key =
  let instance = Hashtbl.find strat1.all key in
  let instance' = Hashtbl.find strat2.all key in
  let inter = set.inter in
  let exter = set.exter in
  let only_s1 = set.only_s1 in
  let only_s2 = set.only_s2 in
  match instance.time,instance'.time with
  | Some(_),Some(_) -> {set with inter = key::inter}
  | Some(_), None -> {set with only_s1 = key::only_s1}
  | None, Some(_) -> {set with only_s2 = key::only_s2}
  | None, None -> {set with exter = key::exter}

let compute_set (strat1 : strategy) (strat2 : strategy) =
  let keys = get_keys strat1.all in
  let set = {strategy1 = strat1; strategy2 = strat2; inter = []; exter = []; only_s1 = []; only_s2 = [];} in
  List.fold_left (check_inclusion strat1 strat2) set keys


module NameSet = Set.Make(String)
module StatCounter = Map.Make(NameSet)

let compute_one_problem instance_name (instances_set:instances_set) =
  let by_strategy solver_name (strat:strategy) =
    let instance = Hashtbl.find strat.all instance_name in
    match instance.time with
    | Some _ -> [solver_name ^ "-" ^ strat.name]
    | None -> [] in
  let by_solver (solver:solver) =
    List.flatten (List.map (by_strategy solver.name) solver.strategies) in
  let all_strats = List.flatten (List.map by_solver instances_set.solvers) in
  NameSet.of_list all_strats

(* For a specific set of instances, we compute the distribution of the problems among strategies.
   For example, if inclusions({strat1,strat2}) = 2, it means that strat1 and strat2 are the only strategies that solve these 2 problems. *)
let compute_inclusions instances_set =
  let inc_counter name _ counter =
    let strat_set = compute_one_problem name instances_set in
    StatCounter.update strat_set (
      fun value -> match value with
        | Some x -> Some (x + 1)
        | None -> Some (1)) counter in
  Hashtbl.fold inc_counter instances_set.optimum StatCounter.empty

let count_strats (instances_set:instances_set) =
  let by_solver acc (solver:solver) = acc + List.length solver.strategies in
  List.fold_left by_solver 0 instances_set.solvers

let print_inclusion_stats problem_name instances_set =
  let num_strats = count_strats instances_set in
  let stats = compute_inclusions instances_set in
  let all = StatCounter.bindings stats in
  Printf.printf "\n%s - %s\n" problem_name instances_set.name;
  List.iter (fun (names, count) ->
    if NameSet.is_empty names then
      Printf.printf "none: %d\n" count
    else if NameSet.cardinal names = num_strats then
      Printf.printf "all: %d\n" count
    else
    begin
      List.iter (Printf.printf "%s ") (NameSet.elements names);
      Printf.printf ": %d\n" count
    end) all

let print_inclusion_stats_of_database database =
  let by_problem problem =
    List.iter (print_inclusion_stats problem.name) problem.instances_set in
  List.iter by_problem database
