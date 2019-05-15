open Absolute_analyzer


type strategy_2 = {
	solver_name : string;
	strategy_name : string;
	all: (string, (float option * optimum)) Hashtbl.t;
	steps : (int*int) list;
}


type instances_set_2 = {
	problem_name : string;
	instance_name : string;
	strategies : strategy_2 list;
}

let count_time_strategy (time : int) strategy =
	let steps = (time, Hashtbl.fold (fun _ y z -> match y with
	None,_ -> z
	|Some(t), _ -> if (int_of_float t) < time then z + 1 else z	
	 ) strategy.all 0)::strategy.steps in
	{strategy with steps = steps}


let count_time instance time =
	{instance with strategies = List.map (count_time_strategy time) instance.strategies}


let exec_step one_step timeout (instance : instances_set_2) = 
	let rec exec_step_rec one_step timeout instance time = 
	if time > timeout then 
		let strategies = List.map (fun x -> {x with steps = List.rev x.steps}) instance.strategies in
		{ instance with strategies = strategies}
	else 
		exec_step_rec one_step timeout (count_time instance time) (time + one_step) 
	in exec_step_rec one_step timeout instance one_step 


let convert_solver (solver : solver) (strategy : strategy) =
	{solver_name = solver.name; strategy_name = strategy.name; all = strategy.all; steps = [];}

(* -> strategy_2 list *)
let append_solvers solvers =
	let rec append_solvers_rec (solvers : solver list) strategies =
	match solvers with 
	[] -> strategies 
	|s::solvers -> let li = List.map (convert_solver s) s.strategies in
	append_solvers_rec solvers (List.append strategies li)
	in append_solvers_rec solvers []


let convert_instance problem (instances_set : instances_set) =
	{problem_name = problem.name; instance_name = instances_set.name; strategies = append_solvers instances_set.solvers}

(*  -> instances_set_temp list *)
let append_problems (database : problem list) = 
	let rec append_problems_rec database instances =
	match database with 
	[] -> instances 
	|p::database -> let li = List.map (convert_instance p) p.instances_set in
	append_problems_rec database (List.append instances li)
in append_problems_rec database []

let print_step step =
	let (time,nb) = step in
	print_string ("			time "^(string_of_int time)^" : "^(string_of_int nb)^"\n")

let print_steps_strategy strategy =
	print_string ("	solver : "^strategy.solver_name^"\n");
	print_string ("		strategy : "^strategy.strategy_name^"\n");
	List.iter print_step strategy.steps

let print_steps_instance instance =
	print_string ("instance : "^instance.instance_name^"\n");
	List.iter print_steps_strategy instance.strategies

let print_steps computed =
	List.iter print_steps_instance computed 

let _ = let timeout = 60 in
		let steps = 10 in 
		let (database : database) = read_database "benchmark/database/" in
    	let (database : database) = process_database database in
    	let database = append_problems database in
    	let one_step = timeout / steps in 
		let computed = List.map (exec_step one_step timeout) database in
		print_steps computed
