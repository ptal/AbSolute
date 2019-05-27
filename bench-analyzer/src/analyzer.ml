open Absolute_analyzer

type arg = {
  action : int; (* 0 values / 1 pour comp / 2 pour diff / 3 pour inclusion / 4 pour help*)
  problem : string;
  instance : string;
  solver1 : string;
  solver2 : string;
  strat1 : string;
  strat2 : string;
  timeout : float;
  file : string;
}

type instances_inclusion = {
  strategy1 : strategy;
  strategy2 : strategy;
  inter : string list;
  exter : string list;
  only_s1 : string list;
  only_s2 : string list;
}

type diff = {
  strategy1 : strategy;
  strategy2 : strategy;
  same : string list;
  s1_better_than_s2 : string list;
  s2_better_than_s1 : string list;  
}

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
  comparisons : comp;
  differencies : diff;
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

type split_strategies = {
  strategies : strategy list;
  others : strategy list;
}

type split_solvers = {
  solvers : solver list;
  others : solver list;
}

exception Json of string
exception Arg of string

let print_arg arg =
  match arg with
  |"--help" -> Printf.printf "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n"
        "--help "
        " -option "
        " output : default is strategies statistics"
        " -c comparisons "
        " -d differencies"
        " -p [prefix] problem (default is all problem)"
        " -i [prefix] instance (default is all instances)"
        " -sv [prefix] solver compared to all other solvers (default is all solvers)"
        " -svs [prefix1][prefix2] solver1 compared to solver2 (default is all solvers - all solvers)"
        " -sg [prefix] strategy compared to all other strategies (default is all strategies)" 
        " -sgs [prefix1][prefix2] strategy1 compared to strategy2 (default is all strategies - all strategies)"
  |_ -> print_string "invalid arguments"

let arg_n args =
  let (n,args) = args in
  let arg = Sys.argv.(n) in
  match arg with 
    |"--help" -> (Array.length Sys.argv,{args with action = 4 ;})
    |"-comp"-> (n+1,{args with action = 1;})
    |"-diff"-> (n+1,{args with action = 2;})
    |"-incl"-> (n+1,{args with action = 3;})
    |"-f"->(n+2,{args with file = Sys.argv.(n+1);})
    |"-t"->(n+2,{args with timeout = (float_of_string Sys.argv.(n+1));})
    |"-p"->(n+2,{args with problem = Sys.argv.(n+1);})
    |"-i"->(n+2,{args with instance = Sys.argv.(n+1);})
    |"-sv"->(n+2,{args with solver1 = Sys.argv.(n+1);})
    |"-sg"->(n+2,{args with strat1 = Sys.argv.(n+1);})
    |"-svs"->(n+3,{args with solver1 = Sys.argv.(n+1);solver2 = Sys.argv.(n+2);})
    |"-sgs"->(n+3,{args with strat1 = Sys.argv.(n+1);strat2 = Sys.argv.(n+2);})
    |_ ->(n+1,args)



let rec read_arg args = 
  match args with
  |n,_ when n < Array.length Sys.argv -> read_arg (arg_n args)
  |n,_ when n >= Array.length Sys.argv -> args
  |_ -> failwith ("wrong arguments")

let get_arg argi =
  let (_,args) = read_arg (1,argi) in
  args

let print_args args =
  Printf.printf "%s%u\n%s%s\n%s%s\n%s%s\n%s%s\n%s%s\n%s%s\n"
  "action " args.action "problem " args.problem "instance " args.instance "solver1 " args.solver1 "solver2 " args.solver2 "strat1 " args.strat1 "strat2 " args.strat2

let check_prefix name pre = 
  let prefix_len = String.length pre in
  let prefix_len = min prefix_len (String.length name) in
  let name_prefix = String.sub name 0 prefix_len in
  String.equal name_prefix pre

let filter_strategies args (solver : solver) =
  let strategies = List.filter (fun (x : strategy) -> check_prefix x.name args.strat1 || check_prefix x.name args.strat2) solver.strategies in
  {solver with strategies = strategies;}

let filter_solvers args (instances_set : instances_set) = 
  let solvers = List.filter (fun (x : solver) -> check_prefix x.name args.solver1 || check_prefix x.name args.solver2) instances_set.solvers in
  if String.equal args.strat1 "" || String.equal args.strat2 ""
  then {instances_set with solvers = solvers;} else {instances_set with solvers = List.map (filter_strategies args) solvers}

let filter_instances (args : arg) (problem : problem) =
  let instances_set = List.filter (fun (x : instances_set) -> check_prefix x.name args.instance) problem.instances_set in
  if (String.equal args.solver1 "" || String.equal args.solver2 "")
  then {problem with instances_set = instances_set;} else {problem with instances_set = List.map (filter_solvers args) instances_set;}

let filter_problems (args : arg) database =
  let problems = List.filter (fun (x : problem) -> check_prefix x.name args.problem) database in
  List.map (filter_instances args) problems

(*
let isolate_strategies (strategies : strategy list) args = 
  let rec isolate_strategies_rec (strategies : strategy list) args arg_strategies others = 
    match strategies with 
    |[] -> {strategies = arg_strategies; others = others;}
    |s::strategies -> if (check_prefix s.name args.solver1)
    then isolate_strategies_rec strategies args (s::arg_strategies) others
    else isolate_strategies_rec strategies args arg_strategies (s::others)
  in isolate_strategies_rec strategies args [] []
*)

let isolate_solvers (solvers : solver list) args = 
  let rec isolate_solvers_rec (solvers : solver list) args arg_solvers others = 
    match solvers with 
    |[] -> {solvers = arg_solvers; others = others;}
    |s::solvers -> match (check_prefix s.name args.solver1),(check_prefix s.name args.solver2) with 
             |true,_ -> isolate_solvers_rec solvers args (s::arg_solvers) others
             |false,true -> isolate_solvers_rec solvers args arg_solvers (s::others)
             |false,false -> isolate_solvers_rec solvers args arg_solvers others
  in isolate_solvers_rec solvers args [] []


let add_key key _ li =
  key::li

let get_keys tbl = 
  Hashtbl.fold (add_key) tbl []

let compare_instances strat1 strat2 diff key = 
  let (time1,optimum1) = Hashtbl.find strat1.all key in
  let (time2,optimum2) = Hashtbl.find strat2.all key in 
  let s = diff.same in
  let s1bts2 = diff.s1_better_than_s2 in
  let s2bts1 = diff.s2_better_than_s1 in
  match time1,time2 with
  (* terminé sans timeout donc le résultat est soit optimale soit insatisfiable *)
  |Some(t), Some(t') when t = t'-> begin match optimum1, optimum2 with
                       |Bounded _,Bounded _ -> {diff with same = key::s}
                       |Unsat, Unsat -> {diff with same = key::s}
                       |_,_ -> diff
                  end
  |Some(t), Some(t') when t < t'-> {diff with s1_better_than_s2 = key::s1bts2}
  |Some(t), Some(t') when t > t'-> {diff with s2_better_than_s1 = key::s2bts1}
  (* Le résultat trouvé par la première stratégie est forcément meilleur que celui obtenu par la 2 ème *)
  |Some(_), None -> {diff with s1_better_than_s2 = key::s1bts2}
  |None, Some(_) -> {diff with s2_better_than_s1 = key::s2bts1}
  (* les 2 stratégies se sont terminées avec un timeout, on regarde les bornes supérieure obtenue  *)
  |None,None -> begin match optimum1, optimum2 with
            |Bounded(_,ub), Bounded(_,ub') when ub = ub' -> {diff with same = key::s} (* à discuter : cas no_ub*)
            |Bounded(_,ub), Bounded(_,ub') when ub < ub'->  {diff with s1_better_than_s2 = key::s1bts2}
            |Bounded(_,ub), Bounded(_,ub') when ub > ub'-> {diff with s2_better_than_s1 = key::s2bts1}
            |_,_ -> diff
          end
  |_,_ -> diff

let check_inclusion strat1 strat2 set key =
  let (t,_) = Hashtbl.find strat1.all key in
  let (t',_) = Hashtbl.find strat2.all key in
  let inter = set.inter in 
  let exter = set.exter in 
  let only_s1 = set.only_s1 in
  let only_s2 = set.only_s2 in 
  match t,t' with
  |Some(_),Some(_) -> {set with inter = key::inter}
  |Some(_), None -> {set with only_s1 = key::only_s1}
  |None, Some(_) -> {set with only_s2 = key::only_s2}
  |None, None -> {set with exter = key::exter}

let compute_set strat1 strat2 =
  let keys = get_keys strat1.all in
  let set = {strategy1 = strat1; strategy2 = strat2; inter = []; exter = []; only_s1 = []; only_s2 = [];} in
  List.fold_left (check_inclusion strat1 strat2) set keys 

let compute_diff strat1 strat2 = 
  let keys = get_keys strat1.all in
  let diff = {strategy1 = strat1; strategy2 = strat2; same = []; s1_better_than_s2 = []; s2_better_than_s1 = [];} in
  List.fold_left (compare_instances strat1 strat2) diff keys 

let compute_comp solver_strategy other_strategy = 
  let total1 = float_of_int (Hashtbl.length solver_strategy.all) in
  let total2 = float_of_int (Hashtbl.length other_strategy.all) in
  let to_percent1 x = (float_of_int x) /. total1 *. 100. in
  let to_percent2 x = (float_of_int x) /. total2 *. 100. in
  { delta_feas = ( to_percent1 (solver_strategy.feasible + solver_strategy.optimum) ) -. (to_percent2 (other_strategy.feasible + other_strategy.optimum));
  delta_opt = (to_percent1 solver_strategy.optimum) -. to_percent2 (other_strategy.optimum);
  delta_unsat = (to_percent1 solver_strategy.unsat) -. (to_percent2 other_strategy.unsat);
  delta_lb = other_strategy.delta_lb -. solver_strategy.delta_lb;}

let compare_solver_strategy args solver (other : solver) solver_strategy (other_strategy : strategy) = 
  if check_prefix other_strategy.name args.strat2 then
    {solver = solver; other = other; solver_strategy = solver_strategy; other_strategy = other_strategy; comparisons = (compute_comp solver_strategy other_strategy); differencies = (compute_diff solver_strategy other_strategy); instances_inclusion = (compute_set solver_strategy other_strategy)}
  else 
    let comp = {delta_feas = 0.;delta_opt = 0.; delta_unsat = 0.; delta_lb = 0.;} in
    let diff = {strategy1 = solver_strategy; strategy2 = other_strategy; same = []; s1_better_than_s2 = []; s2_better_than_s1 = [];} in
    let set = {strategy1 = solver_strategy; strategy2 = other_strategy; inter = []; exter = []; only_s1 = []; only_s2 = [];} in
    {solver = solver; other = other; solver_strategy = solver_strategy; other_strategy = other_strategy; comparisons = comp; differencies = diff; instances_inclusion = set}


let compare_strategies args solver other (solver_strategy :strategy) =
  if check_prefix solver_strategy.name args.strat1 then
    {solver = solver; other = other; solver_strategy = solver_strategy; comp_solver_strategies = List.map (compare_solver_strategy args solver other solver_strategy) other.strategies; }
  else  
    {solver = solver; other = other; solver_strategy = solver_strategy; comp_solver_strategies = []; }

let compare_solver args (solver : solver) (other : solver) = 
  if (String.equal solver.name other.name) then {solver = solver; other = other; comp_strategies = []}
  else
  {solver = solver; other = other; comp_strategies = List.map (compare_strategies args solver other) solver.strategies;}

let compare_solver_to_others args (others : solver list) solver = 
  {solver = solver; others = others; comp_solver = List.map (compare_solver args solver) others; }


let compare_instances_set args (instance : instances_set) = 
  if (String.equal args.solver1 "") then 
    {instance = instance;
    comp_solvers = List.map (compare_solver_to_others args instance.solvers) instance.solvers;
    } 
  else
    let sol = isolate_solvers instance.solvers args in
    let solvers = sol.solvers in
    let others = sol.others in
    match solvers,others with
    |[],[] -> failwith "no solvers to compare with these solvers prefixes" 
    |solvers,others -> {instance = instance; comp_solvers = List.map (compare_solver_to_others args others) solvers;} 

let compare_problem args  problem = 
  {problem = problem; comp_instances = List.map (compare_instances_set args) problem.instances_set }

let print_instances instance = 
  print_string ("         "^instance);
  print_string "\n"

let print_diff diff = 
  Printf.printf "%s%s%s%s\n%s%s%u\n%s%s%u\n%s%s%u\n%s%s%u\n"
  (""^"(") diff.strategy1.name " - " (diff.strategy2.name^")") 
  ("        ") "same : " (List.length diff.same)
  ("        ") "s1 better than s2 : " (List.length diff.s1_better_than_s2)
  ("        ") "s2 better than s1 : " (List.length diff.s2_better_than_s1)
  ("        ") "sum : " ((List.length diff.same)+(List.length diff.s1_better_than_s2)+(List.length diff.s2_better_than_s1));
  print_string "\n"
  (*List.iter print_instances diff.s1_better_than_s2*)

let print_set (instances_inclusion : instances_inclusion) =
  Printf.printf "%s%s%s%s\n%s%s%u\n%s%s%u\n%s%s%u\n%s%s%u\n%s%s%u\n"
  (""^"(") instances_inclusion.strategy1.name " - " (instances_inclusion.strategy2.name^")") 
  ("        ") "inter : " (List.length instances_inclusion.inter)
  ("        ") "exter : " (List.length instances_inclusion.exter)
  ("        ") "only s1 : " (List.length instances_inclusion.only_s1)
  ("        ") "only s2 : " (List.length instances_inclusion.only_s2)
  ("        ") "sum : " ((List.length instances_inclusion.inter)+(List.length instances_inclusion.exter)+(List.length instances_inclusion.only_s1)+(List.length instances_inclusion.only_s2));
  print_string "\n"

let print_comp_solver_strategy print (comp_solver_strategy : comp_solver_strategy) =
  match print with
  |1->Printf.printf "%s\n%s%s%s%s\n%s%2f%s\n%s%2f%s\n%s%2f%s\n%s%2f%s\n\n"
  "     strategy1 - strategy2"
  "     " comp_solver_strategy.solver_strategy.name " - " comp_solver_strategy.other_strategy.name 
  "       delta feas : " comp_solver_strategy.comparisons.delta_feas" %"
  "       delta opt : " comp_solver_strategy.comparisons.delta_opt" %"
  "       delta unsat : " comp_solver_strategy.comparisons.delta_unsat" %"
  "       delta lb : " (comp_solver_strategy.comparisons.delta_lb*.100.) " %"
  |2->print_diff comp_solver_strategy.differencies
  |3->print_set comp_solver_strategy.instances_inclusion
  |_->()

let print_comp_strategies print (comp_strategies : comp_strategies) =
  List.iter (print_comp_solver_strategy print) comp_strategies.comp_solver_strategies

let print_comp_solver print (comp_solver : comp_solver) = 
  let solver1 = comp_solver.solver in
  let solver2 = comp_solver.other in
  if String.equal solver1.name solver2.name then 
    List.iter (print_comp_strategies print) comp_solver.comp_strategies
  else
    Printf.printf "%s\n%s%s%s%s\n"
    "   solver1 - solver2 :"
    "   " solver1.name " - " solver2.name ;
    List.iter (print_comp_strategies print) comp_solver.comp_strategies

let print_comp_solver print (comp_solver : comp_solver_to_others) = 
  List.iter (print_comp_solver print) comp_solver.comp_solver

let print_comp_instance print (comp_instance : comp_instance) = 
  Printf.printf "%s%s%s"
  " Instance : " comp_instance.instance.name "\n";
  List.iter (print_comp_solver print) comp_instance.comp_solvers

let print_comp_problem print (comp_problem : comp_problem)  = 
  Printf.printf "%s%s%s"
  "Problem : " comp_problem.problem.name "\n";
  List.iter (print_comp_instance print) comp_problem.comp_instances

let print_comp_database comp_database print= 
  List.iter (print_comp_problem print) comp_database 

(*
let instances_to_json args instance =


let to_json args database =
  let instances = " " in
  "{\"name\":\"cactus_plot\",\"instances\":"^instances^"}"*)

let check_file file =
  if not (Sys.file_exists file) then 
  begin
    let cmd = "touch "^file in
    let code = Sys.command cmd in
    if not (code = 0) then 
      raise (Sys_error ("shell error :"^(string_of_int code)^"\n"))
  end
  else 
  begin
    print_string (file^" content will be overwrite, continue ? [Y/n]");
    let arg = read_line () in
    if not (String.equal arg "Y") then
      failwith ("execution stopped")
  end

let check_ext file =
  let len = String.length file in
  let ext_json = String.sub file (len-5) 5 in
  if String.equal ext_json ".json" then
    check_file file
  else
    raise (Json "not a json file")

let file_to_open args =
  match args.action with
  |1 -> ""
  |2 -> ""
  |3 -> "bench-analyzer/src/instances_inclusion/data/"^args.file
  |_ -> failwith "nothing to export"

let write_in_file args json =
  let file = file_to_open args in
  check_ext file;
  let oc = open_out file in   
  Printf.fprintf oc "%s\n" json;     
  close_out oc

let remove_last_char chars =
  String.sub chars 0 (String.length chars -1)

let json_name args =
  match args.action with 
  |1 -> "\"Instances differencie\""
  |2 -> "\"Instances comparison\""
  |3 -> "\"Instances inclusion\""
  |_ -> ""

let to_json_solver_strategy args problem (instance : instances_set) all (comp_solver_strategy : comp_solver_strategy) =
  let prob = "\""^problem.name^"\"" in
  let inst = "\""^instance.name^"\"" in
  let solver1 = "\""^comp_solver_strategy.solver.name^"\"" in
  let solver2 = "\""^comp_solver_strategy.other.name^"\"" in
  let strat1 = "\""^comp_solver_strategy.solver_strategy.name^"\"" in
  let strat2 = "\""^comp_solver_strategy.other_strategy.name^"\"" in

  match args.action with 
  |1 -> ""
  |2 -> ""
  |3 -> let set = comp_solver_strategy.instances_inclusion in
      let labels = "[\"inter\",\"exter\",\"only_1st_solver \",\"only_2nd_solver\"]" in
      let data = "["^(string_of_int (List.length set.inter))^","^(string_of_int (List.length set.exter))^","^(string_of_int (List.length set.only_s1))^","^(string_of_int (List.length set.only_s2))^"]"in 
  all^("{\"problem\":"^prob^",\"instance\":"^inst^",\"solver1\":"^solver1^",\"solver2\":"^solver2^",\"strat1\":"^strat1^",\"strat2\":"^strat2^",\"labels\":"^labels^",\"data\":"^data^"},")
  |_ -> ""

(*let to_json_solver_strategies args problem instance all comp_solver_strategies =
  List.fold_left (to_json_solver_strategy args problem instance) all comp_solver_strategies.comp_solver_strategies*)
let to_json_strategies args problem instance all comp_strategies =
  List.fold_left (to_json_solver_strategy args problem instance) all comp_strategies.comp_solver_strategies

let to_json_solver args problem instance all comp_solver =
  List.fold_left (to_json_strategies args problem instance) all comp_solver.comp_strategies

let to_json_solvers args problem instance all comp_solvers =
  List.fold_left (to_json_solver args problem instance) all comp_solvers.comp_solver

let to_json_instances_set args problem all comp_instances=
  List.fold_left (to_json_solvers args problem comp_instances.instance) all comp_instances.comp_solvers

let to_json_problem args all comp_problem =
  List.fold_left (to_json_instances_set args comp_problem.problem) all comp_problem.comp_instances

let to_json_instances comp_database args =
  (*comp_database : comp_problem list*)
  List.fold_left (to_json_problem args) "" comp_database

let to_json comp_database args = 
  let json = "{\"name\":"^(json_name args)^",\"timeout\":"^(remove_last_char (string_of_float args.timeout))^",\"instances\":["^(remove_last_char (to_json_instances comp_database args))^"]}" in
  write_in_file args json

let exec args database =
  match args.action with
  |0 -> print_database database
  |1 -> let comp_database = List.map (compare_problem args) database in
        print_comp_database comp_database 1
  |2 -> let comp_database = List.map (compare_problem args ) database in
        print_comp_database comp_database 2
  |3 -> let comp_database = List.map (compare_problem args) database in
      print_comp_database comp_database 3;
      to_json comp_database args 
  |_ -> ()


let _ = let argi = {action = 0; problem = ""; instance = ""; solver1 = ""; solver2 = ""; strat1 = ""; strat2 = ""; timeout = 60.; file = "arg.json"} in
  let args = get_arg argi in
  print_args args;
  print_string "******\n";
  let (database : database) = read_database "benchmark/example/" in
  let (database : database) = process_database database in
  let database = filter_problems args database in
  exec args database
      