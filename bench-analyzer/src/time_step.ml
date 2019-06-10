open Analyzer_all
open Absolute_analyzer

let nb_steps = 30
let one_step = timeout /. (float_of_int nb_steps)

let count_time_strategy (time : float) (strategy : strategy_2) =
  let num_solved_before_time = Hashtbl.fold (fun _ instance z ->
    match instance.time with
    | None -> z
    | Some(t) -> if t < time then z + 1 else z ) strategy.all 0 in
  let steps = (time, num_solved_before_time)::strategy.steps in
  {strategy with steps = steps}

let count_time (instance : instances_set_2) time =
  {instance with strategies = List.map (count_time_strategy time) instance.strategies}

let exec_step one_step timeout (instance : instances_set_2) =
  let rec aux one_step timeout (instance : instances_set_2) time =
    if time > timeout then
      let strategies = List.map (fun x -> {x with steps = List.rev x.steps}) instance.strategies in
      { instance with strategies = strategies}
    else
      aux one_step timeout (count_time instance time) (time +. one_step)
  in aux one_step timeout instance one_step

let print_step step =
  let (time,nb) = step in
  print_string ("     time "^(string_of_float time)^" : "^(string_of_int nb)^"\n")

let print_steps_strategy strategy =
  print_string (" solver : "^strategy.solver_name^"\n");
  print_string ("   strategy : "^strategy.strategy_name^"\n");
  List.iter print_step strategy.steps

let print_steps_instance instance =
  print_string ("instance : "^instance.instance_name^"\n");
  List.iter print_steps_strategy instance.strategies

let print_steps computed =
  List.iter print_steps_instance computed

let steps_to_time one_step steps =
  let rec steps_to_time_rec one_step steps step time =
  match steps with
  1 -> "[0.0,"^(time^(string_of_float step)^"0")^"]"
  |_ -> let time = time^(string_of_float step)^"0," in steps_to_time_rec one_step (steps-1) (step+.one_step) time
in steps_to_time_rec one_step steps one_step ""

let steps_to_string steps =
  let rec steps_to_string_rec steps str =
  match steps with
  [] -> "[0"^str^"]"
  |(_,s)::steps -> let str = str^","^(string_of_int s) in steps_to_string_rec steps str
in steps_to_string_rec steps ""

let strategy_to_json_string str strategy =
  let steps = steps_to_string strategy.steps in
  let json = "{\"solver\":\""^strategy.solver_name^"\",\"strategy\":\""^strategy.strategy_name^"\",\"steps\":"^steps^"}," in
  str^json

let instances_to_json_string timeout steps str (instance : instances_set_2) =
  let one_step = timeout /. (float_of_int steps) in
  let time = steps_to_time one_step steps in
  let strategies = List.fold_left strategy_to_json_string "" instance.strategies in
  let strategies = (String.sub strategies 0 (String.length strategies -1))  in
  let json = "{\"problem\":\""^instance.problem_name^"\",\"instance\":\""^instance.instance_name^"\",\"nb_instances\":"^(string_of_int instance.nb_instances)^",\"time\":"^time^",\"strategies\":["^strategies^"]}," in
  str^json

let database_to_json_string database timeout steps =
  let name = "{\"name\":\"Time Step\",\"timeout\":"^(string_of_float timeout)^"0,\"steps\":"^(string_of_int steps)^"," in
  let instances = (List.fold_left (instances_to_json_string timeout steps) ("") database) in
  let instances = (String.sub instances 0 (String.length instances -1))  in
  let json = "\"instances\":["^instances^"]}" in
  name^json
