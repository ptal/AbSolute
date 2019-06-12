open Absolute_analyzer
open Analyzer_all

let hash_to_json_cactus (strat1 : strategy) (strat2 : strategy) =
  let rec hash_to_json_cactus_rec (strat1 : strategy) (strat2 : strategy) keys times =
  match keys with
  | [] -> "["^(remove_last_char times)^"]"
  | k::keys ->
    let instance = (Hashtbl.find strat1.all k) in
    let instance' = (Hashtbl.find strat2.all k) in
    let (t,t') = ((float_option_to_string instance.time),(float_option_to_string instance'.time)) in
    let time = "{"^"\"x\":"^t^",\"y\":"^t'^"}," in
    hash_to_json_cactus_rec strat1 strat2 keys (times^time)
  in hash_to_json_cactus_rec strat1 strat2 (get_keys strat1.all) ""
