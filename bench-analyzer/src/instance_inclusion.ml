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
