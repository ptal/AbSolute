open Absolute_analyzer

type instances_inclusion = {
  strategy1 : strategy;
  strategy2 : strategy;
  inter : string list;
  exter : string list;
  only_s1 : string list;
  only_s2 : string list;
}

let add_key key _ li =
  key::li

let get_keys tbl = 
  Hashtbl.fold (add_key) tbl []

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