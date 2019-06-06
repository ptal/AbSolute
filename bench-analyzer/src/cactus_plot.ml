open Absolute_analyzer

type diff = {
  strategy1 : strategy;
  strategy2 : strategy;
  same : string list;
  s1_better_than_s2 : string list;
  s2_better_than_s1 : string list;  
}

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

let compute_diff strat1 strat2 = 
  let keys = get_keys strat1.all in
  let diff = {strategy1 = strat1; strategy2 = strat2; same = []; s1_better_than_s2 = []; s2_better_than_s1 = [];} in
  List.fold_left (compare_instances strat1 strat2) diff keys 