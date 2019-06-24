
module type Boolean_rep_sig =
sig
  type t
  type var_kind = unit
  type var_id = var
  type rconstraint = (Lit.t Vec.t) list
  val empty: t
  val extend: t -> (Csp.var * var_id) -> t
  val to_logic_var: t -> var_id -> var
  val to_abstract_var: t -> var -> var_id
  val rewrite: t -> bformula -> rconstraint list
  val relax: t -> bformula -> rconstraint list
  val negate: rconstraint -> rconstraint
end

(* Replace `Equiv` and `Imply` with their logical equivalent using `And` and `Or`.
   NOTE: It duplicates constraints if they occur in `<=>`. *)
let eliminate_imply_and_equiv formula =
  let rec aux = function
    | Cmp _ as c
    | BVar _ as c -> c
    | Equiv (f1,f2) ->
        let f1 = aux f1 in
        let f2 = aux f2 in
        And (Or (f1, Not f2), Or (Not f1, f2))
    | Imply (f1,f2) -> Or (Not (aux f1), aux f2)
    | And (f1,f2) -> And (aux f1, aux f2)
    | Or (f1,f2) -> Or (aux f1, aux f2)
    | Not f -> Not (aux f) in
  aux formula

(* Move logical negation inwards the formula by De Morgan's Law, e.g. not (a \/ b) --> (not a /\ not b).
   It supposes `eliminate_imply_and_equiv` has already been applied. *)
let move_not_inwards formula =
  let rec aux neg formula =
    if neg then
      match formula with
      | Cmp (op, e1, e2) -> Cmp (Csp.neg op,e1,e2)
      | BVar f -> Not (BVar f)
      | And (f1,f2) -> Or (aux true f1, aux true f2)
      | Or (f1,f2) -> And (aux true f1, aux true f2)
      | Not f -> aux false f
      | Equiv _ | Imply _ -> failwith "`move_not_inwards` must be called after `eliminate_imply_and_equiv`."
    else
      match formula with
      | Cmp _as c | BVar _ as c -> c
      | And (f1,f2) -> And (aux false f1, aux false f2)
      | Or (f1,f2) -> Or (aux false f1, aux false f2)
      | Not f -> aux true f
      | Equiv _ | Imply _ -> failwith "`move_not_inwards` must be called after `eliminate_imply_and_equiv`." in
  aux false formula

(* Distribute `Or` over `And`: a \/ (b /\ c) --> (a \/ b) /\ (a \/ c). *)
let distribute_or formula =
  let rec one_round = function
    | Cmp _ as c, false
    | BVar _ as c -> c, false
    | Or (f1, And (f2, f3)) -> And(Or(f1,f2), Or(f1,f3)), true
    | Or (And(f1,f2), f3) -> aux Or(f3, And(f1,f2))
    | And (f1,f2) ->
        let (f1, t1) = aux f1 in
        let (f2, t2) = aux f2 in
        And (f1, f2), (t1 || t2)
    | Or (f1,f2) ->
        let (f1, t1) = aux f1 in
        let (f2, t2) = aux f2 in
        Or (f1, f2), (t1 || t2)
    | Not f -> Not (aux f)
    | Equiv _ | Imply _ -> failwith "`distribute_or` must be called after `eliminate_imply_and_equiv`." in
  let rec aux (has_changed, formula) =
    if has_changed then aux (one_round formula)
    else formula in
  aux (true, formula)

(* Naive conversion of a Boolean formula to a conjunctive normal form (CNF). *)
let formula_to_cnf formula =
  let formula = eliminate_imply_and_equiv formula in
  let formula = move_not_inwards formula in
  distribute_or formula

module Boolean_rep =
struct
  type var_kind = unit
  type var_id = var
  type rconstraint = (Lit.t Vec.t) list

  module Env = Tools.VarMap
  module REnv = Mapext.Make(struct
    type t=var_id
    let compare = compare end)
  type t = {
    (* maps each variable name to its store index. *)
    env: var_id Env.t;
    (* reversed mapping of `env`. *)
    renv: var REnv.t;
  }

  let empty = {env=Env.empty; renv=REnv.empty}
  let extend repr (v,idx) = {
    env=(Env.add v idx repr.env);
    renv=(REnv.add idx v repr.renv);
  }
  let to_logic_var repr idx = REnv.find idx repr.renv
  let to_abstract_var repr v = Env.find v repr.env

  let rewrite repr formula =
    let cnf = formula_to_cnf formula in


  let relax = rewrite
  let negate = function
end
