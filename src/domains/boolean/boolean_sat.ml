open Satml

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

let eliminate_imply_and_equiv formula =
  let rec aux = function
  | Cmp (op,e1,e2) as c -> c
  | Equiv (b1,b2) ->
      let b1 = aux b1 in
      let b2 = aux b2 in
      And (Or (b1, Not b2), Or (Not b1, b2))
  | Imply (b1,b2) -> Or (Not (aux b1), aux b2)
  | And (b1,b2) -> And (aux b1, aux b2)
  | Or (b1,b2) -> Or (aux b1, aux b2)
  | Not b -> Not (aux b)

let formula_to_cnf formula =
  let formula = eliminate_imply_and_equiv formula in
  let formula = move_NOT_inwards formula in
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

  let rewrite repr f =

  let relax = rewrite
  let negate = function
end
