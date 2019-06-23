open Csp

type box_var = int
type box_constraint = box_var gbconstraint
type box_expr = box_var gexpr

module type Box_rep_sig =
sig
  type t
  type var_kind = unit
  type var_id = box_var
  type rconstraint = box_constraint
  val empty: t
  val extend: t -> (Csp.var * var_id) -> t
  val to_logic_var: t -> var_id -> var
  val to_abstract_var: t -> var -> var_id
  val rewrite: t -> bconstraint -> rconstraint list
  val relax: t -> bconstraint -> rconstraint list
  val negate: rconstraint -> rconstraint
end

module Box_rep =
struct
  type var_kind = unit
  type var_id = box_var
  type rconstraint = box_constraint
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

  let rewrite_expr repr e : var_id gexpr =
    let rec aux = function
    | Funcall(x, exprs) -> Funcall(x, List.map aux exprs)
    | Unary(LNOT, e) -> Unary(LNOT, aux e)
    | Unary(NEG, e) -> Unary(NEG, aux e)
    | Binary(op, e1, e2) -> Binary (op, aux e1, aux e2)
    | Var(x) -> Var(to_abstract_var repr x)
    | Cst(v, a) -> Cst(v, a) in
    aux e

  let rewrite repr (e1,op,e2) = [(rewrite_expr repr e1, op, rewrite_expr repr e2)]

  let relax = rewrite
  let negate (e1,op,e2) = (e1,neg op,e2)
end
