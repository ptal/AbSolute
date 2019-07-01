open Csp
open Vardom_sig

type box_var = int

module type Box_rep_functor = functor (Vardom: Vardom_sig) ->
sig
  type t
  type var_kind = unit
  type var_id = box_var
  type rexpr = node * Vardom.t ref
  and node =
    | BFuncall of string * node list
    | BUnary   of unop * node
    | BBinary  of binop * node * node
    | BVar     of var_id
    | BCst     of Vardom.t
  type rconstraint = rexpr * cmpop * rexpr
  val empty: t
  val extend: t -> (Csp.var * var_id) -> t
  val to_logic_var: t -> var_id -> var
  val to_abstract_var: t -> var -> var_id
  val rewrite: t -> bconstraint -> rconstraint list
  val relax: t -> bconstraint -> rconstraint list
  val negate: rconstraint -> rconstraint
end

module Box_rep = functor (Vardom: Vardom_sig) ->
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

  let rewrite_expr repr e : rexpr =
    let rec aux e =
      let e = match e with
        | Funcall(x, exprs) -> BFuncall(x, List.map aux exprs)
        | Unary(NEG, e) -> BUnary(NEG, aux e)
        | Binary(op, e1, e2) -> BBinary (op, aux e1, aux e2)
        | Var(x) -> BVar(to_abstract_var repr x)
        | Cst(v, a) -> BCst(Vardom.create (Vardom.OF_RAT c), a) in
        (e, ref Vardom.create Vardom.TOP)
    aux e

  let rewrite repr (e1,op,e2) = [(rewrite_expr repr e1, op, rewrite_expr repr e2)]

  let relax = rewrite
  let negate (e1,op,e2) = (e1,neg op,e2)
end
