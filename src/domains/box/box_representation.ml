open Csp
open Vardom_sig
open Var_store

module type Box_rep_sig =
sig
  module Vardom: Vardom_sig
  module Store: Var_store_sig with module V=Vardom

  type t
  type var_kind = unit
  type var_id = Store.key
  type var_dom = Store.cell

  type rexpr = {
    node: node;
    mutable value: Vardom.t
  }
  and node =
    | BFuncall of string * rexpr list
    | BUnary   of unop * rexpr
    | BBinary  of binop * rexpr * rexpr
    | BVar     of var_id
    | BCst     of Vardom.t

  type rconstraint = rexpr * cmpop * rexpr

  val empty: t
  val extend: t -> (Csp.var * var_id) -> t
  val to_logic_var: t -> var_id -> var
  val to_abstract_var: t -> var -> var_id
  val rewrite: t -> bformula -> rconstraint list
  val relax: t -> bformula -> rconstraint list
  val negate: rconstraint -> rconstraint
  val to_logic_constraint: t -> rconstraint -> bconstraint
  val make_expr: node -> rexpr
  val vars_of_constraint: rconstraint -> var_id list
end

module type Box_rep_functor = functor (Vardom: Vardom_sig) -> Box_rep_sig
  with module Vardom=Vardom

module Box_rep = functor (Vardom: Vardom_sig) ->
struct
  module Vardom = Vardom
  module Store = Var_store.Make(Vardom)

  type var_kind = unit
  type var_id = Store.key
  type var_dom = Store.cell

  type rexpr = {
    node: node;
    mutable value: Vardom.t
  }
  and node =
    | BFuncall of string * rexpr list
    | BUnary   of unop * rexpr
    | BBinary  of binop * rexpr * rexpr
    | BVar     of var_id
    | BCst     of Vardom.t

  type rconstraint = rexpr * cmpop * rexpr

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

  let make_expr e = { node=e; value=Vardom.create Vardom.TOP }

  let rewrite_expr repr e : rexpr =
    let rec aux e : rexpr =
      let e = match e with
        | Funcall(x, exprs) -> BFuncall(x, List.map aux exprs)
        | Unary(NEG, e) -> BUnary(NEG, aux e)
        | Binary(op, e1, e2) -> BBinary (op, aux e1, aux e2)
        | Var(x) -> BVar(to_abstract_var repr x)
        | Cst(v, _) -> BCst(Vardom.create (Vardom.OF_RAT v)) in
      make_expr e in
    aux e

  let rewrite repr f =
    let rewrite_one (e1, op, e2) = [(rewrite_expr repr e1, op, rewrite_expr repr e2)] in
    try mapfold_conjunction rewrite_one f
    with Wrong_modelling _ -> []

  let relax = rewrite

  let negate (e1,op,e2) = (e1,neg op,e2)

  let to_logic_expr repr expr =
    let rec aux expr =
      match expr.node with
      | BCst v -> Cst (fst (Vardom.to_rational_range v), Vardom.to_annot v)
      | BVar x -> Var(to_logic_var repr x)
      | BUnary (op, e) -> Unary(op, aux e)
      | BBinary (op, e1, e2) -> Binary(op, aux e1, aux e2)
      | BFuncall (x,args) -> Funcall(x, List.map aux args) in
    aux expr

  let to_logic_constraint repr (e1,op,e2) =
    (to_logic_expr repr e1, op, to_logic_expr repr e2)

  let rec vars_of_expr expr =
    match expr.node with
    | BCst _ -> []
    | BVar v -> [v]
    | BUnary (_, e) -> vars_of_expr e
    | BBinary (_, e1, e2) -> (vars_of_expr e1)@(vars_of_expr e2)
    | BFuncall (_,args) -> List.concat (List.map vars_of_expr args)

  let vars_of_constraint (e1,_,e2) = (vars_of_expr e1)@(vars_of_expr e2)
end
