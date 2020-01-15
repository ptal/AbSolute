(* Copyright 2019 AbSolute Team

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)


open Core
open Domains.Interpretation
open Typing.Tast
open Lang.Ast
open Vardom.Vardom_sig
open Var_store

module type Box_interpretation_sig =
sig
  module Vardom: Vardom_sig
  module Store: Var_store_sig with module V=Vardom

  include module type of (Interpretation_base(struct type var_id=Store.key end))

  type var_dom = Store.cell

  type rexpr = {
    node: node;
    mutable value: Vardom.t
  }
  and node =
    | BFuncall of string * rexpr list
    | BUnary   of unop * rexpr
    | BBinary  of rexpr * binop * rexpr
    | BVar     of var_id
    | BCst     of Vardom.t * Types.var_abstract_ty

  type rconstraint = rexpr * cmpop * rexpr

  val interpret: t -> approx_kind -> tformula -> t * rconstraint list
  val to_qformula: t -> rconstraint list -> tqformula

  val make_expr: node -> rexpr
  val vars_of_constraint: rconstraint -> var_id list
end

module type Box_interpretation_functor = functor (Vardom: Vardom_sig) -> Box_interpretation_sig
  with module Vardom=Vardom

module Box_interpretation = functor (Vardom: Vardom_sig) ->
struct
  module Vardom = Vardom
  module Store = Var_store.Make(Vardom)

  module IB = Interpretation_base(struct type var_id=Store.key end)
  include IB

  type var_dom = Store.cell

  type rexpr = {
    node: node;
    mutable value: Vardom.t
  }
  and node =
    | BFuncall of string * rexpr list
    | BUnary   of unop * rexpr
    | BBinary  of rexpr * binop * rexpr
    | BVar     of var_id
    | BCst     of Vardom.t * Types.var_abstract_ty

  type rconstraint = rexpr * cmpop * rexpr

  let make_expr e = { node=e; value=fst (Vardom.top ()) }

  let interpret_expr repr e : rexpr =
    let rec aux e : rexpr =
      let e = match e with
        | Funcall(x, exprs) -> BFuncall(x, List.map aux exprs)
        | Unary(NEG, e) -> BUnary(NEG, aux e)
        | Binary(e1, op, e2) -> BBinary (aux e1, op, aux e2)
        | Var(x) -> BVar(fst (to_abstract_var_wm repr x))
        | Cst(v, cty) ->
            let l, u = ((Vardom.B.of_rat_down v),(Vardom.B.of_rat_up v)) in
            let v, aty = Vardom.of_bounds ~ty:(Types.Concrete cty) (l,u) in
            BCst(v, aty) in
      make_expr e in
    aux e

  (* We check that the approximation is possible with the current variables and formula `f`.
     See `interpret` documentation. *)
  let check_approx_typing repr tf approx =
    let is_exact_approx v =
      let _, tv = to_abstract_var_wm repr v in
      (* We list the variants explicitly because it must be considered if we add a new abstract type. *)
      match tv.ty with
      | Concrete _ -> failwith
          "A variable in Box has a concrete type but it should be instantiated to an abstract type."
      | Abstract ty ->
        match ty with
        | VUnit | Bool | Machine Z | Machine Q | BDD _ -> ()
        | Machine F -> raise (Wrong_modelling ("Variable `" ^ v ^ "` cannot be `" ^ (string_of_approx approx) ^ "` in the current abstract domain."))
    in
    match approx with
    | OverApprox -> ()
    | UnderApprox | Exact ->
       let vars = vars_of_tformula tf in
       List.iter is_exact_approx vars

  let interpret_bconstraint repr (e1, op, e2) =
    [(interpret_expr repr e1, op, interpret_expr repr e2)]

  let interpret repr approx tf =
    if (fst tf) <> (IB.uid repr) then
      raise (Wrong_modelling "Box.interpret: The formula has not the right UID.");
    let rec aux (_, f) =
      match f with
      | TCmp c -> interpret_bconstraint repr c
      | TFVar x -> interpret_bconstraint repr (Var x, EQ, one)
      | TNot ((_,TFVar x)) -> interpret_bconstraint repr (Var x, EQ, zero)
      | TAnd (tf1, tf2) -> (aux tf1)@(aux tf2)
      | _ -> raise (Wrong_modelling "Box.interpret: Box do not support logical constraints (see e.g. `Logic_completion`).")
    in
    check_approx_typing repr tf approx;
    (repr, aux tf)

  let to_logic_expr repr expr =
    let rec aux expr =
      match expr.node with
      | BCst (v,aty) ->
          Cst (fst (Vardom.to_rational_range v), Types.abstract_to_concrete_ty aty)
      | BVar x -> Var(to_logic_var' repr x)
      | BUnary (op, e) -> Unary(op, aux e)
      | BBinary (e1, op, e2) -> Binary(aux e1, op, aux e2)
      | BFuncall (x,args) -> Funcall(x, List.map aux args) in
    aux expr

  let to_formula_one repr (e1, op, e2) =
    (IB.uid repr, TCmp (to_logic_expr repr e1, op, to_logic_expr repr e2))

  let to_qformula repr cs =
    let fs = List.map (fun c -> TQFFormula (to_formula_one repr c)) cs in
    let tqf = q_conjunction (IB.uid repr) fs in
    match tqf with
    | TQFFormula tf -> equantify repr tf
    | _ -> failwith "unreachable (to_qformula): no variable inserted."

  let rec vars_of_expr expr =
    match expr.node with
    | BCst _ -> []
    | BVar v -> [v]
    | BUnary (_, e) -> vars_of_expr e
    | BBinary (e1, _, e2) -> (vars_of_expr e1)@(vars_of_expr e2)
    | BFuncall (_,args) -> List.concat (List.map vars_of_expr args)

  let vars_of_constraint (e1,_,e2) = (vars_of_expr e1)@(vars_of_expr e2)
end
