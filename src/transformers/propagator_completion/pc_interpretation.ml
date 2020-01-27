(* Copyright 2020 Pierre Talbot

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
open Domains.Abstract_domain
open Typing.Tast
open Typing.Ad_type
open Lang.Ast
open Vardom

module type PC_interpretation_sig =
sig
  module V: Vardom_sig.S
  module A: Abstract_domain

  type t = {
    a: A.t ref;
    uid: ad_uid;
  }

  type var_id = unit

  type rexpr = {
    node: node;
    mutable value: V.t
  }
  and node =
    | BFuncall of string * rexpr list
    | BUnary   of unop * rexpr
    | BBinary  of rexpr * binop * rexpr
    | BVar     of A.I.var_id * tvariable
    | BCst     of V.t * Types.var_abstract_ty

  type rconstraint = rexpr * cmpop * rexpr

  val name: string
  val exact_interpretation: bool
  val empty: ad_uid -> t
  val to_logic_var: t -> var_id -> tvariable
  val to_abstract_var: t -> vname -> (var_id * tvariable)
  val interpret: t -> approx_kind -> tformula -> t * rconstraint list
  val to_qformula: t -> rconstraint list -> tqformula

  val make_expr: node -> rexpr
end

module type PC_interpretation_functor =
  functor (V: Vardom_sig.S)(A: Abstract_domain) -> PC_interpretation_sig
    with module V = V and module A = A

let no_variable_exn msg = no_variable_exn msg; failwith "unreachable"

module PC_interpretation = functor (V: Vardom_sig.S)(A: Abstract_domain) ->
struct
  module V = V
  module A = A

  type t = {
    a: A.t ref;
    uid: ad_uid;
  }

  type var_id = unit

  type rexpr = {
    node: node;
    mutable value: V.t
  }
  and node =
    | BFuncall of string * rexpr list
    | BUnary   of unop * rexpr
    | BBinary  of rexpr * binop * rexpr
    | BVar     of A.I.var_id * tvariable
    | BCst     of V.t * Types.var_abstract_ty

  type rconstraint = rexpr * cmpop * rexpr

  let name = "Propagator_completion(" ^ A.name ^ ")"

  let exact_interpretation = not (Types.is_continuous V.B.abstract_ty)

  let empty _ = raise (Wrong_modelling "`PC_interpretation.empty` is not supported, you should first create the abstract domains and then create the `Propagator_completion`.")
  let to_logic_var _ _ = no_variable_exn "Logic_completion_interpretation.to_logic_var"
  let to_abstract_var _ _ = no_variable_exn "Logic_completion_interpretation.to_abstract_var"

  let make_expr e = { node=e; value=fst (V.top ()) }

  let to_abstract_var_wm repr x =
    try
      A.I.to_abstract_var (A.interpretation !(repr.a)) x
    with Not_found ->
      raise (Wrong_modelling ("Propagation_completion: Could not find the variable `" ^ x ^ "`."))


  let interpret_expr repr e : rexpr =
    let rec aux e : rexpr =
      let e = match e with
        | Funcall(x, exprs) -> BFuncall(x, List.map aux exprs)
        | Unary(NEG, e) -> BUnary(NEG, aux e)
        | Binary(e1, op, e2) -> BBinary (aux e1, op, aux e2)
        | Var(x) ->
            let (vid, tv) = to_abstract_var_wm repr x in
            BVar(vid, tv)
        | Cst(v, cty) ->
            let l, u = ((V.B.of_rat_down v),(V.B.of_rat_up v)) in
            let v, aty = V.of_bounds ~ty:(Types.Concrete cty) (l,u) in
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
    guarded_interpret repr repr.uid name tf
      (fun repr tf ->
        check_approx_typing repr tf approx;
        ground_interpret repr repr.uid name tf interpret_bconstraint)

  let to_logic_expr expr =
    let rec aux expr =
      match expr.node with
      | BCst (v,aty) ->
          Cst (fst (V.to_rational_range v), Types.abstract_to_concrete_ty aty)
      | BVar (_,tv) -> Var tv.name
      | BUnary (op, e) -> Unary(op, aux e)
      | BBinary (e1, op, e2) -> Binary(aux e1, op, aux e2)
      | BFuncall (x,args) -> Funcall(x, List.map aux args) in
    aux expr

  let to_formula_one repr (e1, op, e2) =
    (repr.uid, TCmp (to_logic_expr e1, op, to_logic_expr e2))

  let to_qformula repr cs =
    let fs = List.map (fun c -> TQFFormula (to_formula_one repr c)) cs in
    q_conjunction repr.uid fs
end
