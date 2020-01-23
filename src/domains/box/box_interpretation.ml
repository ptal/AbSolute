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
open Bounds
open Domains.Interpretation
open Typing.Tast
open Lang
open Lang.Ast
open Vardom.Vardom_sig
open Var_store

module type Box_interpretation_sig =
sig
  module Vardom: Vardom_sig
  module Store: Var_store_sig with module V=Vardom

  include module type of (Interpretation_ground(struct type var_id=Store.key end))
  type var_dom = Store.cell
  type rconstraint = var_id * var_dom

  val exact_interpretation: bool
  val interpret: t -> approx_kind -> tformula -> t * rconstraint list
  val to_qformula: t -> rconstraint list -> tqformula
end

module type Box_interpretation_functor = functor (Vardom: Vardom_sig) -> Box_interpretation_sig
  with module Vardom=Vardom

module Box_interpretation = functor (Vardom: Vardom_sig) ->
struct
  module Vardom = Vardom
  module Store = Var_store.Make(Vardom)

  module IG = Interpretation_ground(struct type var_id=Store.key end)
  include IG

  type var_dom = Store.cell
  type rconstraint = var_id * var_dom

  let exact_interpretation = not (Types.is_continuous Vardom.B.abstract_ty)

  let interpret_bconstraint approx repr (e1, op, e2) =
    let x, op, (v,_) =
      match e1, op, e2 with
      | Var x, op, Cst(v, ty) -> (x, op, (v,ty))
      | Cst(v, ty), op, Var x -> (x, Rewritting.inv op, (v,ty))
      | c -> raise (Wrong_modelling ("Cannot interpret the formula " ^
          (Pretty_print.string_of_constraint c) ^ " in box."))
    in
      let vid, tv = IG.to_abstract_var_wm repr x in
      [(vid, Vardom.interpret approx (tv,op,Vardom.of_rat v))]

  let interpret repr approx tf =
    IG.interpret_gen repr "Box" tf (interpret_bconstraint approx)

  (* NOTE: It would be more precise if we add to_formula in Vardom as well, and use this one here.
     For instance, we could have `x > l` from Open_close_itv. *)
  let to_formula_one repr (vid, v) =
    let x = IG.to_logic_var' repr vid in
    let make_cons x op v = Some (IG.uid repr, TCmp (Var x, op, Cst (v, Vardom.B.concrete_ty))) in
    let (l,u) = Vardom.to_rational_range v in
    let lb_cons =
      if Bound_rat.classify l = FINITE then
        make_cons x GEQ l
      else None in
    let ub_cons =
      if Bound_rat.classify u = FINITE then
        make_cons x LEQ u
      else None in
    match lb_cons, ub_cons with
    | Some c1, Some c2 -> (IG.uid repr, TAnd(c1,c2))
    | Some c1, None -> c1
    | None, Some c2 -> c2
    | None, None -> (IG.uid repr, ctrue)

  let to_qformula repr cs = IG.to_qformula_gen repr cs to_formula_one
end