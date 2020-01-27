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
open Lang.Ast
open Typing
open Typing.Ad_type
open Typing.Tast
open Vardom
open Domains.Interpretation
open Domains.Abstract_domain

module type PC_interpretation_sig =
sig
  module V: Vardom_sig.S
  module A: Abstract_domain

  type t = {
    a: A.t ref;
    uid: ad_uid;
  }

  type var_id = unit

  (** We annotate each node of this expression with its interval evaluation.
    This is useful for the HC4 algorithm.
    The `value` field is get_vars_set_formulanever backtracked, it is only useful to propagate inside on node of the search tree. *)
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
  val to_logic_var: t -> var_id -> Tast.tvariable
  val to_abstract_var: t -> vname -> (var_id * Tast.tvariable)

  (** Over-approximation is always supported.
      No special effort is currently made for supporting under-approximations, it is considered equivalent to Exact representation.
      Exact representation is supported if the abstract types of the variables of `f` are all different from floating point numbers. *)
  val interpret: t -> approx_kind -> Tast.tformula -> t * rconstraint list
  val to_qformula: t -> rconstraint list -> Tast.tqformula

  (** Create an expression from a node.
      The vardom ref is initialized to TOP. *)
  val make_expr: node -> rexpr
end

module type PC_interpretation_functor =
  functor (V: Vardom_sig.S)(A: Abstract_domain) -> PC_interpretation_sig
    with module V = V and module A = A

module PC_interpretation: PC_interpretation_functor
