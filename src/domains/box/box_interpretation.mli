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
open Lang.Ast
open Typing
open Vardom
open Var_store
open Domains.Interpretation

module type Box_interpretation_sig =
sig
  module Vardom: Vardom_sig.Vardom_sig

  (** We depend on the store to represent the `var_id`.
      However, the store is contained in `Box` itself and not here.
      NOTE: We could parametrize Box_interpretation_sig with a `Store` if we have different kind of stores in the future. *)
  module Store: Var_store_sig with module V=Vardom

  include module type of (Interpretation_ground(struct type var_id=Store.key end))

  type var_dom = Store.cell

  (** We annotate each node of this expression with its interval evaluation.
    This is useful for the HC4 algorithm.
    The `value` field is get_vars_set_formulanever backtracked, it is only useful to propagate inside on node of the search tree. *)
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

  val exact_interpretation: bool

  (** Over-approximation is always supported.
      No special effort is currently made for supporting under-approximations, it is considered equivalent to Exact representation.
      Exact representation is supported if the abstract types of the variables of `f` are all different from floating point numbers. *)
  val interpret: t -> approx_kind -> Tast.tformula -> t * rconstraint list
  val to_qformula: t -> rconstraint list -> Tast.tqformula

  (** Create an expression from a node.
      The vardom ref is initialized to TOP. *)
  val make_expr: node -> rexpr

  (** List of variables occuring in the constraint (duplicates possible). *)
  val vars_of_constraint: rconstraint -> var_id list
end

module type Box_interpretation_functor = functor (Vardom: Vardom_sig.Vardom_sig) -> Box_interpretation_sig
  with module Vardom=Vardom

module Box_interpretation: Box_interpretation_functor
