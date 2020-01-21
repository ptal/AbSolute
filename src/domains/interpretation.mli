(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Lang
open Typing
open Typing.Ad_type

(** The kind of approximation when interpreting a logical formula into an abstract element.
    Note that the values `UnderApprox` or `OverApprox` interpret the constraint exactly if it is possible.
    Over-approximation: the abstract element might have more solutions than the concrete solutions of the formula.
    Under-approximation: the abstract element might have less solutions than the concrete solutions of the formula. *)
type approx_kind =
    Exact
  | UnderApprox
  | OverApprox

(** Over-approximation becomes under-approximation, and vice-versa. *)
val neg_approx: approx_kind -> approx_kind

(** Past participle form of the verb representing the approximation (i.e. "under-approximated"). *)
val string_of_approx: approx_kind -> string

(** Every abstract domain may have a different variable and constraint representation according to their internal implementation.
  We ask every abstract domain to provide an interpretation module in order to connect the logic specification (`Ast.tqformula`) and the representation of the abstract domain.
  This module can also interpret a logic constraint into a more suited representation of the abstract domain.
  Rational: Why not directly pushing these functions into the definition of the abstract domain?
    We could parametrize the abstract domain by an interpretation although we do not do it yet.
    Another reason is that sometimes, we want to convert a formula into a abstract domain's constraint but without adding this formula in the abstract domain yet.
    This is for example the case with equivalence constraint "c1 <=> c2".
    We avoid the overhead of converting the logical to abstract constraint during the computation. *)
module type Interpretation_sig = sig
  type t

  (** Variable ID as represented in the abstract domain. *)
  type var_id

  (** Constraint representation in the abstract domain. *)
  type rconstraint

  (** An empty interpretation. *)
  val empty: ad_uid -> t

  (** `True` if the variable `v` exists in the abstract element.
      Note that it does not necessarily implies that a mapping exists.
      This is convenient for abstract domain that are defined on top of others domains such as `Logic_product`. *)
  val exists: t -> Ast.vname -> bool

  (** Conversions utilities between logical variables and their
     representations. *)
  val to_logic_var: t -> var_id -> Tast.tvariable
  val to_abstract_var: t -> Ast.vname -> (var_id * Tast.tvariable)

  (** Interpret a logic formula into a list of abstract constraints.
      It approximates the representation of the formula if needed according to `approx`.
      The list of constraints might be empty if the formula is detected to be a tautology.
      Raise `Wrong_modelling` if:
        1. the formula cannot be approximated according to `approx` in the abstract domain.
        2. a variable of the formula does not belong to the interpretation. *)
  val interpret: t -> approx_kind -> Tast.tformula -> t * rconstraint list

  (** Give a logical representation of an abstract element.
      This function is the reverse of `interpret`.
      Note that we do not need to approximate the result as a formula should always be able to represent exactly an element.
      The free variables in the formula obtained should be considered existentially quantified. *)
  val to_qformula: t -> rconstraint list -> Tast.tqformula
end

(* Many interpretations have the same underlying structure that we factorize in this module.
   See `Interpretation_sig` for the documentation of these functions. *)
module Interpretation_base(V_ID:sig type var_id end) :
sig
  type var_id = V_ID.var_id
  type t

  val empty: ad_uid -> t
  val uid: t -> ad_uid

  (** Add a mapping between a logical variable and its representation
     in the abstract domain. *)
  val extend: t -> var_id * Tast.tvariable -> t
  val exists: t -> Ast.vname -> bool
  val to_logic_var: t -> var_id -> Tast.tvariable
  val to_abstract_var: t -> Ast.vname -> (var_id * Tast.tvariable)

  (** Add existential quantifiers to the variables occuring in the formula. *)
  val equantify: t -> Tast.tformula -> Tast.tqformula

  (** Conveniency version of `to_logic_var` without the type of the variable. *)
  val to_logic_var': t -> var_id -> Ast.vname
  val to_abstract_var': t -> Ast.vname -> var_id

  (** Conveniency version of `to_abstrct_var` raising `Wrong_modelling` with a message indicating that the variable does not belong to the abstract element. *)
  val to_abstract_var_wm: t -> Ast.vname -> (var_id * Tast.tvariable)
end

(** Helper functions to interpret and transform constraints in ground abstract domains. *)
module Interpretation_ground(V_ID:sig type var_id end):
sig
  include module type of (Interpretation_base(V_ID))

  (** [interpret_helper repr ad_name approx tf interpret_bconstraint] is a helper function to interpret a formula.
      It checks that the formula's UID matches the one of the domain, and that the formula is not a tautology.
      It also checks that only ground constraints connected with conjunction are used in the formula. *)
  val interpret_gen: t -> string -> Tast.tformula
    -> (t -> Ast.bconstraint -> 'a list) -> t * 'a list

  (** Same than [interpret_gen] but with a more general sub-interpretation function. *)
  val interpret_gen': t -> string -> Tast.tformula
    -> (t -> Tast.tformula -> t * 'a list) -> t * 'a list

  (** [to_qformula repr cs to_formula_one] creates a typed quantified formula.
      The function `to_formula_one` turns a rconstraint into its unquantified typed formula. *)
  val to_qformula_gen: t -> 'a list -> (t -> 'a -> Tast.tformula) -> Tast.tqformula
end
