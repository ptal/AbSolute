(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)


(** This module attempt to give a type to each component of a logical formula.
    In our context, a type is a list of UIDs representing the abstract domains in which the part of a formula can be typed.

    NOTE: Only the function [infer_type] should be useful for users of this module, but we also give the functions of the inference engine for documentation purposes. *)

open Core.Types
open Ad_type
open Tast
open Lang.Ast

(** The type inferred from a formula.
    In case the user already provided some UIDs, we flag these with a Boolean.  *)
type inferred_type =
  | CannotType of string
  | Typed of ad_uid list

val merge_ity: inferred_type -> inferred_type -> inferred_type

type iformula = inferred_type aformula
type iqformula = inferred_type aqformula

val uids_of': inferred_type -> ad_uid list
val uids_of: iformula -> ad_uid list

val is_uid_in: ad_uid -> iformula -> bool
val is_uid_in2: ad_uid -> iformula -> iformula -> bool

(** See [qformula_to_iqformula]. *)
val formula_to_iformula: formula -> iformula

(** Convert an untyped formula to a formula with a empty type (Typed []) that will be populated. *)
val qformula_to_iqformula: qformula -> iqformula

(** Module containing all the necessary information to type a formula with a given abstract domain type. *)
module Inference :
sig
  module Var2UID : Map.S with type key=var
  type var_env = (ad_uid list) Var2UID.t
  type t

  (** Boolean is the `trace` which indicates if the inference should create the error message or not (it might take longer if yes). *)
  val init: ad_ty -> bool -> t

  (** Create `CannotType` with the given message if the trace is activated, and an empty message otherwise. *)
  val gen_err: t -> (unit -> string) -> inferred_type

  val variable_not_in_dom_err: t -> var -> inferred_type
  val not_an_octagonal_constraint_err: t -> inferred_type
  val ground_dom_does_not_handle_logic_connector_err: t -> inferred_type
  val no_domain_support_this_variable_err: t -> var -> var_ty -> inferred_type
  val sat_does_not_support_term_err: t -> inferred_type
  val direct_product_no_subdomain_err: t -> inferred_type
  val logic_completion_subdomain_failed_on_term_err: t -> inferred_type

  (* I. Inference of the variables types. *)

  val interval_can_represent_var: vardom_ty -> var_ty -> bool
  val compatible_ty: var_ty -> value_ty -> bool
  val infer_var: var_ty -> ad_ty -> ad_uid list

  (** For each variable, infer its UIDs. *)
  val infer_vars_ty: t -> iqformula -> (t * iqformula)

  (* II. Inference of the constraints types. *)

  val belong: t -> ad_uid -> var -> bool
  val bool_var_infer: t -> var -> ad_uid -> inferred_type

  (** Infer type for ground abstract domain, [term_infer] gives a type to term.
      It is a helper to implement inference for box, octagon and other ground abstract domains.
      It walks through conjunction `TAnd`, `TFVar` and call [term_infer] on `TCmp`. *)
  val ground_dom_infer: t -> ad_uid -> (bconstraint -> inferred_type) -> iformula -> iformula

  (** `None` if the variables of `c` are all treatable in the element `uid`, otherwise `Some v` where `v` is a variable not treatable. *)
  val fully_defined_over: t -> ad_uid -> bconstraint -> var option

  (** Infer the type of all the box constraints. *)
  val box_infer: t -> ad_uid -> iformula -> iformula

  (** Infer the type of all the octagonal constraints. *)
  val octagon_infer: t -> ad_uid -> iformula -> iformula

  (** A generic inference scheme for logical formula (useful for SAT and Logic completion).
      [literal] and [term] are functions called on [TFVar] and [TCmp] respectively. *)
  val generic_formula_infer: ad_uid -> iformula -> (var -> iformula -> inferred_type) -> (bconstraint -> iformula -> inferred_type) -> iformula

  (** Infer the type of all SAT subformulas. *)
  val sat_infer: t -> ad_uid -> iformula -> iformula

  (** Infer the type of all subformulas compatible with this direct product. *)
  val direct_product_infer: t -> ad_uid -> ad_ty list -> iformula -> iformula

  (** Infer the type of all subformulas compatible with the logic completion of the given domain's type. *)
  val logic_completion_infer: t -> ad_uid -> ad_ty -> iformula -> iformula

  (** General inference of a formula with regards to the available abstract domain type. *)
  val infer_constraints_ty: t -> iformula -> ad_ty -> iformula

  (** Type a formula or raise `Wrong_modelling` if it cannot type it. *)
  val infer_constraints_ty_or_fail: t -> iqformula -> iqformula

  (* III. Removes domain uids from variables if only unary (or no) constraints are involved with this variable in this domain.
       If a variable has only unary (or no) constraints, we only keep the least specialized (most efficient) domain. *)

  module VarConsCounter : Map.S with type key=(var * ad_uid)

  (** We first create a structure storing the number of unary and nary constraints for each variable and domain uid. *)
  val build_var_cons_map: iformula -> (int * int) VarConsCounter.t

  (** Instantiate a variable with a single type. *)
  val instantiate_var_ty: iqformula -> iqformula

  (* IV. Select a single type for each sub-formula.
         If several abstract domain are on-par (unordered), only the first one is kept. *)

  (** Instantiate a formula with a single type per sub-formula. *)
  val instantiate_formula_ty: t -> iqformula -> tqformula

  (** Raises `Wrong_modelling` if a variable in `TExists` has a type `CannotType`. *)
  val check_type_var: iqformula -> unit
end

(** Given an abstract domain type and an untyped formula, infer the type of each component of the formula matching the given abstract domain.
    Raises `Wrong_modelling msg` if the formula is not typable for this abstract domain's type; `msg` should explain why the formula is not typable.
    Note: `ad_ty` encompasses domain transformers including product such as "t1 X t2", thus part of a formula might be typed with `t1` or `t2`. *)
val infer_type: ad_ty -> qformula -> tqformula
