(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** Typed AST (TAST).
    This is the AST used by the abstract domain because each sub-formula contains type information.
    In our context, the type of a formula is an index of an abstract element.
    We also have types for variables (see `Types.var_ty`) but they work at the value level rather than the level of abstract domains. *)

open Core
open Lang.Ast
open Ad_type

type tvariable = {
  name: vname;
  (** Name of the variable as appearing in the model. *)

  ty: Types.var_ty;
  (** Type of the variable in the abstract element.
      If the element is a product, and the variable has two different abstract types in the sub-domains, then the concrete domain is stored. *)

  uid: ad_uid;
  (** UID of the abstract element in which the variable occurs. *)
}

val string_of_tvar: tvariable -> string

type tformula = ad_uid * tformula_
and tformula_ =
  | TFVar of vname
  | TCmp of bconstraint
  | TEquiv of tformula * tformula
  | TImply of tformula * tformula
  | TAnd of tformula * tformula
  | TOr  of tformula * tformula
  | TNot of tformula

(** Each variable has a type and belong to an abstract element. *)
type tqformula =
  | TQFFormula of tformula
  | TExists of tvariable * tqformula

val tformula_to_formula: tformula -> formula
val tqformula_to_qformula: tqformula -> qformula

(** Encoding of true and false as formula. *)
val ctrue: tformula_
val ttrue: tqformula
val tfalse: tqformula

val vars_of_tformula: tformula -> vname list

(** [quantify env f] Given a variable environment `env`, existentially quantify the formula `f`.
    It adds `Exists` in front of `f` for each variable occuring in `f` and `env`.
    Variables not in `env` but in `f` (and vice-versa) are ignored. *)
val quantify: tvariable list -> tformula -> tqformula

(** [quantifiers tqf] extracts all the existentially quantified variables from `tqf`.
    Duplicated quantifiers are removed.
    `Wrong_modelling` is raised if two identical variable's names have two distinct types. *)
val quantifiers: tqformula -> tvariable list

(** [quantifier_free_of tqf] removes the quantifiers from the formula `tqf`. *)
val quantifier_free_of: tqformula -> tformula

(** [map_formula f tqf] applies the function `f` on `tqf` bypassing the quantifiers. *)
val map_tformula: (tformula -> tformula) -> tqformula -> tqformula

(** [merge_formula make f1 f2].
    Given two quantified formulas, merge their quantifier-free part using `make`.
    The existential binders of identical names are merged together. *)
val merge_formula: (tformula -> tformula -> tformula) -> tqformula -> tqformula -> tqformula

(** Merge a list of formula with the conjunctive connector of the given abstract domain type. *)
val q_conjunction: ad_uid -> tqformula list -> tqformula
val q_disjunction: ad_uid -> tqformula list -> tqformula

(** Negate a logic formula.
    NOT are pushed inwards such that it only occur over FVar.
    The size of the formula does not increase.
    The UID is the UID of a domain that support arbitrary logical formula. *)
val neg_formula: ad_uid -> tformula -> tformula

(** [replace_uid uid tf] replaces all the UIDs that are equal to `fst tf` by `uid` in `tf`. *)
val replace_uid: ad_uid -> tformula -> tformula
