(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core.Types
open Lang.Ast
open Ad_type

(** Generic annotated version of a formula.
    It is useful to have this generic structure for UID types, and in [Infer] where we need to store local information along the formula. *)
type 'a aformula = 'a * 'a aformula_
and 'a aformula_ =
  | TFVar of var
  | TCmp of bconstraint
  | TEquiv of 'a aformula * 'a aformula
  | TImply of 'a aformula * 'a aformula
  | TAnd of 'a aformula * 'a aformula
  | TOr  of 'a aformula * 'a aformula
  | TNot of 'a aformula

type 'a aqformula =
  | TQFFormula of 'a aformula
  | TExists of var * var_ty * 'a * 'a aqformula

(** Remove the types from a formula.
    It is particularly useful to use the functions available in [Rewriting] and [Pretty_print] modules. *)
val aformula_to_formula: 'a aformula -> formula
val aqformula_to_qformula: 'a aqformula -> qformula

val string_of_aformula: 'a aformula -> string

val map_tqf: ('a aformula -> 'a aformula) -> 'a aqformula -> 'a aqformula
val map_annot_aformula: 'a aformula -> ('a -> 'b) -> 'b aformula
val map_annot_aqformula: 'a aqformula -> ('a -> 'b) -> 'b aqformula

(** We equip a formula with the UIDs of abstract elements in which it can be interpreted.
    The UID `0` is by default the largest abstract type avalaible (usually a product among the available abstract domains).
    This allows us to create partially typed formula, if the user wants some parts to be treated in a certain domain. *)
type tformula = ad_uid aformula
type tformula_ = ad_uid aformula_

(** Each variable has a type and belong to a list of abstract elements.
    When interpreted, a variable belonging to more than one abstract element is added in all these elements. *)
type tqformula = ad_uid aqformula
