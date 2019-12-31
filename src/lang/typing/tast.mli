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

(** We equip a formula with the possible UIDs of abstract elements in which it can be interpreted.
    The list can be empty if the abstract type of the formula has not been inferred yet.
    This allows us to create partially typed formula, if the user wants some parts to be treated in a certain domain. *)
type tformula = ad_uid list * tformula_
and tformula_ =
  | TFVar of var
  | TCmp of bconstraint
  | TEquiv of tformula * tformula
  | TImply of tformula * tformula
  | TAnd of tformula * tformula
  | TOr  of tformula * tformula
  | TNot of tformula

(** Each variable has a type and belong to a list of abstract elements.
    When interpreted, a variable belonging to more than one abstract element is added in all these elements. *)
type tqformula =
  | TQFFormula of tformula
  | TExists of var * var_ty * ad_uid list * tqformula

(** Cf. [qformula_to_tqformula]. *)
val formula_to_tformula: formula -> tformula

(** This function creates an empty typed formula (all the `ad_uid list` are empty).
    The type can be inferred next in `Typing.infer_type`. *)
val qformula_to_tqformula: qformula -> tqformula
