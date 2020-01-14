(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** Annotated AST (AAST).
    This AST is used internally by the type inference engine (`Infer.ml`). *)

open Core.Types
open Lang.Ast

(** Generic annotated version of a formula.
    It is useful to have this generic structure for UID types, and in [Infer] where we need to store local information along the formula. *)
type 'a aformula = 'a * 'a aformula_
and 'a aformula_ =
  | AFVar of vname
  | ACmp of bconstraint
  | AEquiv of 'a aformula * 'a aformula
  | AImply of 'a aformula * 'a aformula
  | AAnd of 'a aformula * 'a aformula
  | AOr  of 'a aformula * 'a aformula
  | ANot of 'a aformula

type 'a aqformula =
  | AQFFormula of 'a aformula
  | AExists of vname * var_ty * 'a * 'a aqformula

(** Remove the types from a formula.
    It is particularly useful to use the functions available in [Rewriting] and [Pretty_print] modules. *)
val aformula_to_formula: 'a aformula -> formula
val aqformula_to_qformula: 'a aqformula -> qformula

val string_of_aformula: 'a aformula -> string

val map_tqf: ('a aformula -> 'a aformula) -> 'a aqformula -> 'a aqformula
val map_annot_aformula: 'a aformula -> ('a aformula -> 'b) -> 'b aformula
val map_annot_aqformula: 'a aqformula -> ('a -> 'b) -> 'b aqformula
