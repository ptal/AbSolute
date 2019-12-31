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

type tformula = ad_uid list * tformula_
and tformula_ =
  | TFVar of var
  | TCmp of bconstraint
  | TEquiv of tformula * tformula
  | TImply of tformula * tformula
  | TAnd of tformula * tformula
  | TOr  of tformula * tformula
  | TNot of tformula

type tqformula =
  | TQFFormula of tformula
  | TExists of var * var_ty * ad_uid list * tqformula

let rec formula_to_tformula f =
  let tf = match f with
    | FVar v -> TFVar v
    | Cmp c -> TCmp c
    | Equiv(f1, f2) -> TEquiv(formula_to_tformula f1, formula_to_tformula f2)
    | Imply(f1, f2) -> TImply(formula_to_tformula f1, formula_to_tformula f2)
    | And(f1, f2) -> TAnd(formula_to_tformula f1, formula_to_tformula f2)
    | Or(f1, f2) -> TOr(formula_to_tformula f1, formula_to_tformula f2)
    | Not f1 -> TNot (formula_to_tformula f1)
  in
  ([], tf)

let rec qformula_to_tqformula = function
| QFFormula f -> TQFFormula (formula_to_tformula f)
| Exists(v, ty, qf) -> TExists (v, ty, [], qformula_to_tqformula qf)
