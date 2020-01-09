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

let rec aformula_to_formula (_,af) =
  match af with
  | TFVar v -> FVar v
  | TCmp c -> Cmp c
  | TEquiv(f1,f2) -> Equiv(aformula_to_formula f1, aformula_to_formula f2)
  | TImply(f1,f2) -> Imply(aformula_to_formula f1, aformula_to_formula f2)
  | TAnd(f1,f2) -> And(aformula_to_formula f1, aformula_to_formula f2)
  | TOr(f1,f2) -> Or(aformula_to_formula f1, aformula_to_formula f2)
  | TNot f1 -> Not (aformula_to_formula f1)

let rec aqformula_to_qformula = function
  | TQFFormula f -> QFFormula (aformula_to_formula f)
  | TExists(v, ty, _, qf) -> Exists (v, ty, aqformula_to_qformula qf)

let string_of_aformula af =
  Lang.Pretty_print.print_formula Format.str_formatter (aformula_to_formula af);
  Format.flush_str_formatter ()

let rec map_tqf f = function
  | TQFFormula tqf -> TQFFormula (f tqf)
  | TExists (v,vty,ty,tf) -> TExists (v,vty,ty,map_tqf f tf)

let rec map_annot_aformula (a, af) f =
  (f a, match af with
  | TFVar v -> TFVar v
  | TCmp c -> TCmp c
  | TEquiv(f1,f2) -> TEquiv(map_annot_aformula f1 f, map_annot_aformula f2 f)
  | TImply(f1,f2) -> TImply(map_annot_aformula f1 f, map_annot_aformula f2 f)
  | TAnd(f1,f2) -> TAnd(map_annot_aformula f1 f, map_annot_aformula f2 f)
  | TOr(f1,f2) -> TOr(map_annot_aformula f1 f, map_annot_aformula f2 f)
  | TNot f1 -> TNot(map_annot_aformula f1 f))

let rec map_annot_aqformula af f =
  match af with
  | TQFFormula qf -> TQFFormula (map_annot_aformula qf f)
  | TExists(v, ty, a, qf) -> TExists (v, ty, f a, map_annot_aqformula qf f)

type tformula = ad_uid aformula
type tformula_ = ad_uid aformula_
type tqformula = ad_uid aqformula
