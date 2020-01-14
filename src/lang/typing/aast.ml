(* Copyright 2020 Pierre Talbot

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

let rec aformula_to_formula (_,af) =
  match af with
  | AFVar v -> FVar v
  | ACmp c -> Cmp c
  | AEquiv(f1,f2) -> Equiv(aformula_to_formula f1, aformula_to_formula f2)
  | AImply(f1,f2) -> Imply(aformula_to_formula f1, aformula_to_formula f2)
  | AAnd(f1,f2) -> And(aformula_to_formula f1, aformula_to_formula f2)
  | AOr(f1,f2) -> Or(aformula_to_formula f1, aformula_to_formula f2)
  | ANot f1 -> Not (aformula_to_formula f1)

let rec aqformula_to_qformula = function
  | AQFFormula f -> QFFormula (aformula_to_formula f)
  | AExists(v, ty, _, qf) -> Exists (v, ty, aqformula_to_qformula qf)

let string_of_aformula af =
  Lang.Pretty_print.print_formula Format.str_formatter (aformula_to_formula af);
  Format.flush_str_formatter ()

let rec map_tqf f = function
  | AQFFormula tqf -> AQFFormula (f tqf)
  | AExists (v,vty,ty,tf) -> AExists (v,vty,ty,map_tqf f tf)

let rec map_annot_aformula (a, af) f =
  (f (a, af), match af with
  | AFVar v -> AFVar v
  | ACmp c -> ACmp c
  | AEquiv(f1,f2) -> AEquiv(map_annot_aformula f1 f, map_annot_aformula f2 f)
  | AImply(f1,f2) -> AImply(map_annot_aformula f1 f, map_annot_aformula f2 f)
  | AAnd(f1,f2) -> AAnd(map_annot_aformula f1 f, map_annot_aformula f2 f)
  | AOr(f1,f2) -> AOr(map_annot_aformula f1 f, map_annot_aformula f2 f)
  | ANot f1 -> ANot(map_annot_aformula f1 f))

let rec map_annot_aqformula af f =
  match af with
  | AQFFormula qf -> AQFFormula (map_annot_aformula qf (fun (a,_) -> f a))
  | AExists(v, ty, a, qf) -> AExists (v, ty, f a, map_annot_aqformula qf f)
