(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core
open Lang
open Lang.Ast
open Ad_type

type tvariable = {
  name: vname;
  ty: Types.var_ty;
  uid: ad_uid;
}

let string_of_tvar tv =
  tv.name ^ ":" ^ (Types.string_of_ty tv.ty) ^ "@" ^ (string_of_int tv.uid)

type tformula = ad_uid * tformula_
and tformula_ =
  | TFVar of vname
  | TCmp of bconstraint
  | TEquiv of tformula * tformula
  | TImply of tformula * tformula
  | TAnd of tformula * tformula
  | TOr  of tformula * tformula
  | TNot of tformula

type tqformula =
  | TQFFormula of tformula
  | TExists of tvariable * tqformula

let rec tformula_to_formula (_,f) =
  match f with
  | TFVar v -> FVar v
  | TCmp c -> Cmp c
  | TEquiv(f1,f2) -> Equiv(tformula_to_formula f1, tformula_to_formula f2)
  | TImply(f1,f2) -> Imply(tformula_to_formula f1, tformula_to_formula f2)
  | TAnd(f1,f2) -> And(tformula_to_formula f1, tformula_to_formula f2)
  | TOr(f1,f2) -> Or(tformula_to_formula f1, tformula_to_formula f2)
  | TNot f1 -> Not (tformula_to_formula f1)

let rec tqformula_to_qformula = function
  | TQFFormula f -> QFFormula (tformula_to_formula f)
  | TExists(tv, qf) -> Exists (tv.name, tv.ty, tqformula_to_qformula qf)

let ttrue = TQFFormula (0, TCmp (zero, LEQ, zero))
let tfalse = TQFFormula (0, TCmp (one, LEQ, zero))

let vars_of_tformula tf =
  let rec aux (_,f) =
    match f with
    | TCmp c -> Rewritting.vars_of_bconstraint c
    | TFVar v -> [v]
    | TEquiv (tf1, tf2) | TImply (tf1, tf2) | TAnd (tf1, tf2) | TOr (tf1, tf2) ->
        (aux tf1)@(aux tf2)
    | TNot tf -> aux tf
  in List.sort_uniq compare (aux tf)

let quantify env tf =
  let vars = vars_of_tformula tf in
  let rec aux = function
  | [] -> TQFFormula tf
  | v::vars ->
      let tqf = aux vars in
      begin
        match List.find_opt (fun tv -> tv.name = v) env with
        | None -> tqf
        | Some tv -> TExists (tv, tqf)
      end
  in aux vars

let rec quantifiers = function
  | TQFFormula _ -> []
  | TExists(tv,f) ->
      let tvars = quantifiers f in
      match List.find_opt (fun tv' -> tv.name = tv'.name) tvars with
      | None -> tv::tvars
      | Some tv' when tv.uid = tv'.uid && tv.ty = tv'.ty -> tvars
      | Some tv' -> raise (Wrong_modelling
          ("Two quantifiers on variable `" ^ tv.name ^ "` with two distinct types (`"
          ^ (string_of_tvar tv) ^ "` and `" ^ (string_of_tvar tv') ^ "`."))

let rec quantifier_free_of = function
  | TQFFormula tf -> tf
  | TExists(_,tqf) -> quantifier_free_of tqf

let rec map_formula next = function
  | TQFFormula tqf -> TQFFormula (next tqf)
  | TExists (tv,tqf) -> TExists (tv, map_formula next tqf)

let merge_formula make qf1 qf2 =
  let vars = (quantifiers qf1)@(quantifiers qf2) in
  let qf =
    map_formula (fun f1 ->
      quantifier_free_of (
        map_formula (fun f2 -> make f1 f2) qf2)) qf1
  in
    quantify vars (quantifier_free_of qf)

let rec q_conjunction uid = function
  | qf1::[] -> qf1
  | qf1::qfs ->
      let qf2 = q_conjunction uid qfs in
      merge_formula (fun f1 f2 -> (uid, TAnd(f1,f2))) qf1 qf2
  | [] -> ttrue
