(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Bounds
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

let string_of_tformula' env tf =
  let wrap uid parenthesis s =
    if parenthesis then  "(" ^ s ^ ")" ^ ":" ^ (string_of_int uid)
    else s ^ ":" ^ (string_of_int uid) in
  let rec aux (uid, f) =
    match f with
    | TFVar v -> wrap uid false v
    | TCmp c -> wrap uid false (Pretty_print.string_of_constraint c)
    | TAnd(tf1,tf2) -> binary_aux uid tf1 tf2 "/\\"
    | TEquiv(tf1,tf2) -> binary_aux uid tf1 tf2 "<=>"
    | TImply(tf1,tf2) -> binary_aux uid tf1 tf2 "=>"
    | TOr(tf1,tf2) -> binary_aux uid tf1 tf2 "\\/"
    | TNot tf1 -> wrap uid true ("not " ^ (aux tf1))
  and binary_aux uid tf1 tf2 op =
    wrap uid true ((aux tf1) ^ " " ^ op ^ " " ^ (aux tf2)) in
  let rec top_aux (uid, f) =
    match f with
    | TAnd(tf1, tf2) -> (top_aux tf1) ^ (top_aux tf2)
    | _ -> "constraint:" ^ (string_of_type env uid) ^ " " ^ (aux (uid,f)) ^ "\n" in
  top_aux tf

let string_of_tformula adty tf =
  let env = build_adenv adty in
  (string_of_adty_env env) ^ (string_of_tformula' env tf)

let string_of_tqformula adty tqf =
  let env = build_adenv adty in
  let rec aux = function
    | TExists(tv, tqf) ->
        "var:" ^ (string_of_type env tv.uid) ^ ":" ^ (Types.string_of_ty tv.ty) ^ " " ^ tv.name ^ "\n" ^
        aux tqf
    | TQFFormula tf -> "\n" ^ (string_of_tformula' env tf)
  in
  (string_of_adty_env env) ^ "\n" ^ (aux tqf)

let ctrue = TCmp (zero, LEQ, zero)
let cfalse = TCmp (zero, LEQ, zero)
let ctrue' = (0, ctrue)
let cfalse' = (0, cfalse)
let ttrue = TQFFormula ctrue'
let tfalse = TQFFormula cfalse'

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

let rec map_tformula next = function
  | TQFFormula tqf -> TQFFormula (next tqf)
  | TExists (tv,tqf) -> TExists (tv, map_tformula next tqf)

let merge_formula make qf1 qf2 =
  let vars = (quantifiers qf1)@(quantifiers qf2) in
  let qf =
    map_tformula (fun f1 ->
      quantifier_free_of (
        map_tformula (fun f2 -> make f1 f2) qf2)) qf1
  in
    quantify vars (quantifier_free_of qf)

let rec q_conjunction uid = function
  | qf1::[] -> qf1
  | qf1::qfs ->
      let qf2 = q_conjunction uid qfs in
      merge_formula (fun f1 f2 -> (uid, TAnd(f1,f2))) qf1 qf2
  | [] -> ttrue

let rec q_disjunction uid = function
  | qf1::[] -> qf1
  | qf1::qfs ->
      let qf2 = q_disjunction uid qfs in
      merge_formula (fun f1 f2 -> (uid, TOr(f1,f2))) qf1 qf2
  | [] -> tfalse

let neg_formula luid tf =
  let rec aux = function
    | uid, TCmp(e1, EQ, e2) ->
        luid, TOr((uid, TCmp(e1,LT,e2)), (uid, TCmp(e1, GT, e2)))
    | uid, TCmp c -> uid, TCmp (Rewritting.neg_bconstraint c)
    | uid, TFVar v -> uid, TNot (uid, TFVar v)
    | _, TEquiv (tf1,tf2) -> luid, TEquiv (aux tf1, tf2)
    | _, TImply (tf1,tf2) -> luid, TAnd (tf1, aux tf2)
    | _, TAnd (tf1,tf2) -> luid, TOr (aux tf1, aux tf2)
    | _, TOr (tf1,tf2) -> luid, TAnd (aux tf1, aux tf2)
    | _, TNot tf -> tf
  in aux tf

let replace_uid uid tf =
  let rec aux (u, f) =
    let uid = if u = fst tf then uid else u in
    match f with
    | TCmp c -> uid, TCmp c
    | TFVar v -> uid, TFVar v
    | TEquiv (tf1,tf2) -> uid, TEquiv (aux tf1, aux tf2)
    | TImply (tf1,tf2) -> uid, TImply (aux tf1, aux tf2)
    | TAnd (tf1,tf2) -> uid, TAnd (aux tf1, aux tf2)
    | TOr (tf1,tf2) -> uid, TOr (aux tf1, aux tf2)
    | TNot tf -> uid, TNot (aux tf)
  in aux tf

let instantiate_vars vars tf =
  let instantiate_vars_expr e =
    Rewritting.replace_var_in_expr (fun v ->
      try
        let tvar, value = List.assoc v vars in
        Cst (value, Types.to_concrete_ty tvar.ty)
      with Not_found -> Var v
    ) e in
  let rec aux (uid,f) =
    match f with
    | TCmp (e1,op,e2) ->
        uid, TCmp(instantiate_vars_expr e1, op, instantiate_vars_expr e2)
    | TFVar v ->
      begin
        try
          let _, value = List.assoc v vars in
          if Bound_rat.equal value Bound_rat.zero then
            cfalse'
          else ctrue'
        with Not_found -> uid, TFVar v
      end
    | TEquiv (tf1,tf2) -> uid, TEquiv (aux tf1, aux tf2)
    | TImply (tf1,tf2) -> uid, TImply (aux tf1, aux tf2)
    | TAnd (tf1,tf2) -> uid, TAnd (aux tf1, aux tf2)
    | TOr (tf1,tf2) -> uid, TOr (aux tf1, aux tf2)
    | TNot tf -> uid, TNot (aux tf) in
  aux tf
