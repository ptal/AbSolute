(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Lang.Ast
open Lang.Rewritting
open Tast
open Ad_type
open Core.Types

module Var2UID = Map.Make(struct type t=var let compare=compare end)
(* type var_env = (ad_uid list) Var2UID.t *)

(* Inference of the variables types. *)

let interval_can_represent_var vardom_ty ty =
  let is_integer = function
    | Interval Z | Interval_mix -> true
    | Interval_oc _ | Interval _ -> false (* Open-close interval only makes sense with float or rational. *) in
  let is_rational = function
    | Interval_oc Q | Interval Q -> true
    | _ -> false in (* NOTE: Interval_mix is implemented with floating point number for the real part. *)
  let is_float = function
    | Interval_oc F | Interval_mix | Interval F -> true
    | _ -> false in
  let is_real x = is_rational x || is_float x in
  match ty with
  | Concrete Int -> is_integer vardom_ty
  | Concrete Real -> is_real vardom_ty
  | Abstract Bool -> is_integer vardom_ty
  | Abstract (Machine Z) -> is_integer vardom_ty
  | Abstract (Machine Q) -> is_rational vardom_ty
  | Abstract (Machine F) -> is_float vardom_ty
  | Abstract VUnit -> false
  | Abstract (BDD _) -> false

let compatible_ty ty vty =
  match ty with
  | Concrete Int -> vty = Z
  | Concrete Real -> vty = F || vty = Q
  | Abstract (Machine ty) -> vty = ty
  | Abstract _ -> false

let rec infer_var ty adty =
  let is_boolean = function
    | Abstract Bool -> true
    | _ -> false in
  match adty with
  | (uid, Box vardom_ty) when interval_can_represent_var vardom_ty ty -> [uid]
  | (uid, Octagon vty) when compatible_ty ty vty -> [uid]
  | (uid, SAT) when is_boolean ty -> [uid]
  | (_, Direct_product adtys) -> List.flatten (List.map (infer_var ty) adtys)
  | (_, Logic_completion adty) -> infer_var ty adty
  | _ -> []

let rec infer_vars_ty adty = function
  | (TQFFormula _) as tf -> (Var2UID.empty, tf)
  | TExists (v, ty, uids, tf) ->
      let (env, tf) = infer_vars_ty adty tf in
      let uids = List.sort_uniq compare (uids@(infer_var ty adty)) in
      (Var2UID.add v uids env), TExists (v, ty, uids, tf)

(* Inference of the constraints types. *)

(* val box_infer: var_env -> tformula -> tformula *)

let belong uid env v =
  let uids = Var2UID.find v env in
  List.exists (fun u -> u = uid) uids

(** Infer type for ground abstract domain, only [term_infer] is given to type a term.
    It walks through conjunction `TAnd`, `TFVar` and call [term_infer] on `TCmp`. *)
let ground_dom_infer uid env term_infer tf =
  let rec aux = function
    | (uids, TAnd (tf1, tf2)) ->
        let tf1, tf2 = aux tf1, aux tf2 in
        if List.mem uid (fst tf1) && List.mem uid (fst tf2) then
          (uid::uids, TAnd(tf1,tf2))
        else
          (uids, TAnd(tf1,tf2))
    | (uids, TFVar v) when Var2UID.mem v env -> (uid::uids, TFVar v)
    | (uids, TCmp c) -> term_infer uids c
    | tf -> tf in
  aux tf

(** True if the variables of `c` are all treatable in the element `uid`. *)
let fully_defined_over uid env c =
  let vars = vars_of_bconstraint c in
  List.for_all (belong uid env) vars

let box_infer box_uid env tf =
  let term_infer uids c =
    if fully_defined_over box_uid env c then
      (box_uid::uids, TCmp c)
    else
      (uids, TCmp c)
  in ground_dom_infer box_uid env term_infer tf

let octagon_infer oct_uid env tf =
  let rec is_octagonal_term = function
    | Funcall(_, exprs) -> List.length (List.flatten (List.map get_vars_expr exprs)) = 0
    | Unary(NEG, e) -> is_octagonal_term e
    | Binary(e1,ADD,e2) | Binary(e1,SUB,e2) -> is_octagonal_term e1 && is_octagonal_term e2
    | Var _ | Cst _ -> true
    | _ -> false
  in
  let is_octagonal_constraint ((e1, op, e2) as c) =
    let vars = vars_of_bconstraint c in
    if List.length vars <= 2 then
      match op with
      | NEQ -> false
      | _ -> is_octagonal_term e1 && is_octagonal_term e2
    else
      false
  in
  let term_infer uids c =
    if fully_defined_over oct_uid env c && is_octagonal_constraint c then
      (oct_uid::uids, TCmp c)
    else
      (uids, TCmp c)
  in ground_dom_infer oct_uid env term_infer tf

(* A generic inference scheme for logical formula (useful for SAT and Logic completion).
   [literal] and [term] are functions called on [TFVar] and [TCmp] respectively. *)
let generic_formula_infer uid tf literal term =
  let rec aux (uids, f) =
    match f with
    | TFVar v -> literal v uids f
    | TCmp c -> term c uids f
    | TEquiv(tf1,tf2) -> binary_aux uids tf1 tf2 (fun tf1 tf2 -> TEquiv(tf1,tf2))
    | TImply(tf1,tf2) -> binary_aux uids tf1 tf2 (fun tf1 tf2 -> TImply(tf1,tf2))
    | TAnd(tf1,tf2) -> binary_aux uids tf1 tf2 (fun tf1 tf2 -> TAnd(tf1,tf2))
    | TOr(tf1,tf2) -> binary_aux uids tf1 tf2 (fun tf1 tf2 -> TOr(tf1,tf2))
    | TNot tf ->
        let (tf, can) = aux tf in
        if can then (uid::uids, TNot tf), true
        else (uids, TNot tf), false
  and binary_aux uids tf1 tf2 make =
    let (tf1, can1), (tf2, can2) = aux tf1, aux tf2 in
    let f = make tf1 tf2 in
    if can1 && can2 then (uid::uids, f), true
    else (uids, f), false
  in
    fst (aux tf)

let sat_infer sat_uid tf =
  generic_formula_infer sat_uid tf
    (fun _ uids f -> (sat_uid::uids, f), true)
    (fun _ uids f -> (uids, f), false)

let rec infer_constraints_ty env tf (uid, adty) =
  match adty with
  | Box _ -> box_infer uid env tf
  | Octagon _ -> octagon_infer uid env tf
  | SAT -> sat_infer uid tf
  | Direct_product adtys -> direct_product_infer uid env adtys tf
  | Logic_completion adty -> logic_completion_infer uid env adty tf
and direct_product_infer dp_uid env adtys tf =
  let tf = List.fold_left (infer_constraints_ty env) tf adtys in
  let adtys_uids = List.map fst adtys in
  let rec aux = function
    | (uids, TAnd(tf1,tf2)) as tf ->
        let (tf1,can1), (tf2, can2) = aux tf1, aux tf2 in
        if can1 && can2 then (dp_uid::uids, TAnd(tf1,tf2)), true
        else tf, false
    | (uids, _) as tf ->
        (* If there exists one sub-domain that can handle this formula, then the direct product can handle it.
           However, we do not assign `dp_uid` to this formula to avoid useless types (the interesting type is the one of the sub-domain). *)
        if List.exists (fun u -> List.exists (fun u' -> u = u') adtys_uids) uids then
          tf, true
        else tf, false
  in fst (aux tf)
and logic_completion_infer lc_uid env adty tf =
  let tf = infer_constraints_ty env tf adty in
  (* Whenever this term is typed in the sub-domain of the completion, the completion can handle it too. *)
  let lc_term _ uids f =
    if List.mem (fst adty) uids then (lc_uid::uids, f), true
    else (uids, f), false
  in
  generic_formula_infer lc_uid tf lc_term lc_term

(* III. Removes domain uids from variables if only unary (or no) constraints are involved with this variable in this domain.
     If a variable has only unary (or no) constraints, we only keep the least specialized (most efficient) domain. *)

module VarConsCounter = Map.Make(struct type t=var * ad_uid let compare=compare end)

(* We first create a structure storing the number of unary and nary constraints for each variable and domain uid. *)
let build_var_cons_map tf =
  let rec aux env (uids, f) =
    let add_unary v env uid =
      VarConsCounter.update (v,uid) (fun x ->
        match x with
        | Some (u,n) -> Some (u+1,n)
        | None -> Some(1,0)) env in
    let add_nary c env uid =
      let vars = vars_of_bconstraint c in
      if List.length vars = 1 then
        add_unary (List.hd vars) env uid
      else
        List.fold_left (fun env v ->
          VarConsCounter.update (v,uid) (fun x ->
            match x with
            | Some (u,n) -> Some (u,n+1)
            | None -> Some(0,1)) env) env vars in
    match f with
    | TFVar v -> List.fold_left (add_unary v) env uids
    | TCmp c -> List.fold_left (add_nary c) env uids
    | TEquiv(tf1,tf2) | TImply(tf1,tf2) | TAnd(tf1,tf2) | TOr(tf1,tf2) ->
        aux (aux env tf1) tf2
    | TNot tf -> aux env tf
  in aux VarConsCounter.empty tf

let select_vars tf =
  let rec extract_formula = function
    | TQFFormula tqf -> tqf
    | TExists (_,_,_,tqf) -> extract_formula tqf in
  let tqf = extract_formula tf in
  let vsm = build_var_cons_map tqf in
  let rec aux = function
    | TQFFormula tqf -> TQFFormula tqf
    | TExists (v,ty,uids,tqf) ->
        let tqf = aux tqf in
        let nary = List.fold_left
          (fun nary uid ->
            if snd (VarConsCounter.find (v,uid) vsm) > 0 then
              uid::nary
            else
              nary) [] uids in
        let uids = if (List.length nary) > 0 then nary else uids in
        TExists (v,ty,uids,tqf)
  in aux tf

(* IV. Select a single type for each sub-formula.
       If several abstract domain are on-par (unordered), only the first one is kept. *)

let rec map_tqf f = function
  | TQFFormula tqf -> TQFFormula (f tqf)
  | TExists (v,ty,uids,tf) -> TExists (v,ty,uids,map_tqf f tf)

module UID2Adty = Map.Make(struct type t=ad_uid let compare=compare end)

let build_adenv adty =
  let rec aux env ((uid, adty_) as adty) =
    match adty_ with
    | Box _ | Octagon _ | SAT -> UID2Adty.add uid adty env
    | Logic_completion adty' ->
        UID2Adty.add uid adty (aux env adty')
    | Direct_product adtys ->
        UID2Adty.add uid adty (List.fold_left aux env adtys)
  in aux UID2Adty.empty adty

let select_formula adty tf =
  let ad_env = build_adenv adty in
  let rec aux (uids, f) =
    let f = match f with
      | TFVar _ | TCmp _ -> f
      | TEquiv(tf1,tf2) -> TEquiv(aux tf1,aux tf2)
      | TImply(tf1,tf2) -> TImply(aux tf1,aux tf2)
      | TAnd(tf1,tf2) -> TAnd(aux tf1,aux tf2)
      | TOr(tf1,tf2) -> TOr(aux tf1,aux tf2)
      | TNot tf1 -> TNot (aux tf1)
    in
    let adtys = List.map (fun uid -> UID2Adty.find uid ad_env) uids in
    let (uid, _) = List.fold_left (fun current adty ->
        if (is_more_specialized current adty) <> False then current else adty
      ) (List.hd adtys) (List.tl adtys) in
    ([uid], f)
  in map_tqf aux tf

(* V. Assemble all the typing parts together. *)

let rec check_type_var = function
  | TQFFormula _ -> ()
  | TExists(v,_,[],_) ->
      raise (Wrong_modelling ("Variable `" ^ v ^ "` cannot be handled in any domain."))
  | TExists(_,_,_,tf) -> check_type_var tf

let infer_type adty tf =
  let (env, tf) = infer_vars_ty adty tf in
  let type_formula tf =
    map_tqf (fun tqf ->
      let tqf = infer_constraints_ty env tqf adty in
      if (fst tqf) = [] then
        raise (Wrong_modelling "The formula is not typable in the abstract domain provided.")
      else
        tqf) tf
  in
  let tf = type_formula tf in
  let tf = select_vars tf in
  check_type_var tf;
  select_formula adty tf
