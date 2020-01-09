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
open Core.Types
open Lang.Ast
open Lang.Rewritting
open Tast
open Ad_type

type inferred_type =
  | CannotType of string
  | Typed of ad_uid list

type iformula = inferred_type aformula
type iqformula = inferred_type aqformula

let uids_of' = function
  | CannotType _ -> []
  | Typed uids -> uids

let uids_of (ty,_) = uids_of' ty
let merge_ity ty1 ty2 =
  match ty1, ty2 with
  | CannotType msg1, CannotType msg2 -> CannotType (msg1 ^ "\n" ^ msg2)
  | CannotType msg, Typed []
  | Typed [],  CannotType msg -> CannotType msg
  | Typed uids, CannotType _ | CannotType _, Typed uids -> Typed uids
  | Typed uids1, Typed uids2 -> Typed (List.sort_uniq compare (uids1@uids2))

let is_uid_in uid tf = List.mem uid (uids_of tf)
let is_uid_in2 uid tf1 tf2 = is_uid_in uid tf1 && is_uid_in uid tf2

let rec formula_to_iformula f =
  let tf = match f with
    | FVar v -> TFVar v
    | Cmp c -> TCmp c
    | Equiv(f1, f2) -> TEquiv(formula_to_iformula f1, formula_to_iformula f2)
    | Imply(f1, f2) -> TImply(formula_to_iformula f1, formula_to_iformula f2)
    | And(f1, f2) -> TAnd(formula_to_iformula f1, formula_to_iformula f2)
    | Or(f1, f2) -> TOr(formula_to_iformula f1, formula_to_iformula f2)
    | Not f1 -> TNot (formula_to_iformula f1)
  in
  (Typed [], tf)

let rec qformula_to_iqformula = function
  | QFFormula f -> TQFFormula (formula_to_iformula f)
  | Exists(v, ty, qf) -> TExists (v, ty, Typed [], qformula_to_iqformula qf)

module Inference =
struct
  module Var2UID = Map.Make(struct type t=var let compare=compare end)
  type var_env = (ad_uid list) Var2UID.t

  type t = {
    trace: bool;
    (** `true` if we want to trace the reason why we cannot type a formula.
        If `false`, all strings `s` in `CannotType s` will be empty.
        This is mostly an optimization to avoid constructing error messages (which can be quite long) when the formula is typable. *)

    venv: var_env; (** Variable environment mapping a variable name to its supported abstract domains. *)

    adty: ad_ty; (** The abstract domain type, we try to infer a type for a formula matching this type. *)

    ad_env: ad_ty UID2Adty.t;
  }

  let init adty trace =
    {trace; venv = Var2UID.empty; adty; ad_env = (build_adenv adty)}

  (* Inference errors. *)

  let gen_err typer f = CannotType (if typer.trace then f () else "")

  let variable_not_in_dom_err typer v =
    gen_err typer (fun () -> "Variable `" ^ v ^ "` does not belong to the abstract domain.")

  let not_an_octagonal_constraint_err typer =
    gen_err typer (fun () -> "Constraint is not an octagonal constraint, so we cannot add it into octagon.")

  let ground_dom_does_not_handle_logic_connector_err typer =
    gen_err typer (fun () -> "Ground abstract domain does not support logic connectors other than conjunction.")

  let no_domain_support_this_variable_err typer v ty =
    gen_err typer (fun () -> "The variable `" ^ v ^ "` (type `" ^ (string_of_ty ty) ^ "`) is not supported in any abstract domain.")

  let sat_does_not_support_term_err typer =
    gen_err typer (fun () -> "SAT domain does not support term, only Boolean formulas are supported.")

  let direct_product_no_subdomain_err typer =
    gen_err typer (fun () -> "The formula is not supported in any of the sub-domain of the direct product.")

  let logic_completion_subdomain_failed_on_term_err typer =
    gen_err typer (fun () -> "Logic completion delegates the term typing to its sub-domain, but it could not type this term.")

  (* I. Inference of the variables types. *)

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

  let rec infer_vars_ty typer = function
    | (TQFFormula _) as tf -> (typer, tf)
    | TExists (v, ty, Typed [], tf) ->
        let (typer, tf) = infer_vars_ty typer tf in
        let uids = List.sort_uniq compare (infer_var ty typer.adty) in
        let res =
          if List.length uids > 0 then Typed uids
          else no_domain_support_this_variable_err typer v ty
        in
        {typer with venv=(Var2UID.add v uids typer.venv)}, TExists (v, ty, res, tf)
    | TExists (v,_,_,_) -> failwith
        ("infer_vars_ty: Existential connector of `" ^ v ^ "` is partly typed, which is not yet supported.")

  (* II. Inference of the constraints types. *)

  let belong typer uid v =
    let uids = Var2UID.find v typer.venv in
    List.exists (fun u -> u = uid) uids

  let bool_var_infer typer v uid =
    if Var2UID.mem v typer.venv then
      Typed [uid]
    else
      variable_not_in_dom_err typer v

  let ground_dom_infer typer uid term_infer tf =
    let rec aux typer (ty, f) =
      let (ty', f) =
        match f with
        | TFVar v -> merge_ity ty (bool_var_infer typer v uid), f
        | TCmp c -> (term_infer c, f)
        (* For conjunction, we type `TAnd` only if the inferred type is the same in both formula. *)
        | TAnd (tf1, tf2) ->
            let tf1, tf2 = aux typer tf1, aux typer tf2 in
            if is_uid_in2 uid tf1 tf2 then
              (merge_ity ty (Typed [uid]), TAnd(tf1,tf2))
            else
              (ty, TAnd(tf1,tf2))
        | TOr (tf1,tf2) -> binary_aux typer tf1 tf2 (fun tf1 tf2 -> TOr(tf1, tf2))
        | TImply (tf1,tf2) -> binary_aux typer tf1 tf2 (fun tf1 tf2 -> TImply(tf1, tf2))
        | TEquiv (tf1,tf2) -> binary_aux typer tf1 tf2 (fun tf1 tf2 -> TEquiv(tf1, tf2))
        | TNot tf1 -> (ground_dom_does_not_handle_logic_connector_err typer, TNot (aux typer tf1)) in
      (merge_ity ty ty', f)
    and binary_aux typer tf1 tf2 make =
      let tf1, tf2 = aux typer tf1, aux typer tf2 in
      (ground_dom_does_not_handle_logic_connector_err typer, make tf1 tf2) in
    aux typer tf

  let fully_defined_over typer uid c =
    let vars = vars_of_bconstraint c in
    List.find_opt (fun v -> not (belong typer uid v)) vars

  let box_infer typer box_uid tf =
    let term_infer c =
      match fully_defined_over typer box_uid c with
      | None -> Typed [box_uid]
      | Some v -> variable_not_in_dom_err typer v
    in ground_dom_infer typer box_uid term_infer tf

  let octagon_infer typer oct_uid tf =
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
    let term_infer c =
      if is_octagonal_constraint c then
        match fully_defined_over typer oct_uid c with
        | None -> Typed [oct_uid]
        | Some v -> variable_not_in_dom_err typer v
      else
        not_an_octagonal_constraint_err typer
    in ground_dom_infer typer oct_uid term_infer tf

  let generic_formula_infer uid tf literal term =
    let rec aux ((ty, f) as tf) =
      match f with
      | TFVar v -> merge_ity ty (literal v tf), TFVar v
      | TCmp c -> merge_ity ty (term c tf), TCmp c
      | TEquiv(tf1,tf2) -> binary_aux ty tf1 tf2 (fun tf1 tf2 -> TEquiv(tf1,tf2))
      | TImply(tf1,tf2) -> binary_aux ty tf1 tf2 (fun tf1 tf2 -> TImply(tf1,tf2))
      | TAnd(tf1,tf2) -> binary_aux ty tf1 tf2 (fun tf1 tf2 -> TAnd(tf1,tf2))
      | TOr(tf1,tf2) -> binary_aux ty tf1 tf2 (fun tf1 tf2 -> TOr(tf1,tf2))
      | TNot tf ->
          let tf = aux tf in
          if is_uid_in uid tf then
            (merge_ity ty (Typed [uid]), TNot tf)
          else
            (ty, TNot tf)
    and binary_aux ty tf1 tf2 make =
      let tf1, tf2 = aux tf1, aux tf2 in
      let f = make tf1 tf2 in
      if is_uid_in2 uid tf1 tf2 then
        (merge_ity ty (Typed [uid]), f)
      else
        (ty, f)
    in
      aux tf

  let sat_infer typer sat_uid tf =
    generic_formula_infer sat_uid tf
      (fun v _ -> bool_var_infer typer v sat_uid)
      (fun _ _ -> sat_does_not_support_term_err typer)

  let rec infer_constraints_ty typer tf (uid, adty) =
    match adty with
    | Box _ -> box_infer typer uid tf
    | Octagon _ -> octagon_infer typer uid tf
    | SAT -> sat_infer typer uid tf
    | Direct_product adtys -> direct_product_infer typer uid adtys tf
    | Logic_completion adty -> logic_completion_infer typer uid adty tf
  and direct_product_infer typer dp_uid adtys tf =
    (* (1) Attempt to give a type to the formula `tf` in every component individually. *)
    let tf = List.fold_left (infer_constraints_ty typer) tf adtys in
    let adtys_uids = List.map fst adtys in
    (* (2) The next step is to give the type `dp_uid` to formula of the form `f1:t1 /\ f2:t2` if t1,t2 are in the product, and t1 != t2. *)
    let rec aux (ty, f) =
      match f with
      | TAnd(tf1,tf2) ->
          let tf1, tf2 = aux tf1, aux tf2 in
          let f = TAnd(tf1, tf2) in
          let uids1, uids2 = uids_of tf1, uids_of tf2 in
          let ty' =
            if is_uid_in2 dp_uid tf1 tf2 then
              Typed [dp_uid]
            else
              let u1 = Tools.intersect adtys_uids uids1 in
              let u2 = Tools.intersect adtys_uids uids2 in
              let common = Tools.intersect u1 u2 in
              if List.length common > 0 then Typed common
              else
                (* The sub-formulas tf1 and tf2 can be interpreted in different sub-domains of the product. *)
                if List.length u1 > 0 && List.length u2 > 0 then
                  Typed [dp_uid]
                else
                  direct_product_no_subdomain_err typer
            in
            (merge_ity ty ty'), f
      | _ ->
          (* If there exists one sub-domain that can handle this formula, then the direct product can handle it.
             However, we do not assign `dp_uid` to this formula, if we did, it means we want this formula to be treated in every domain of the product (redundant information).
             Redundant constraints might be interesting to explore, but this is for future work (at least the typing framework already support it). *)
          match ty with
          | Typed uids when List.length (Tools.intersect adtys_uids uids) > 0 -> ty, f
          | _ -> merge_ity ty (direct_product_no_subdomain_err typer), f
    in aux tf
  and logic_completion_infer typer lc_uid adty tf =
    let tf = infer_constraints_ty typer tf adty in
    (* Whenever this term is typed in the sub-domain of the completion, the completion can handle it too. *)
    let lc_term _ tf =
      if List.mem (fst adty) (uids_of tf) then Typed [lc_uid]
      else logic_completion_subdomain_failed_on_term_err typer
    in
    generic_formula_infer lc_uid tf lc_term lc_term

  let infer_constraints_ty_or_fail typer tf =
    map_tqf (fun tqf ->
      (* We first try to type the formula without tracing the errors. *)
      let typer = {typer with trace=false} in
      let tqf = infer_constraints_ty typer tqf typer.adty in
      match fst tqf with
      | CannotType _ ->
          let typer = {typer with trace=true} in
          let tqf = infer_constraints_ty typer tqf typer.adty in
          (match fst tqf with
          | CannotType msg -> raise (Wrong_modelling msg)
          | Typed _ -> failwith "CannotType with trace=false, and Typed with trace=true, `trace` should not impact typing.")
      | Typed _ -> tqf
    ) tf

  (* III. Variable's type instantiatino. *)

  module VarConsCounter = Map.Make(struct type t=var * ad_uid let compare=compare end)

  let build_var_cons_map tf =
    let rec aux env tf =
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
      match tf with
      | Typed uids, TFVar v -> List.fold_left (add_unary v) env uids
      | Typed uids, TCmp c -> List.fold_left (add_nary c) env uids
      | _, TEquiv(tf1,tf2) | _, TImply(tf1,tf2) | _, TAnd(tf1,tf2) | _, TOr(tf1,tf2) ->
          aux (aux env tf1) tf2
      | _, TNot tf -> aux env tf
      | CannotType _, _ ->
          failwith "CannotType cannot occurs in build_var_cons_map (it is checked by `infer_constraints_ty_or_fail` before)."
    in aux VarConsCounter.empty tf

  let instantiate_var_ty tf =
    let rec extract_formula = function
      | TQFFormula tqf -> tqf
      | TExists (_,_,_,tqf) -> extract_formula tqf in
    let tqf = extract_formula tf in
    let vsm = build_var_cons_map tqf in
    let rec aux = function
      | TQFFormula tqf -> TQFFormula tqf
      | TExists (v,ty,Typed uids,tqf) ->
          let tqf = aux tqf in
          let nary = List.fold_left
            (fun nary uid ->
              if snd (VarConsCounter.find (v,uid) vsm) > 0 then
                uid::nary
              else
                nary) [] uids in
          let uids = if (List.length nary) > 0 then nary else uids in
          TExists (v,ty,Typed uids,tqf)
      | TExists (_,_,CannotType _,_) -> failwith "instantiate_var_ty: Reached a CannotType, but should be checked before in `check_type_var`."
    in aux tf

  (* IV. Instantiation of the formula type. *)

  let instantiate_formula_ty typer tf =
    let rec aux = function
      | TQFFormula tqf -> TQFFormula(
          map_annot_aformula tqf (fun ty ->
            let adtys = List.map (fun uid -> UID2Adty.find uid typer.ad_env) (uids_of' ty) in
            if adtys = [] then failwith "Type uids should not be empty (should be checked in `infer_constraints_ty_or_fail`)";
            fst (List.fold_left (fun current adty ->
                if (is_more_specialized current adty) <> False then current else adty
              ) (List.hd adtys) (List.tl adtys)
          )))
      | TExists (v,ty,Typed [uid],tf) -> TExists (v,ty,uid,aux tf)
      | TExists _ -> failwith "instantiate_formula_ty: Variable type should be instantiated in `instantiate_var_ty`."
    in aux tf

  let rec check_type_var = function
    | TQFFormula _ -> ()
    | TExists(_,_,CannotType msg,_) ->
        raise (Wrong_modelling msg)
    | TExists(_,_,Typed [],_) ->
        failwith "Empty list of UIDs: we should either give a type to the variable, or `CannotType`."
    | TExists(_,_,_,tf) -> check_type_var tf
end

let infer_type adty f =
  let open Inference in
  let tf = qformula_to_iqformula f in
  let typer = init adty true in
  let (typer, tf) = infer_vars_ty typer tf in
  check_type_var tf;
  let tf = infer_constraints_ty_or_fail typer tf in
  let tf = instantiate_var_ty tf in
  instantiate_formula_ty typer tf
