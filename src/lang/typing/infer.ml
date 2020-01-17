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
open Lang.Pretty_print
open Tast
open Aast
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
  | CannotType msg1, CannotType msg2 -> CannotType (
      msg1 ^ (if msg1 = "" || msg2 = "" then "" else "\n") ^ msg2)
  | CannotType msg, Typed []
  | Typed [],  CannotType msg -> CannotType msg
  | Typed uids, CannotType _ | CannotType _, Typed uids -> Typed uids
  | Typed uids1, Typed uids2 -> Typed (List.sort_uniq compare (uids1@uids2))

let is_uid_in uid tf = List.mem uid (uids_of tf)
let is_uid_in2 uid tf1 tf2 = is_uid_in uid tf1 && is_uid_in uid tf2

let rec formula_to_iformula f =
  let tf = match f with
    | FVar v -> AFVar v
    | Cmp c -> ACmp c
    | Equiv(f1, f2) -> AEquiv(formula_to_iformula f1, formula_to_iformula f2)
    | Imply(f1, f2) -> AImply(formula_to_iformula f1, formula_to_iformula f2)
    | And(f1, f2) -> AAnd(formula_to_iformula f1, formula_to_iformula f2)
    | Or(f1, f2) -> AOr(formula_to_iformula f1, formula_to_iformula f2)
    | Not f1 -> ANot (formula_to_iformula f1)
  in
  (Typed [], tf)

let rec qformula_to_iqformula = function
  | QFFormula f -> AQFFormula (formula_to_iformula f)
  | Exists(v, ty, qf) -> AExists (v, ty, Typed [], qformula_to_iqformula qf)

module Inference =
struct
  module Var2UID = Map.Make(struct type t=vname let compare=compare end)
  type var_env = (ad_uid list) Var2UID.t

  type t = {
    trace: bool;
    (** `true` if we want to trace the reason why we cannot type a formula.
        If `false`, all strings `s` in `CannotType s` will be empty.
        This is mostly an optimization to avoid constructing error messages (which can be quite long) when the formula is typable. *)

    debug: bool;
    (** If set to `true`, we have a trace of the typing process.
        It is useful to debug the inference process. *)

    indent: int;
    (** Useful for clear debugging messages. *)

    venv: var_env; (** Variable environment mapping a variable name to its supported abstract domains. *)

    adty: ad_ty; (** The abstract domain type, we try to infer a type for a formula matching this type. *)

    ad_env: ad_ty UID2Adty.t;
  }

  let init adty trace =
    {trace; debug=false; indent=0; venv = Var2UID.empty; adty; ad_env = (build_adenv adty)}

  (* Debugging facilities. *)

  let rec make_indent = function
    | 0 -> ""
    | x -> " " ^ (make_indent (x-1))

  let indent typer = { typer with indent=(typer.indent+2) }

  let debug typer make_msg =
    if typer.debug then
      Printf.printf "%s%s\n" (make_indent typer.indent) (make_msg ())
    else ()

  let string_of_adtys adtys =
    let rec aux = function
      | [] -> ""
      | x::l -> (string_of_adty x) ^ (if l <> [] then " ; " else "") ^ (aux l)
    in
    if List.length adtys = 1 then aux adtys
    else "[" ^ (aux adtys) ^ "]"

  let string_of_ity typer = function
    | CannotType msg -> "Error(" ^ msg ^ ")"
    | Typed ad_uids ->
        string_of_adtys (List.map (fun x -> UID2Adty.find x typer.ad_env) ad_uids)

  let debug_adty typer make_msg adty =
    if typer.debug then
      Printf.printf "%s%s %s\n" (make_indent typer.indent) (make_msg ()) (string_of_adty adty)
    else ()

  let debug_ty typer term ty =
    if typer.debug then
      Printf.printf "%s%s:%s\n" (make_indent typer.indent)
        (term ()) (string_of_ity typer ty)
    else ()

  (* Inference errors. *)

  let ad_name typer uid = string_of_adty (UID2Adty.find uid typer.ad_env)

  let gen_err typer uid f =
    CannotType
      (if typer.trace then
        "[" ^ (ad_name typer uid) ^ "@" ^ (string_of_int uid) ^ "] " ^ f ()
      else
        "")

  let variable_not_in_dom_err typer uid v =
    gen_err typer uid (fun () -> "Variable `" ^ v ^ "` does not belong to the abstract domain.")

  let not_an_octagonal_constraint_err typer uid =
    gen_err typer uid (fun () -> "Constraint is not an octagonal constraint, so we cannot add it into octagon.")

  let ground_dom_does_not_handle_logic_connector_err typer uid =
    gen_err typer uid (fun () -> "Ground abstract domain does not support logic connectors other than conjunction.")

  let no_domain_support_this_variable_err typer uid v ty =
    gen_err typer uid (fun () -> "The variable `" ^ v ^ "` (type `" ^ (string_of_ty ty) ^ "`) is not supported in any abstract domain.")

  let sat_does_not_support_term_err typer uid =
    gen_err typer uid (fun () -> "SAT domain does not support term, only Boolean formulas are supported.")

  let direct_product_no_subdomain_err typer uid msg =
    gen_err typer uid (fun () -> "The formula is not supported in any of the sub-domain of the direct product because:\n"
      ^ (Tools.indent msg))

  let logic_completion_subdomain_failed_on_term_err typer uid =
    gen_err typer uid (fun () -> "Logic completion delegates the term typing to its sub-domain, but it could not type this term.")

  let no_var_mgad_err v adtys =
    raise (Wrong_modelling(
      "Variable `" ^ v ^ "` must be interpreted in several abstract elements, but there is not a most general one.\n"
      ^ "For instance, if a variable exists in two abstract elements, e.g. Box and Oct, we must have a type Box X Oct that takes care of mapping this variable in both domains.\n"
      ^ "Note that some abstract domains such as projection-based product take care of synchronizing the variables of both domains.\n"
      ^ "  Candidate abstract domains: " ^ (string_of_adtys adtys)
    ))

  let create_typing_error msg tf =
    let cannot_type_formula msg f =
      "Cannot type the following formula: `" ^ (string_of_aformula (CannotType msg, f)) ^ "` because:\n"
        ^ (Tools.indent msg) in
    let rec aux msg f =
      match f with
      | AFVar v -> "Cannot type of the variable `" ^ v ^ "` because:\n"
          ^ (Tools.indent msg)
      | ACmp c -> "Cannot type the following constraint: `" ^ (string_of_constraint c) ^ "` because:\n"
          ^ (Tools.indent msg)
      | AAnd (tf1, tf2)
      | AOr (tf1,tf2)
      | AImply (tf1,tf2)
      | AEquiv (tf1,tf2) ->
          (match tf1, tf2 with
          | ((CannotType msg, f1), _) -> aux msg f1
          | (_, (CannotType msg, f2)) -> aux msg f2
          | _ -> cannot_type_formula msg f)
      | ANot (CannotType msg, tf1) -> aux msg tf1
      | ANot _ -> cannot_type_formula msg f
    in aux msg (snd tf)

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
    | Abstract Bool -> vty = Z
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

  let build_venv typer tf =
    let rec aux = function
    | AQFFormula _ -> Var2UID.empty
    | AExists(v, _, Typed uids, tf) -> Var2UID.add v uids (aux tf)
    | _ -> failwith "build_venv: `check_var_ty` should be called before."
    in
      {typer with venv=(aux tf)}

  let rec check_type_var = function
    | AQFFormula _ -> ()
    | AExists(_,_,CannotType msg,_) ->
        raise (Wrong_modelling msg)
    | AExists(_,_,Typed [],_) ->
        failwith "Empty list of UIDs: we should either give a type to the variable, or `CannotType`."
    | AExists(_,_,_,tf) -> check_type_var tf

  let infer_vars_ty typer tf =
    debug typer (fun () -> "I. Typing of the variables\n");
    let rec aux typer = function
      | (AQFFormula _) as tf -> tf
      | AExists (v, ty, Typed [], tf) ->
          let tf = aux typer tf in
          let uids = List.sort_uniq compare (infer_var ty typer.adty) in
          let res =
            if List.length uids > 0 then Typed uids
            else no_domain_support_this_variable_err typer 0 v ty
          in
          debug_ty typer (fun () -> v) res;
          AExists (v, ty, res, tf)
      | AExists (v,_,_,_) -> failwith
          ("infer_vars_ty: Existential connector of `" ^ v ^ "` is partly typed, which is not yet supported.")
    in
      let tf = aux typer tf in
      check_type_var tf;
      let typer = build_venv typer tf in
      (typer, tf)

  (* II. Inference of the constraints types. *)

  let belong typer uid v =
    let uids = Var2UID.find v typer.venv in
    List.exists (fun u -> u = uid) uids

  let bool_var_infer typer v uid =
    if Var2UID.mem v typer.venv then
      Typed [uid]
    else
      variable_not_in_dom_err typer uid v

  let ground_dom_infer typer uid term_infer tf =
    let rec aux typer (ty, f) =
      let (ty', f) =
        match f with
        | AFVar v -> merge_ity ty (bool_var_infer typer v uid), f
        | ACmp c -> (term_infer c, f)
        (* For conjunction, we type `TAnd` only if the inferred type is the same in both formula. *)
        | AAnd (tf1, tf2) ->
            let tf1, tf2 = aux typer tf1, aux typer tf2 in
            if is_uid_in2 uid tf1 tf2 then
              (merge_ity ty (Typed [uid]), AAnd(tf1,tf2))
            else
              (ty, AAnd(tf1,tf2))
        | AOr (tf1,tf2) -> binary_aux typer tf1 tf2 (fun tf1 tf2 -> AOr(tf1, tf2))
        | AImply (tf1,tf2) -> binary_aux typer tf1 tf2 (fun tf1 tf2 -> AImply(tf1, tf2))
        | AEquiv (tf1,tf2) -> binary_aux typer tf1 tf2 (fun tf1 tf2 -> AEquiv(tf1, tf2))
        | ANot tf1 -> (ground_dom_does_not_handle_logic_connector_err typer uid, ANot (aux typer tf1)) in
      let tf = (merge_ity ty ty', f) in
      let _ = debug_ty typer (fun () -> string_of_aformula tf) (fst tf) in
      tf
    and binary_aux typer tf1 tf2 make =
      let tf1, tf2 = aux typer tf1, aux typer tf2 in
      (ground_dom_does_not_handle_logic_connector_err typer uid, make tf1 tf2) in
    aux typer tf

  let fully_defined_over typer uid c =
    let vars = vars_of_bconstraint c in
    List.find_opt (fun v -> not (belong typer uid v)) vars

  let box_infer typer box_uid tf =
    let term_infer c =
      match fully_defined_over typer box_uid c with
      | None -> Typed [box_uid]
      | Some v -> variable_not_in_dom_err typer box_uid v
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
        | Some v -> variable_not_in_dom_err typer oct_uid v
      else
        not_an_octagonal_constraint_err typer oct_uid
    in ground_dom_infer typer oct_uid term_infer tf

  let generic_formula_infer typer uid tf literal term =
    let rec aux ((ty, f) as tf) =
      let tf = match f with
        | AFVar v -> merge_ity ty (literal v tf), AFVar v
        | ACmp c -> merge_ity ty (term c tf), ACmp c
        | AEquiv(tf1,tf2) -> binary_aux ty tf1 tf2 (fun tf1 tf2 -> AEquiv(tf1,tf2))
        | AImply(tf1,tf2) -> binary_aux ty tf1 tf2 (fun tf1 tf2 -> AImply(tf1,tf2))
        | AAnd(tf1,tf2) -> binary_aux ty tf1 tf2 (fun tf1 tf2 -> AAnd(tf1,tf2))
        | AOr(tf1,tf2) -> binary_aux ty tf1 tf2 (fun tf1 tf2 -> AOr(tf1,tf2))
        | ANot tf ->
            let tf = aux tf in
            if is_uid_in uid tf then
              (merge_ity ty (Typed [uid]), ANot tf)
            else
              (ty, ANot tf)
      in
        let _ = debug_ty typer (fun () -> string_of_aformula tf) (fst tf) in
        tf
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
    generic_formula_infer typer sat_uid tf
      (fun v _ -> bool_var_infer typer v sat_uid)
      (fun _ _ -> sat_does_not_support_term_err typer sat_uid)

  let rec infer_constraints_ty typer tf (uid, adty) =
    debug_adty typer (fun () -> "Typing of `" ^ (string_of_aformula tf) ^ "` with abstract domain") (uid, adty);
    let typer = indent typer in
    match adty with
    | Box _ -> box_infer typer uid tf
    | Octagon _ -> octagon_infer typer uid tf
    | SAT -> sat_infer typer uid tf
    | Direct_product adtys -> direct_product_infer typer uid tf adtys
    | Logic_completion adty -> logic_completion_infer typer uid tf adty
  and direct_product_infer typer dp_uid tf adtys =
    (* (1) Attempt to give a type to the formula `tf` in every component individually. *)
    let tf = List.fold_left (infer_constraints_ty typer) tf adtys in
    let adtys_uids = List.map fst adtys in
    (* (2) The next step is to give the type `dp_uid` to formulas of the form `f1:t1 /\ f2:t2` if t1,t2 are in the product, and t1 != t2. *)
    let rec aux (ty, f) =
      match f with
      | AAnd(tf1,tf2) ->
          let tf1, tf2 = aux tf1, aux tf2 in
          let f = AAnd(tf1, tf2) in
          let uids1, uids2 = uids_of tf1, uids_of tf2 in
          let ty' =
            if is_uid_in2 dp_uid tf1 tf2 then
              Typed [dp_uid]
            else
              let u1 = Tools.intersect adtys_uids uids1 in
              let u2 = Tools.intersect adtys_uids uids2 in
              let common = Tools.intersect u1 u2 in
              (* The sub-formulas tf1 and tf2 can be interpreted in different sub-domains of the product. *)
              if List.length u1 > 0 && List.length u2 > 0 then
                Typed (dp_uid::common)
              else
                CannotType ("Direct product could not type a conjunction c1 /\\ c2 because " ^
                  (match List.length u1, List.length u2 with
                   | 0, 0 -> "none of the sub-formula could be treated in any sub-domain."
                   | x, 0 when x > 0 -> "c2 could not be treated in any sub-domain"
                   | 0, x when x > 0 -> "c1 could not be treated in any sub-domain"
                   | _ -> failwith "unreachable"))
            in
            (merge_ity ty ty'), f
      | _ ->
          (* If there exists one sub-domain that can handle this formula, then the direct product can handle it.
             However, we do not assign `dp_uid` to this formula, if we did, it means we want this formula to be treated in every domain of the product (redundant information).
             Redundant constraints might be interesting to explore, but this is for future work (at least the typing framework already support it). *)
          ty,f
(*           match ty with
          | Typed uids when List.length (Tools.intersect adtys_uids uids) > 0 -> ty, f
          | _ -> ty, f *)
    in
      match aux tf with
      | CannotType msg, f -> direct_product_no_subdomain_err typer dp_uid msg, f
      | tf ->
          let _ = debug_ty typer (fun () -> string_of_aformula tf) (fst tf) in
          tf
  and logic_completion_infer typer lc_uid tf adty =
    let tf = infer_constraints_ty (indent typer) tf adty in
    (* Whenever this term is typed in the sub-domain of the completion, the completion can handle it too. *)
    let lc_term _ tf =
      if is_mgad adty (uids_of tf) then Typed [lc_uid]
      else logic_completion_subdomain_failed_on_term_err typer lc_uid
    in
    generic_formula_infer typer lc_uid tf lc_term lc_term

  let infer_constraints_ty_or_fail typer tf =
    debug typer (fun () -> "\nII. Typing of the sub-formulas & constraints.\n");
    map_tqf (fun tqf ->
      (* We first try to type the formula without tracing the errors. *)
      let typer = {typer with trace=false} in
      let tqf = infer_constraints_ty typer tqf typer.adty in
      match fst tqf with
      | CannotType _ ->
          debug typer (fun () -> "Typing of the formula failed, so we now create the error message and trace the typing process.");
          let typer = {typer with trace=true; debug=false} in
          let tqf = infer_constraints_ty typer tqf typer.adty in
          (match fst tqf with
          | CannotType msg -> raise (Wrong_modelling (create_typing_error msg tqf))
          | Typed _ -> failwith "CannotType with trace=false, and Typed with trace=true, `trace` should not impact typing.")
      | Typed _ -> tqf
    ) tf

  (* III. Variable's type restriction. *)

  module VarConsCounter = Map.Make(struct type t=vname * ad_uid let compare=compare end)

  let build_var_cons_map tf uids_of =
    let rec aux vcm tf =
      let add_unary v vcm uid =
        VarConsCounter.update (v,uid) (fun x ->
          match x with
          | Some (u,n) -> Some (u+1,n)
          | None -> Some(1,0)) vcm in
      let add_nary c vcm uid =
        let vars = vars_of_bconstraint c in
        if List.length vars = 1 then
          add_unary (List.hd vars) vcm uid
        else
          List.fold_left (fun vcm v ->
            VarConsCounter.update (v,uid) (fun x ->
              match x with
              | Some (u,n) -> Some (u,n+1)
              | None -> Some(0,1)) vcm) vcm vars in
      match tf with
      | ty, AFVar v -> List.fold_left (add_unary v) vcm (uids_of ty)
      | ty, ACmp c -> List.fold_left (add_nary c) vcm (uids_of ty)
      | _, AEquiv(tf1,tf2) | _, AImply(tf1,tf2) | _, AAnd(tf1,tf2) | _, AOr(tf1,tf2) ->
          aux (aux vcm tf1) tf2
      | _, ANot tf -> aux vcm tf
    in aux VarConsCounter.empty tf

  let restrict_unary_var_dom typer uids =
    let is_octagon uid =
      match UID2Adty.find uid typer.ad_env with
      | _, Octagon _ -> true
      | _ -> false in
    let rec remove_octagon = function
      | [] -> []
      | x::l when is_octagon x -> l
      | x::l -> x::(remove_octagon l) in
    if List.length uids > 1 then remove_octagon uids else uids

  let rec extract_formula = function
    | AQFFormula tqf -> tqf
    | AExists (_,_,_,tqf) -> extract_formula tqf

  let restrict_variable_ty typer tf =
    debug typer (fun () -> "\nIII. Restrict variables' types\n");
    let tqf = extract_formula tf in
    let vcm = build_var_cons_map tqf uids_of' in
    let rec aux = function
      | AQFFormula tqf -> AQFFormula tqf
      | AExists (v,ty,Typed uids,tqf) ->
          let nary = List.filter
            (fun uid -> snd (VarConsCounter.find (v,uid) vcm) > 0)
            uids in
          let uids =
            if (List.length nary) > 0 then nary
            else restrict_unary_var_dom typer uids in
          debug_ty typer (fun () -> v) (Typed uids);
          AExists (v,ty,Typed uids,aux tqf)
      | AExists (_,_,CannotType _,_) -> failwith "restrict_variable_ty: Reached a CannotType, but should be checked before in `check_type_var`."
    in
      let tf = aux tf in
      let typer = build_venv typer tf in
      (typer, tf)

  (* V. Instantiation of the formula type. *)

  (* This function is applied after `instantiate_formula_ty`, therefore the constraints' types have already been instantiated.
     Since we instantiate the variable with its most general abstract domain, the constraints stay well-typed. *)
  let instantiate_var_ty typer vcm vname uids =
    let useful_uids = List.filter
      (fun uid ->
        let u,n = try VarConsCounter.find (vname,uid) vcm with Not_found -> (0,0) in
        u > 0 || n > 0) uids in
    let adtys = List.map (fun uid -> UID2Adty.find uid typer.ad_env) useful_uids in
    match select_mgad adtys with
    | None -> no_var_mgad_err vname adtys
    | Some adty -> fst adty

  let sort_most_specialized typer uids =
    (* We want the list to be sorted with the most specialized first (so we rank it "smaller" in the compare function). *)
    let compare_specialization ty1 ty2 =
      if ty1 = ty2 then 0
      else
        match is_more_specialized ty1 ty2 with
        | True -> -1
        | False -> 1
        | Unknown -> 0 (* Unrelated domains are considered equal here. *)
    in
    let adtys = List.map (fun uid -> UID2Adty.find uid typer.ad_env) uids in
    if adtys = [] then failwith "Type uids should not be empty (should be checked in `infer_constraints_ty_or_fail`)";
    let adtys = List.stable_sort compare_specialization adtys in
    List.map fst adtys

  let most_specialized typer uids = List.hd (sort_most_specialized typer uids)

  (** From a sorted list of UIDS available, pick the first one that is a MGAD w.r.t. the UIDs in the provided list. *)
  let first_supporting typer need_to_support_uids uids_available =
    let rec aux = function
      | [] -> failwith "Could not find an abstract domain supporting sub-formula, this should be checked in `infer_constraints_ty_or_fail`."
      | uid::l ->
          let adty = UID2Adty.find uid typer.ad_env in
          if is_mgad adty need_to_support_uids then uid
          else aux l
    in aux uids_available

  let instantiate_formula_ty typer tf =
    let rec aux tf =
      let (uid,f) = match tf with
        | Typed uids, AFVar v ->
            let var_uids = Var2UID.find v typer.venv in
            let uids = Tools.intersect uids var_uids in
            (most_specialized typer uids, AFVar v)
        | Typed uids, ACmp c -> (most_specialized typer uids, ACmp c)
        | Typed uids, AAnd (tf1, tf2) -> binary_aux uids tf1 tf2 (fun tf1 tf2 -> AAnd(tf1, tf2))
        | Typed uids, AOr (tf1,tf2) -> binary_aux uids tf1 tf2 (fun tf1 tf2 -> AOr(tf1, tf2))
        | Typed uids, AImply (tf1,tf2) -> binary_aux uids tf1 tf2 (fun tf1 tf2 -> AImply(tf1, tf2))
        | Typed uids, AEquiv (tf1,tf2) -> binary_aux uids tf1 tf2 (fun tf1 tf2 -> AEquiv(tf1, tf2))
        | Typed uids, ANot tf ->
            let (uid1, tf1) = aux tf in
            let sorted_uids = sort_most_specialized typer uids in
            (first_supporting typer [uid1] sorted_uids, ANot (uid1,tf1))
        | _ -> failwith "instantiate_formula_ty: Formula should all be typed after the call to `infer_constraints_ty_or_fail`."
      in
        (* debug_ty typer (fun () -> string_of_aformula tf) (fst tf); *)
        debug_ty typer (fun () -> string_of_aformula tf) (Typed [uid]);
        (uid,f)
    and binary_aux uids tf1 tf2 make =
      let (uid1, tf1), (uid2, tf2) = aux tf1, aux tf2 in
      let sorted_uids = sort_most_specialized typer uids in
      (first_supporting typer [uid1; uid2] sorted_uids, make (uid1,tf1) (uid2,tf2))
    in
      aux tf

  let instantiate_qformula_ty typer tf =
    debug typer (fun () -> "\nIV. Instantiate the type of formula\n");
    let rec aux = function
      | AQFFormula tqf ->
          let tqf = instantiate_formula_ty typer tqf in
          AQFFormula tqf, build_var_cons_map tqf (fun uid -> [uid])
      | AExists (vname,ty,Typed uids,tf) ->
          let tf, vcm = aux tf in
          let uid = instantiate_var_ty typer vcm vname uids in
          AExists (vname, ty, uid, tf), vcm
      | AExists (_,_,CannotType msg, _) -> raise (Wrong_modelling msg)
    in fst (aux tf)
end

let make_tqformula tf =
  let rec aux' = function
    | uid, AFVar v -> (uid, TFVar v)
    | uid, ACmp c -> (uid, TCmp c)
    | uid, AAnd (tf1, tf2) -> (uid, TAnd(aux' tf1, aux' tf2))
    | uid, AOr (tf1,tf2) -> (uid, TOr(aux' tf1, aux' tf2))
    | uid, AImply (tf1,tf2) -> (uid, TImply(aux' tf1, aux' tf2))
    | uid, AEquiv (tf1,tf2) -> (uid, TEquiv(aux' tf1, aux' tf2))
    | uid, ANot tf -> (uid, TNot (aux' tf)) in
  let rec aux = function
    | AQFFormula tqf -> TQFFormula (aux' tqf)
    | AExists (name, ty, uid, tf) ->
        TExists ({name; ty; uid}, aux tf)
  in aux tf

let infer_type adty f =
  let open Inference in
  let tf = qformula_to_iqformula f in
  let typer = init adty true in
  let (typer, tf) = infer_vars_ty typer tf in
  let tf = infer_constraints_ty_or_fail typer tf in
  let (typer, _) = restrict_variable_ty typer tf in
  let tf = instantiate_qformula_ty typer tf in
  make_tqformula tf
