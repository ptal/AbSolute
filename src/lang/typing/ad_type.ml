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
open Core.Kleene

type ad_uid = int

type vardom_ty =
  | Interval of value_ty
  | Interval_oc of value_ty
  | Interval_mix

type ad_ty_ =
  | Box of vardom_ty
  | Octagon of value_ty
  | SAT
  | Logic_completion of ad_ty
  | Direct_product of ad_ty list
and ad_ty = ad_uid * ad_ty_

let string_of_vardom_ty = function
  | Interval vty -> "Itv(" ^ (string_of_value_ty_short vty) ^ ")"
  | Interval_oc vty -> "ItvOC(" ^ (string_of_value_ty_short vty) ^ ")"
  | Interval_mix -> "ItvMix"

let string_of_adty adty =
  let rec aux top_product (_,ty) =
    match ty with
    | Box vardom_ty -> "Box(" ^ (string_of_vardom_ty vardom_ty) ^ ")"
    | Octagon vty -> "Oct(" ^ (string_of_value_ty_short vty) ^ ")"
    | SAT -> "SAT"
    | Logic_completion adty -> "LC(" ^ (aux false adty) ^ ")"
    | Direct_product adtys ->
        let sty =
          List.fold_left (fun msg adty -> msg ^ " X " ^ (aux false adty))
            (aux false (List.hd adtys)) (List.tl adtys) in
        if top_product then sty else "(" ^ sty ^ ")"
  in aux true adty

let rec is_more_specialized_value vty1 vty2 =
  if vty1 = vty2 then True
  else match vty1, vty2 with
    | Z, _ -> Unknown
    | Q, F -> True
    | _ -> not_kleene (is_more_specialized_value vty2 vty1)

let rec is_more_specialized_vardom vd1 vd2 =
  if vd1 = vd2 then True
  else match vd1, vd2 with
    | Interval vty1, Interval vty2
    | Interval_oc vty1, Interval vty2
    | Interval_oc vty1, Interval_oc vty2 ->
        is_more_specialized_value vty1 vty2
    | Interval_mix, _ -> False
    | _ -> not_kleene (is_more_specialized_vardom vd2 vd1)

let rec is_more_specialized (u1,a1) (u2,a2) =
  if a1 = a2 then True
  else
    match a1, a2 with
    | SAT, _ -> True
    | Box vd1, Box vd2 -> is_more_specialized_vardom vd1 vd2
    | Octagon vty1, Octagon vty2 -> is_more_specialized_value vty1 vty2
    | Octagon _, _ -> True
    | Logic_completion a1, a2 -> not_kleene (is_more_specialized (u2,a2) a1)
    | Direct_product adtys, a2 ->
        let specs = List.map (is_more_specialized (u2,a2)) adtys in
        if List.for_all (fun s -> s = True) specs then False
        else if List.for_all (fun s -> s = False) specs then True
        else Unknown
    | _ -> not_kleene (is_more_specialized (u2,a2) (u1,a1))

(** Map UID of abstract domain to its type. *)
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

let uids_of adty =
  let rec aux (uid, ty) =
    match ty with
    | Box _ -> [uid]
    | Octagon _ -> [uid]
    | SAT -> [uid]
    | Logic_completion adty -> uid::(aux adty)
    | Direct_product adtys ->
        List.fold_left (fun r adty -> r@(aux adty)) [uid] adtys
  in List.sort_uniq compare (aux adty)

let select_mgad adtys =
  let all_uids = List.fold_left
    (fun uids adty -> uids@(uids_of adty))
    [] adtys in
  let all_uids = List.sort_uniq compare all_uids in
  let adtys = List.filter (fun adty ->
    Core.Tools.is_set_equal (uids_of adty) all_uids) adtys in
  match adtys with
  | [] -> None
  | [adty] -> Some adty
  | _ -> failwith "select_mgad: More than one abstract element is the most general, this can only happen if two abstract element have the same UID, which should be forbidden."

let is_mgad adty uids =
  let uids' = uids_of adty in
  Core.Tools.is_subset_list uids uids'
