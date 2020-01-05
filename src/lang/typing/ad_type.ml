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
    | _ -> is_more_specialized (u2,a2) (u1,a1)
