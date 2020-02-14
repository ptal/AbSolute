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
open Box_interpretation

module type Variable_order = functor (R: Box_interpretation_sig) ->
sig
  module R: Box_interpretation_sig
  type t = R.var_id list
  val select: t -> R.Store.t -> t * (R.var_id * R.var_dom) option
end with module R=R

module type Value_order = functor (R: Box_interpretation_sig) ->
sig
  module R: Box_interpretation_sig
  val select: R.var_dom -> R.Vardom.B.t
end with module R=R

module type Distributor = functor (R: Box_interpretation_sig) ->
sig
  module R: Box_interpretation_sig
  val distribute: R.t -> R.var_id -> R.Vardom.t -> R.rconstraint list
end with module R=R

module type Box_split_sig = functor (R: Box_interpretation_sig) ->
sig
  type t = R.var_id list
  module R: Box_interpretation_sig
  val split: t -> R.t -> R.Store.t -> t * R.rconstraint list
end with module R=R

module Input_order(R: Box_interpretation_sig) =
struct
  module R=R
  module Store = R.Store
  module V = R.Vardom

  type t = R.var_id list

  let rec select vids store =
    match vids with
    | [] -> vids, None
    | vid::vids ->
        let value = Store.get store vid in
        if V.is_singleton value then
          select vids store
        else
          vids, Some (vid, value)
end

(* This module factorizes `First_fail` and `Anti_first_fail`. *)
module Width_order(R: Box_interpretation_sig) =
struct
  module R=R
  module V = R.Vardom
  module Store = R.Store
  module B = V.B
  let select vids store width_cmp =
    let size (l,h) = B.sub_up h l in
    let vids_values =
      Tools.filter_map (fun vid ->
        let d = Store.get store vid in
        if V.is_singleton d then None else Some (vid,d)
      ) vids in
    let var, vids =
      Tools.fold_map (fun a (vid,d) ->
        let width = size (V.to_range d) in
        let best = match a with
          | Some (best,_,_) when width_cmp width best -> Some (width,vid,d)
          | Some _ -> a
          | None -> Some (width,vid,d) in
        best, vid
      ) None vids_values in
    match var with
    | Some (_, vid,d) -> vids, Some (vid,d)
    | None -> vids, None

end

module First_fail(R: Box_interpretation_sig) =
struct
  module R=R
  module W = Width_order(R)
  type t = R.var_id list
  let select vids store = W.select vids store R.Vardom.B.lt
end

module Anti_first_fail(R: Box_interpretation_sig) =
struct
  module R=R
  module W = Width_order(R)
  type t = R.var_id list
  let select vids store = W.select vids store R.Vardom.B.gt
end

module Middle (R: Box_interpretation_sig) =
struct
  module R=R
  module V = R.Vardom
  module B = V.B
  let select vardom =
    let (l,u) = V.to_range vardom in
    B.div_down (B.add_up l u) B.two
end

module Lower_bound (R: Box_interpretation_sig) =
struct
  module R=R
  module V = R.Vardom
  let select vardom = fst (V.to_range vardom)
end

module Upper_bound (R: Box_interpretation_sig) =
struct
  module R=R
  module V = R.Vardom
  let select vardom = snd (V.to_range vardom)
end

module Assign (R: Box_interpretation_sig) =
struct
  module R=R
  module V=R.Vardom
  let distribute repr var_idx value =
    let tv = R.to_logic_var repr var_idx in
    let left = V.interpret Exact (tv,Ast.EQ,value) in
    let right = V.interpret Exact (tv,Ast.NEQ,value) in
    [(var_idx,left);(var_idx,right)]
end

module Bisect (R: Box_interpretation_sig) =
struct
  module R=R
  module V=R.Vardom
  let distribute repr var_idx value =
    let tv = R.to_logic_var repr var_idx in
    let left = V.interpret Exact (tv,Ast.LEQ,value) in
    let right = V.interpret Exact (tv,Ast.GT,value) in
    [(var_idx,left);(var_idx,right)]
end

module ReverseBisect (R: Box_interpretation_sig) =
struct
  module R=R
  module V=R.Vardom
  let distribute repr var_idx value =
    let tv = R.to_logic_var repr var_idx in
    let left = V.interpret Exact (tv,Ast.GEQ,value) in
    let right = V.interpret Exact (tv,Ast.LT,value) in
    [(var_idx,left);(var_idx,right)]
end

module Make
  (VARIABLE: Variable_order)
  (VALUE: Value_order)
  (DISTRIB: Distributor)
  (R: Box_interpretation_sig) =
struct
  module R = R
  module Variable = VARIABLE(R)
  module Value = VALUE(R)
  module Distrib = DISTRIB(R)

  type t = R.var_id list

  let split vids repr store =
    match Variable.select vids store with
    | vids, None -> vids, []
    | vids, Some (var_idx, vardom) ->
        let value = R.Vardom.of_bound (Value.select vardom) in
        vids, Distrib.distribute repr var_idx value
end

module First_fail_bisect = Make(First_fail)(Middle)(Bisect)
module First_fail_LB = Make(First_fail)(Lower_bound)(Bisect)
module Anti_first_fail_LB = Make(Anti_first_fail)(Lower_bound)(Bisect)
module Anti_first_fail_UB = Make(Anti_first_fail)(Upper_bound)(ReverseBisect)
module MSLF_simple = First_fail_LB
