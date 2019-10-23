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
open Core.Kleene
open Dbm
open Octagon_representation
open Domains.Abstract_domain

module Octagon_representation = Octagon_representation
module Dbm = Dbm
module Octagon_split = Octagon_split
module Closure = Closure

module type Octagon_sig =
sig
  module DBM : DBM_sig
  module B = DBM.B
  module R : Octagon_rep_sig with module B=B
  type t
  val empty: ad_uid -> t
  val uid: t -> ad_uid
  val extend: t -> R.var_kind -> (t * R.var_id)
  val project: t -> R.var_id -> (B.t * B.t)
  val lazy_copy: t -> int -> t list
  val copy: t -> t
  val closure: t -> t
  val incremental_closure: t -> R.rconstraint -> t
  val weak_incremental_closure: t -> R.rconstraint -> t
  val entailment: t -> R.rconstraint -> Kleene.t
  val strong_entailment: t -> R.rconstraint -> Kleene.t
  val split: t -> t list
  val volume: t -> float
  val state_decomposition: t -> Kleene.t
  val print: R.t -> Format.formatter -> t -> unit
  val unwrap: t -> DBM.t
end

module Make
  (Closure: Closure.Closure_sig)
  (SPLIT: Octagon_split.Octagon_split_sig) =
struct
  module DBM = Closure.DBM
  module Split = SPLIT(DBM)
  module B = DBM.B
  module R = Octagon_rep(B)

  module Itv_view = Interval_view_dbm.Interval_view(B)

  type t = {
    uid: ad_uid;
    dbm: DBM.t;
    (* These constraints must be coherent (see `Dbm.ml`). *)
    constraints: R.rconstraint list;
  }

  let empty uid = { uid; dbm=DBM.empty; constraints=[] }
  let uid octagon = octagon.uid

  let extend octagon () =
    let (dbm, itv) = DBM.extend octagon.dbm in
    ({octagon with dbm=dbm}, itv)

  let project octagon itv = Itv_view.dbm_to_itv itv (DBM.project octagon.dbm itv)

  let lazy_copy octagon n = List.map (fun dbm -> {octagon with dbm=dbm}) (DBM.lazy_copy octagon.dbm n)
  let copy octagon = {octagon with dbm=(DBM.copy octagon.dbm)}

  let entailment octagon oc =
    let open Kleene in
    let current = DBM.get octagon.dbm oc.v in
    if B.geq oc.d current then True
    (* If the addition of the bounds is less than zero then the two sides of the octagon are reversed. *)
    else if B.lt (B.add_up oc.d (DBM.get octagon.dbm (inv oc.v))) B.zero then False
    else Unknown

  let strong_entailment octagon oc =
    let open Kleene in
    match entailment octagon oc with
    | Unknown ->
      begin
        let dbm' = DBM.copy octagon.dbm in
        try
          let _ = Closure.incremental_closure dbm' oc in
          Unknown
        with Bot.Bot_found -> False
      end
    | r -> r

  let closure octagon =
    let dbm =
      if (List.length octagon.constraints) >= (DBM.dimension octagon.dbm) then
        List.fold_left DBM.set octagon.dbm octagon.constraints
        |> Closure.closure
      else
        List.fold_left Closure.incremental_closure octagon.dbm octagon.constraints in
    {octagon with dbm; constraints=[]}

  let weak_incremental_closure octagon oc =
    match entailment octagon oc with
    | True -> octagon
    | False -> raise Bot.Bot_found
    | Unknown -> { octagon with constraints=oc::octagon.constraints }

  let incremental_closure octagon oc =
    let octagon' = (weak_incremental_closure octagon oc) in
    if (List.length octagon.constraints) <> (List.length octagon'.constraints) then
      closure octagon'
    else
      octagon

  let split octagon =
    let branches = Split.split octagon.dbm in
    let octagons = lazy_copy octagon (List.length branches) in
    List.map2 weak_incremental_closure octagons branches

  let state_decomposition octagon =
    let open Kleene in
    match octagon.constraints with
    | [] -> True
    | _ -> Unknown

  (* Get the value of the lower bound and the volume between the lower and upper bound. *)
  let volume_of octagon itv =
    let (lb, ub) = project octagon itv in
    B.add_up B.one (B.sub_up ub lb)

  let volume octagon = B.to_float_up (Fold_intervals_canonical.fold (fun a itv ->
      B.mul_up a (volume_of octagon itv)
    ) B.one (DBM.dimension octagon.dbm))

  let print _ fmt octagon = DBM.print fmt octagon.dbm
  let unwrap octagon = octagon.dbm
end

module OctagonZ(SPLIT: Octagon_split.Octagon_split_sig) = Make(Closure.ClosureHoistZ)(SPLIT)
module OctagonQ(SPLIT: Octagon_split.Octagon_split_sig) = Make(Closure.ClosureQ)(SPLIT)
module OctagonF(SPLIT: Octagon_split.Octagon_split_sig) = Make(Closure.ClosureF)(SPLIT)
