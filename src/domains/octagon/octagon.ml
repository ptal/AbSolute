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
open Dbm
open Octagon_interpretation
open Domains.Abstract_domain
open Event_loop.Event_abstract_domain

module Octagon_interpretation = Octagon_interpretation
module Dbm = Dbm
module Octagon_split = Octagon_split
module Closure = Closure

module type Octagon_sig =
sig
  include Event_abstract_domain

  module DBM: DBM_sig

  (** Perform the incremental closure of the DBM with the constraint. *)
  val incremental_closure: t -> I.rconstraint -> t * bool

  (** Low-level access to the DBM. *)
  val unwrap: t -> DBM.t
end

module Make
  (Closure: Closure.Closure_sig)
  (SPLIT: Octagon_split.Octagon_split_sig) =
struct
  module DBM = Closure.DBM
  module Split = SPLIT(DBM)
  module B = DBM.B
  module I = Octagon_interpretation(B)

  module Itv_view = Interval_view_dbm.Interval_view(B)

  type t = {
    uid: ad_uid;
    r: I.t;
    dbm: DBM.t;
    (* These constraints must fulfils the coherency rule (see `Dbm.ml`). *)
    constraints: I.rconstraint list;
  }

  let empty uid = {
    uid;
    r=I.empty ();
    dbm=DBM.empty;
    constraints=[] }

  let uid octagon = octagon.uid
  let name = "Octagon(" ^ B.name ^ ")"

  let interpretation octagon = octagon.r
  let map_interpretation octagon f = {octagon with r=(f octagon.r)}

  let extend ?ty octagon =
    let (dbm, v, aty) = DBM.extend ?ty octagon.dbm in
    ({octagon with dbm=dbm}, v, aty)

  let project' octagon itv =
    Itv_view.dbm_to_itv itv (DBM.project octagon.dbm itv)

  let project octagon var = project' octagon (as_interval var)

  type snapshot = t

  let lazy_copy octagon n =
    List.map (fun dbm -> {octagon with dbm=dbm}) (DBM.lazy_copy octagon.dbm n)

  let restore _ snapshot = snapshot

  let entailment octagon oc =
    let current = DBM.get octagon.dbm oc.v in
    B.geq oc.d current

  (** Perform the closure of the DBM taking into account all the constraints added through [weak_incremental_closure]. *)
  let closure octagon =
    let len = List.length octagon.constraints in
    let dbm =
      if len >= (DBM.dimension octagon.dbm) then
        List.fold_left DBM.set octagon.dbm octagon.constraints
        |> Closure.closure
      else
        List.fold_left Closure.incremental_closure octagon.dbm octagon.constraints in
    {octagon with dbm; constraints=[]}, len > 0

  (** Add the octagonal constraint in the octagon, if it is not entailed and without closing the DBM. *)
  let weak_incremental_closure octagon oc =
    if entailment octagon oc then octagon
    else { octagon with constraints=oc::octagon.constraints }

  let incremental_closure octagon oc =
    let octagon' = (weak_incremental_closure octagon oc) in
    if (List.length octagon.constraints) <> (List.length octagon'.constraints) then
      closure octagon'
    else
      octagon, false

  let split octagon =
    let branches = Split.split octagon.dbm in
    let octagons = lazy_copy octagon (List.length branches) in
    List.map2 weak_incremental_closure octagons branches

  let state octagon =
    let open Kleene in
    match octagon.constraints with
    | [] -> True
    | _ -> Unknown

  (* Get the value of the lower bound and the volume between the lower and upper bound. *)
  let volume_of octagon itv =
    let (lb, ub) = project' octagon itv in
    B.add_up B.one (B.sub_up ub lb)

  let volume octagon = B.to_float_up (Fold_intervals_canonical.fold (fun a itv ->
      B.mul_up a (volume_of octagon itv)
    ) B.one (DBM.dimension octagon.dbm))

  let print fmt octagon = DBM.print fmt octagon.dbm
  let unwrap octagon = octagon.dbm

  let make_events octagon vars : event list =
    List.map (fun v -> (octagon.uid, v)) vars

  let drain_events octagon =
    let dbm, deltas = DBM.delta octagon.dbm in
    { octagon with dbm }, (make_events octagon deltas)

  let events_of octagon c = [(octagon.uid, event_of_var c.v)]

  type t' = t
  include QInterpreter_base(struct
    type t=t'
    module I=I
    let name=name
    let interpretation=interpretation
    let map_interpretation=map_interpretation
    let extend=extend
    let weak_incremental_closure=weak_incremental_closure end)
end

module OctagonZ(SPLIT: Octagon_split.Octagon_split_sig) = Make(Closure.ClosureHoistZ)(SPLIT)
module OctagonQ(SPLIT: Octagon_split.Octagon_split_sig) = Make(Closure.ClosureQ)(SPLIT)
module OctagonF(SPLIT: Octagon_split.Octagon_split_sig) = Make(Closure.ClosureF)(SPLIT)
