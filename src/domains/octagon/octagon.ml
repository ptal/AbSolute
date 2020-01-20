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
open Typing.Ad_type
open Typing.Tast
open Octagon_interpretation
open Domains.Abstract_domain

module Octagon_interpretation = Octagon_interpretation
module Dbm = Dbm
module Octagon_split = Octagon_split
module Closure = Closure

module type Octagon_sig =
sig
  module DBM: DBM_sig
  module B = DBM.B
  module I : module type of (Octagon_interpretation(DBM.B))
  include Abstract_domain with
    module I := I and
    module B := B

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
    r: I.t;
    dbm: DBM.t;
    (* These constraints must fulfils the coherency rule (see `Dbm.ml`). *)
    constraints: I.rconstraint list;
  }

  let interpretation octagon = octagon.r
  let map_interpretation octagon f = {octagon with r=(f octagon.r)}

  let empty uid = {
    r=I.empty uid;
    dbm=DBM.empty;
    constraints=[] }

  let uid octagon = I.uid octagon.r
  let name = "Octagon(" ^ B.name ^ ")"

  let type_of octagon = Some (uid octagon, Octagon B.type_of)

  let interpret octagon approx tqf =
    let rec aux octagon = function
      | TQFFormula tf ->
          let r, cs = I.interpret octagon.r approx tf in
          {octagon with r}, cs
      | TExists(tv, tqf) ->
          let (dbm, idx, aty) = DBM.extend ~ty:(tv.ty) octagon.dbm in
          let r = I.extend octagon.r (idx, {tv with ty = Abstract aty}) in
          aux {octagon with r; dbm} tqf
    in aux octagon tqf

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
    (* let _ = Format.fprintf Format.std_formatter "%a\n" Lang.Pretty_print.print_formula
      (Lang.Rewritting.quantifier_free_of (I.to_qformula octagon.r [oc])); flush_all () in *)
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
    List.map (fun v -> (uid octagon, v)) vars

  let drain_events octagon =
    let dbm, deltas = DBM.delta octagon.dbm in
    { octagon with dbm }, (make_events octagon deltas)

  let events_of octagon c = [(uid octagon, event_of_var c.v)]
end

module OctagonZ(SPLIT: Octagon_split.Octagon_split_sig) = Make(Closure.ClosureHoistZ)(SPLIT)
module OctagonQ(SPLIT: Octagon_split.Octagon_split_sig) = Make(Closure.ClosureQ)(SPLIT)
module OctagonF(SPLIT: Octagon_split.Octagon_split_sig) = Make(Closure.ClosureF)(SPLIT)
