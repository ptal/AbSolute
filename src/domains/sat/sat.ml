(* Copyright 2019 Pierre Talbot, Albin Coquereau

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
open Core.Types
open Bounds
open Domains.Abstract_domain
open Sat_interpretation
open Lang.Ast
open Minisatml
open Minisatml.Solver
open Minisatml.Types

module Sat =
struct
  module I = Sat_interpretation
  module B = Bound_int

  (* Unfortunately, minisatml relies on global variables.
     We should update it later. *)
  type t = {
    uid: ad_uid;
    r: I.t;
    (* The depth is necessary to backtrack the state of minisat, it corresponds to the decision level. *)
    depth: int;
    (* A decision from `split` that is propagated in `closure`.
       It is set to `Solver.dummy_lit` if it has already been propagated. *)
    decision: Types.Lit.t;
    (* The last index on the trail since `drain_delta` was called. *)
    last_trail_idx: int;
  }

  let name = "SAT"
  let interpretation sat = sat.r
  let map_interpretation sat f = {sat with r=(f sat.r)}

  let empty uid = {
    uid;
    r=I.empty ();
    depth=0;
    decision=dummy_lit;
    last_trail_idx=0;
  }

  let uid b = b.uid

  type snapshot = t

  let lazy_copy b n =
    newDecisionLevel ();
    let b = {b with depth=decisionLevel ()} in
    List.init n (fun _ -> b)

  (* Minisatml does not use functional structure for automatic backtracking, so we trigger the backtracking manually.
     For safety, we guard all public functions of this module with `backtrack_state`.
     It avoids creating a new function such as `backtrack` only for this abstract domain.
     If later, we notice that such a function is useful for other domain, we might add it to `Abstract_domain`. *)
  let restore _ b =
    (if decisionLevel () <> b.depth then
      cancelUntil b.depth);
    b

  let extend ?(ty = Abstract Bool) b =
    match ty with
    | Abstract Bool -> (b, newVar (), Bool)
    | ty -> raise (Wrong_modelling ("SAT abstract domain does not support variables of type " ^ (string_of_ty ty) ^ "."))

  let project _ v =
    let open Types.Lbool in
    match value v with
    | LTrue -> (B.one, B.one)
    | LFalse -> (B.zero, B.zero)
    | LUndef -> (B.zero, B.one)

  (* The learnt clause are stored in the watched literals array.
     Actually, there is no explicit "learnt clause database". *)
  let learn_clause learnt_clause =
    let clause = Types.Clause.clause_new
      (Vec.get_data learnt_clause)
      (Vec.size learnt_clause) ~learnt:true in
    attachClause clause;
    uncheckedEnqueue_clause (Vec.get learnt_clause 0) clause;
    varDecayActivity ()

  let propagate_decision b =
    if b.decision <> dummy_lit then begin
      uncheckedEnqueue b.decision;
      { b with decision=dummy_lit } end
    else
      b

  (* This closure extracts the propagation and conflict resolution parts from `search` in minisatml. *)
  let closure b =
    let props = numPropagations () in
    let b = propagate_decision b in
    let conflict_clause = ref (propagate ()) in
    (* In case of a conflict, we compute the backjump point and register the learnt clause. *)
    if !conflict_clause <> dummy_clause then
    begin
      if decisionLevel () = 0 then raise Bot.Bot_found;
      let learnt_clause = Vec.init 0 dummy_lit in
      let backtrack_level = ref 0 in
      analyze conflict_clause learnt_clause backtrack_level;
      learn_clause learnt_clause;
      raise (Conflict !backtrack_level)
    end
    else b, props <> (numPropagations ())

  let weak_incremental_closure b c =
    if addClause c then b
    else raise Bot.Bot_found

  exception Satisfiable
  let entailment _ c =
    try
      Vec.iter (fun l ->
        if value l = Types.Lbool.LTrue then
          raise Satisfiable
      ) c;
      false
    with Satisfiable -> true

  let split b =
    (* New variable decision: *)
    let next = splitOnLit () in
    if next = dummy_lit then []
    else
      let branches = lazy_copy b 2 in
      List.map2 (fun b d -> { b with decision=d })
        branches [next; Types.Lit.tild next]

  let volume _ =
    if not (okay ()) then 0.
    else
      let unassigned = nVars () - nAssigns () in
      if unassigned = 0 then 1.
      else (float_of_int unassigned) *. 2.

  let state _ =
    if nAssigns () = nVars() then True
    else Unknown

  let print _ _ = ()

  type t' = t
  include QInterpreter_base(struct
    type t=t'
    module I=I
    let name=name
    let interpretation=interpretation
    let map_interpretation=map_interpretation
    let extend=extend
    let weak_incremental_closure=weak_incremental_closure end)

  let drain_events b =
    let trail = getTrail () in
    let rec aux i =
      if i >= Vec.size trail then []
      else (b.uid, Vec.get trail i)::(aux (i+1))
    in
    let events = aux b.last_trail_idx in
    {b with last_trail_idx = (Vec.size trail)}, events

  let events_of b clause =
    let rec aux i =
      if i >= Vec.size clause then []
      else (b.uid, Lit.var (Vec.get clause i))::(aux (i+1))
    in
    aux 0
end
