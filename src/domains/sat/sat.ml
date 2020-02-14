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
open Domains.Interpretation
open Sat_interpretation
open Lang.Ast
open Typing.Ad_type
open Typing.Tast
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
    r: I.t;
    (* The depth is necessary to backtrack the state of minisat, it corresponds to the decision level. *)
    depth: int;
    (* A decision from `split` that is propagated in `closure`.
       It is set to `Solver.dummy_lit` if it has already been propagated. *)
    decision: Lit.t;
    (* The last index on the trail since `drain_delta` was called. *)
    last_trail_idx: int;
    (* The latest learnt clause, to be considered only once after backjumping. *)
    learnt_clause: Lit.t Vec.t;
  }

  let dummy_learnt_clause = Vec.init 0 dummy_lit


  let uid b = I.uid b.r
  let name = "SAT"
  let type_of sat = Some (uid sat, SAT)

  let interpretation sat = sat.r
  let map_interpretation sat f =
    let (r, a) = f sat.r in
    {sat with r}, a

  let empty uid =
    resetEnv ();
    {
      r=I.empty uid;
      depth=0;
      decision=dummy_lit;
      last_trail_idx=0;
      learnt_clause=dummy_learnt_clause;
    }

  type snapshot = t

  let lazy_copy b n =
    newDecisionLevel ();
    (* let _ = Printf.printf "New level %d.\n" (decisionLevel ()); flush_all () in *)
    let b = {b with depth=decisionLevel ()} in
    List.init n (fun _ -> b)

  (* Minisatml does not use functional structure for automatic backtracking, so we trigger the backtracking manually.
     For safety, we guard all public functions of this module with `backtrack_state`.
     It avoids creating a new function such as `backtrack` only for this abstract domain.
     If later, we notice that such a function is useful for other domain, we might add it to `Abstract_domain`. *)
  let restore b snapshot =
    (* let _ = Printf.printf "Restore from %d to %d.\n" (decisionLevel ()) b.depth; flush_all () in *)
    (if decisionLevel () <> snapshot.depth then
      cancelUntil snapshot.depth);
    { snapshot with learnt_clause=b.learnt_clause }

  let interpret sat approx tqf =
    let rec aux sat = function
      | TQFFormula tf ->
          let r, cs = I.interpret sat.r approx tf in
          {sat with r}, cs
      | TExists(tv, tqf) ->
          guarded_extend sat (uid sat) name tv (fun sat tv ->
            match tv.ty with
            | Abstract Bool ->
                let v = newVar () in
                let r = I.extend sat.r (v, tv) in
                (* let _ = Printf.printf "Extend %d\n" v; flush_all () in *)
                aux {sat with r} tqf
            | ty -> raise (Wrong_modelling ("SAT abstract domain does not support variables of type " ^ (string_of_ty ty) ^ ".")))
    in aux sat tqf

  let project _ v =
    let open Types.Lbool in
    match value v with
    | LTrue -> (B.one, B.one)
    | LFalse -> (B.zero, B.zero)
    | LUndef -> (B.zero, B.one)

  let embed b v (l,u) =
    if B.equal l u then
    begin
      if B.equal l B.zero then
        uncheckedEnqueue (Lit.lit v true)
      else if B.equal l B.one then
        uncheckedEnqueue (Lit.lit v false)
    end;
    b

  (* The learnt clause are stored in the watched literals array.
     Actually, there is no explicit "learnt clause database". *)
  let propagate_conflict b =
    (if Vec.size b.learnt_clause = 0 then ()
    else if Vec.size b.learnt_clause = 1 then
      uncheckedEnqueue (Vec.get b.learnt_clause 0)
    else
    begin
      let clause = Clause.clause_new
        (Vec.get_data b.learnt_clause)
        (Vec.size b.learnt_clause) ~learnt:true in
      let _ = Clause.iter (fun i -> Printf.printf " %d \n" (Lit.var i)) clause; flush_all () in
      attachClause clause;
      uncheckedEnqueue_clause (Vec.get b.learnt_clause 0) clause;
      varDecayActivity ()
    end);
    { b with learnt_clause=dummy_learnt_clause}

  let propagate_decision b =
    if b.decision <> dummy_lit then begin
      uncheckedEnqueue b.decision;
      { b with decision=dummy_lit } end
    else
      b

  (* This closure extracts the propagation and conflict resolution parts from `search` in minisatml. *)
  let closure b =
    let props = numPropagations () in
    let b = propagate_conflict b in
    let b = propagate_decision b in
    let conflict_clause = ref (propagate ()) in
    (* In case of a conflict, we compute the backjump point and register the learnt clause. *)
    if !conflict_clause <> dummy_clause then
    begin
      if decisionLevel () = 0 then raise Bot.Bot_found;
      let backtrack_level = ref 0 in
      analyze conflict_clause b.learnt_clause backtrack_level;
      raise (Conflict !backtrack_level)
    end
    else b, props <> (numPropagations ())

  (** According to an assert in Minisatml, this can only be used at root level (not during solving). *)
  let weak_incremental_closure b c =
    if addClause c then b
    else
      ((* Format.printf "Clause %a unsat.\n" Lang.Pretty_print.print_qformula (I.to_qformula b.r [c]); *)
       raise Bot.Bot_found)

  exception Satisfiable
  let entailment sat c =
    try
      Vec.iter (fun l ->
        if value l = Types.Lbool.LTrue then
          raise Satisfiable
      ) c;
      sat, c, false
    with Satisfiable -> sat, c, true

  let split ?strategy:(strat=Simple) b =
    if strat <> Simple then raise (Wrong_modelling "SAT.split: Only the simple strategy is supported.");
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

  let drain_events b =
    let trail = getTrail () in
    let rec aux i =
      if i >= Vec.size trail then []
      else (uid b, Vec.get trail i)::(aux (i+1))
    in
    let events = aux b.last_trail_idx in
    {b with last_trail_idx = (Vec.size trail)}, events

  let events_of b clause =
    let rec aux i =
      if i >= Vec.size clause then []
      else (uid b, Lit.var (Vec.get clause i))::(aux (i+1))
    in
    aux 0

  let events_of_var b vid = [(uid b, vid)]
end
