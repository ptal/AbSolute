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
open Bounds
open Domains.Abstract_domain
open Sat_rep
open Minisatml
open Minisatml.Solver

module type Sat_sig =
sig
  type t

  module R = Sat_rep
  module B = Bound_int

  val empty: t
  val extend: t -> R.var_kind -> (t * R.var_id)
  val project: t -> R.var_id -> (B.t * B.t)
  val lazy_copy: t -> int -> t list
  val copy: t -> t
  val closure: t -> t
  val weak_incremental_closure: t -> R.rconstraint -> t
  val entailment: t -> R.rconstraint -> Kleene.t
  val split: t -> t list
  val volume: t -> float
  val state_decomposition: t -> Kleene.t
  val print: R.t -> Format.formatter -> t -> unit
end

module Sat =
struct
  (* Unfortunately, minisatml relies on global variables.
     We should update it later. *)
  type t = {
    (* The depth is necessary to backtrack the state of minisat, it corresponds to the decision level. *)
    depth: int;
    (* A decision from `split` that is propagated in `closure`.
       It is set to `Solver.dummy_lit` if it has already been propagated. *)
    decision: Types.Lit.t;
  }

  module R = Sat_rep
  module B = Bound_int

  let empty = { depth=0; decision=dummy_lit }

  (* Minisatml does not use functional structure for automatic backtracking, so we trigger the backtracking manually.
     For safety, we guard all public functions of this module with `backtrack_state`.
     It avoids creating a new function such as `backtrack` only for this abstract domain.
     If later, we notice that such a function is useful for other domain, we might add it to `Abstract_domain`. *)
  let backtrack_state b =
    if decisionLevel () <> b.depth then
      cancelUntil b.depth

  let extend b () =
    backtrack_state b;
    (b, newVar ())

  let project b v =
    backtrack_state b;
    let open Types.Lbool in
    match value v with
    | LTrue -> (B.one, B.one)
    | LFalse -> (B.zero, B.zero)
    | LUndef -> (B.zero, B.one)

  let lazy_copy b n =
    backtrack_state b;
    newDecisionLevel ();
    let b = {b with depth=decisionLevel ()} in
    List.init n (fun _ -> b)

  let copy _ = failwith "`copy` is not supported on `Sat`."

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
    backtrack_state b;
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
    else b

  let weak_incremental_closure b c =
    backtrack_state b;
    if addClause c then b
    else raise Bot.Bot_found

  exception Satisfiable
  let entailment b c =
    backtrack_state b;
    let undef = ref false in
    try
      Vec.iter (fun l ->
        let open Types.Lbool in
        match value l with
        | LTrue -> raise Satisfiable
        | LFalse -> ()
        | LUndef -> undef := true) c;
      if !undef then Unknown else False
    with Satisfiable -> True

  let split b =
    backtrack_state b;
    (* New variable decision: *)
    let next = splitOnLit () in
    if next = dummy_lit then []
    else
      let branches = lazy_copy b 2 in
      List.map2 (fun b d -> { b with decision=d })
        branches [next; Types.Lit.tild next]

  let volume b =
    backtrack_state b;
    if not (okay ()) then 0.
    else
      let unassigned = nVars () - nAssigns () in
      if unassigned = 0 then 1.
      else (float_of_int unassigned) *. 2.

  let state_decomposition b =
    backtrack_state b;
    if nAssigns () = nVars() then True
    else Unknown

  let print _repr _fmt b =
    backtrack_state b;
    ()
end
