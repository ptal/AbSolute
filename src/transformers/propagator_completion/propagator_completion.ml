(* Copyright 2020 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

module PC_interpretation = Pc_interpretation
module Hc4 = Hc4

open Core
open Core.Kleene
open Domains.Abstract_domain
open Lang
open Lang.Ast
open Typing
open Typing.Ad_type
open Typing.Tast
open Bounds
open Vardom
open Pc_interpretation
open Event_loop.Schedulable_abstract_domain

module type Propagator_completion_sig =
sig
  module B: Bound_sig.S
  module V: Vardom_sig.S
  module I: PC_interpretation_sig
  type vardom = V.t
  include Schedulable_abstract_domain with
    module B := B and module I := I
  val init: I.t -> t
end

module Propagator_completion
  (V: Vardom_sig.S)
  (A: Abstract_domain) =
struct
  module I = PC_interpretation(V)(A)
  module V = I.V
  module A = I.A
  module Closure = Hc4.Make(I)
  module B = V.B
  type vardom = V.t

  type t = {
    repr: I.t;
    constraints: I.rconstraint Parray.t;
    vars_equalities: I.rconstraint list;
    (* Store the new constraint's indices since last call to `drain_tasks`. *)
    new_tasks: int list;
    num_active_tasks: int;
  }

  let wrap pc a = pc.repr.a := a; pc
  let unwrap pc = !(pc.repr.a)

  let interpretation pc = pc.repr
  let map_interpretation pc f =
    let (repr, a) = f pc.repr in
    {pc with repr}, a

  (* NOTE: The entailment could be improved if we rewrite `c`. *)
  let entailment pc c = pc, c, Closure.entailment (unwrap pc) c

  let empty _ = raise (Wrong_modelling ("Propagator_completion must be initialized with `init`."))
  let init repr = {
    repr;
    constraints = Tools.empty_parray ();
    vars_equalities = [];
    new_tasks = [];
    num_active_tasks = 0;
  }

  let uid pc = pc.repr.uid

  let name = I.name

  let type_of pc =
    match A.type_of (unwrap pc) with
    | Some t -> Some (uid pc, Propagator_completion t)
    | None -> raise (Wrong_modelling
        "The domain underlying propagator completion must not be a meta-domain (it must have a type).")

  let add_equalities pc tv =
    let local_vars = A.I.local_vars (A.interpretation (unwrap pc)) tv.name in
    let n = List.length local_vars in
    let equalities = List.map (fun i ->
      let v1 = List.nth local_vars i in
      let v2 = List.nth local_vars (i + 1) in
      I.(make_expr (BVar ([v1], tv)), EQ, make_expr (BVar ([v2], tv)))
    ) (Tools.range 0 (n-2)) in
    { pc with vars_equalities=(pc.vars_equalities@equalities) }

  let check_empty_a_constraint = function
    | [] -> ()
    | _ -> raise (Wrong_modelling "Propagator_completion: Underlying domain cannot create constraints from variable.")

  let rec interpret pc approx = function
    (* If the variable has the UID of this PC element, then
         1. We extract the local variables of the underlying domain on this variable.
         2. Add the equalities among these variables in this element.
       This covers the case where a single variable is shared among several domains, in which case they communicate through equalities. *)
    | TExists (tv, tqf) when tv.uid = (uid pc) ->
        let tv = { tv with uid=(A.uid (unwrap pc)) } in
        let (a, cs) = A.interpret (unwrap pc) approx (TExists(tv,ttrue)) in
        check_empty_a_constraint cs;
        let pc = add_equalities (wrap pc a) tv in
        let pc, cs' = interpret pc approx tqf in
        pc, cs'
    | TExists (tv, tqf) ->
        let (a, cs) = A.interpret (unwrap pc) approx (TExists(tv,ttrue)) in
        check_empty_a_constraint cs;
        let pc, cs' = interpret (wrap pc a) approx tqf in
        pc, cs'
    | TQFFormula tf ->
        let (repr, cs) = I.interpret pc.repr approx tf in
        { pc with repr }, cs

  let project pc x = V.to_range (Closure.project (unwrap pc) x)
  let embed pc x v = wrap pc (Closure.embed (unwrap pc) x v)

  (* This abstract domain is totally functional. *)
  type snapshot = t
  let lazy_copy pc n = List.init n (fun _ -> pc)
  let restore _ s = s

  (* This abstract domain has no variable, we symbolically attribute a volume corresponding to the number of active formulas. *)
  let volume pc =
    if pc.num_active_tasks = 0 then 1.
    else float_of_int pc.num_active_tasks

  let print_box_cons fmt f =
    let f = Tast.quantifier_free_of f in
    Format.fprintf fmt "%a\n" Pretty_print.print_formula
      (Tast.tformula_to_formula f)

  let print fmt pc =
    Parray.iter (fun c -> print_box_cons fmt (I.to_qformula pc.repr [c])) pc.constraints

  (** Closure is performed by `Event_loop` calling `exec_task`, unless for the variables equalities.
      Rational:
        * A variable equality is automatically propagated (in exec_task) if a constraint has this variable in scope.
        * If no constraint own this variable, then this domain will not change the value of this variable.
        * Hence, it seems sufficient to propagate the equalities only one time per node (at the beginning). *)
  let closure pc =
    (* Printf.printf "PC.closure %d\n" (List.length pc.vars_equalities); *)
    let a, cons =
      List.fold_left (fun (a,cons) c ->
        let a, entailed = Closure.incremental_closure a c in
        (* let _ = Format.printf "PC.closure: %a %s\n" print_box_cons (I.to_qformula pc.repr [c]) (if entailed then "entailed" else "not yet entailed"); flush_all () in *)
        if entailed then a, cons else a, c::cons
      ) ((unwrap pc),[]) pc.vars_equalities in
    let pc = wrap pc a in
    { pc with vars_equalities=cons }, false (* Although some changes might have occur, it can be ignored. *)

  (** We propagate the constraint immediately.
      If the constraint is not entailed, it is added into the pc. *)
  let weak_incremental_closure pc c =
    (* let _ = Format.printf "inc(PC): %a\n" print_box_cons (I.to_qformula pc.repr [c]); flush_all () in *)
    (* Format.fprintf Format.std_formatter "%a\n" print_store (pc.repr,pc.store); *)
    let a, entailed = Closure.incremental_closure (unwrap pc) c in
    let pc = wrap pc a in
    if entailed then pc
    else
      let c_idx = Parray.length pc.constraints in
      let constraints = Tools.extend_parray pc.constraints c in
      { pc with constraints;
          new_tasks=c_idx::pc.new_tasks;
          num_active_tasks=pc.num_active_tasks+1 }

  (* Entailed constraints are automatically deactivated by `Event_loop`. *)
  let state pc =
    if pc.num_active_tasks = 0 then True
    else Unknown

  let split _ = []

  let exec_task pc (_,c_idx) =
    let c = Parray.get pc.constraints c_idx in
    (* let _ = Format.printf "PC.exec_task %d: %a\n" c_idx print_box_cons (I.to_qformula pc.repr [c]); flush_all () in *)
    let a, entailed = Closure.incremental_closure (unwrap pc) c in
    let num_active_tasks = pc.num_active_tasks - (if entailed then 1 else 0) in
    { (wrap pc a) with num_active_tasks }, entailed

  let drain_events pc =
    let a, events = A.drain_events (unwrap pc) in
    wrap pc a, events

  let events_of_expr pc expr =
    let rec aux expr =
      match I.(expr.node) with
      | BCst _ -> []
      | BVar (vids, _) ->
          List.flatten (List.map (A.events_of_var (unwrap pc)) vids)
      | BUnary (_, e) -> aux e
      | BBinary (e1, _, e2) -> (aux e1)@(aux e2)
      | BFuncall (_,args) -> List.concat (List.map aux args)
    in aux expr

  let events_of pc (e1,_,e2) =
    List.sort_uniq (fun (x,y) (x',y') ->
      let r = compare x x' in
      if r = 0 then compare y y' else r)
      ((events_of_expr pc e1)@(events_of_expr pc e2))

  let events_of_var _ _ = []

  let drain_tasks pc =
    let drain_one acc c_idx =
      let c = Parray.get pc.constraints c_idx in
      let events = events_of pc c in
      ((uid pc, c_idx), events)::acc in
    let tasks_events = List.fold_left drain_one [] pc.new_tasks in
    ({ pc with new_tasks=[] }, tasks_events)
end
