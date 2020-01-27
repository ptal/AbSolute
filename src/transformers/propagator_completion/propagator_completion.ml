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
open Domains.Interpretation
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

let no_variable_exn msg = no_variable_exn msg; failwith "unreachable"

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
    (* Store the new constraint's indices since last call to `drain_tasks`. *)
    new_tasks: int list;
    num_active_tasks: int;
  }

  let wrap pc a = pc.repr.a := a; pc
  let unwrap pc = !(pc.repr.a)

  let interpretation pc = pc.repr
  let map_interpretation pc f = {pc with repr=(f pc.repr)}

  (* Reexported functions from the parametrized modules. *)
  let entailment pc = Closure.entailment (unwrap pc)

  let empty _ = raise (Wrong_modelling ("Propagator_completion must be initialized with `init`."))
  let init repr = {
    repr;
    constraints = Tools.empty_parray ();
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

  let interpret pc approx = function
    | TExists (_, _) -> no_variable_exn "Propagator_completion.interpret"
    | TQFFormula tf ->
        let (repr, cs) = I.interpret pc.repr approx tf in
        { pc with repr }, cs

  let project _ _ = no_variable_exn "Propagator_completion.project"

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

  (** Closure is performed by `Event_loop` calling `exec_task`. *)
  let closure pc = pc, false

  (** We propagate the constraint immediately.
      If the constraint is not entailed, it is added into the pc. *)
  let weak_incremental_closure pc c =
    let _ = Format.printf "%a\n" print_box_cons (I.to_qformula pc.repr [c]); flush_all () in
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
    let a, entailed = Closure.incremental_closure (unwrap pc)
      (Parray.get pc.constraints c_idx) in
    let num_active_tasks = pc.num_active_tasks - (if entailed then 1 else 0) in
    { (wrap pc a) with num_active_tasks }, entailed

  let drain_events pc =
    let a, events = A.drain_events (unwrap pc) in
    wrap pc a, events

  let events_of_expr pc expr =
    let rec aux expr =
      match I.(expr.node) with
      | BCst _ -> []
      | BVar (vid, _) -> A.events_of_var (unwrap pc) vid
      | BUnary (_, e) -> aux e
      | BBinary (e1, _, e2) -> (aux e1)@(aux e2)
      | BFuncall (_,args) -> List.concat (List.map aux args)
    in aux expr

  let events_of pc (e1,_,e2) = (events_of_expr pc e1)@(events_of_expr pc e2)
  let events_of_var _ _ = []

  let drain_tasks pc =
    let drain_one acc c_idx =
      let c = Parray.get pc.constraints c_idx in
      let events = events_of pc c in
      ((uid pc, c_idx), events)::acc in
    let tasks_events = List.fold_left drain_one [] pc.new_tasks in
    ({ pc with new_tasks=[] }, tasks_events)
end
