(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

module Schedulable_abstract_domain = Schedulable_abstract_domain

open Domains
open Domains.Abstract_domain
open Fixpoint
open Core
open Lang.Ast
open Bounds
open Schedulable_abstract_domain
open Typing.Ad_type

module type Event_combinator =
sig
  type t
  val name: string
  val consume_task: t -> task -> (t * bool * event list)
  val produce_events: t -> Pengine2D.t -> (t * Pengine2D.t)
  val produce_tasks: t -> Pengine2D.t -> (t * Pengine2D.t)
end

module Event_atom(A: Schedulable_abstract_domain) =
struct
  type t = A.t ref

  let name = A.name

  let consume_task a task =
    let a', is_entailed = A.exec_task !a task in
    let a', events = A.drain_events a' in
    a := a';
    a, is_entailed, events

  let produce_tasks a pengine =
    let a', tasks_events = A.drain_tasks !a in
    a := a';
    let pengine = List.fold_left (
      fun p (task, events) -> Pengine2D.subscribe p task events) pengine tasks_events in
    a, pengine

  let produce_events a pengine =
    let a', events = A.drain_events !a in
    a := a';
    Pengine2D.react pengine events;
    a, pengine
end

module Event_cons(A: Schedulable_abstract_domain)(B: Event_combinator) =
struct
  type t = (A.t ref * B.t)

  module Atom = Event_atom(A)

  let name = Atom.name ^ "," ^ B.name

  let consume_task (a, b) ((uid, task_id) as task) =
    if A.uid !a = uid then
      let a, entailed, events = Atom.consume_task a task in
      (a,b), entailed, events
    else
      let b, entailed, events = B.consume_task b (uid, task_id) in
      (a,b), entailed, events

  let produce_tasks (a, b) pengine =
    let a, pengine = Atom.produce_tasks a pengine in
    let b, pengine = B.produce_tasks b pengine in
    (a,b), pengine

  let produce_events (a, b) pengine =
    let a, pengine = Atom.produce_events a pengine in
    let b, pengine = B.produce_events b pengine in
    (a,b), pengine
end

module Event_loop(L: Event_combinator) =
struct
  module B = Bound_unit
  module I = Unit_interpretation

  type t = {
    uid: ad_uid;
    l: L.t;
    pengine: Pengine2D.t;
  }

  let init uid l = {
    uid; l;
    pengine=Pengine2D.empty ();
  }

  let uid p = p.uid

  let name = "Event_loop(" ^ L.name ^ ")"

  let type_of _ = None

  let closure p =
    let l, pengine = L.produce_tasks p.l p.pengine in
    let l, pengine = L.produce_events l pengine in
    (* Printf.printf "E.closure: num active tasks %d\n" (Pengine2D.num_active_tasks pengine); *)
    let pengine, l, has_changed = Pengine2D.fixpoint pengine L.consume_task l in
    (* if has_changed then Printf.printf "E.closure: has_changed\n" else Printf.printf "E.closure: NOT has_changed\n"; *)
    {p with l; pengine}, has_changed

  let state _ = Kleene.True
  let split _ = []
  let volume _ = 1.
  let interpretation _ = Unit_interpretation.empty 0
  let map_interpretation x f =
    x, snd (f (Unit_interpretation.empty 0))
  let print _ _ = ()

  (* This abstract domain is totally functional. *)
  type snapshot = t
  let lazy_copy p n = List.init n (fun _ -> p)
  let restore _ s = s

  let meta_exn () = raise (Wrong_modelling ("[" ^ name ^ "] Event_loop is a meta abstract domain that does not represent any kind of variable or constraint."))

  let empty _ = raise (Wrong_modelling "`Event_loop.empty` is not supported, you should first create the abstract domains and then pass their references to `Event_loop.init`.")
  let project _ _ = meta_exn ()
  let embed _ _ _ = meta_exn ()
  let weak_incremental_closure _ _ = meta_exn ()
  let entailment _ _ = meta_exn ()
  let interpret _ _ _ = meta_exn ()
  let drain_events _ = meta_exn ()
  let events_of _ _ = meta_exn ()
  let events_of_var _ _ = meta_exn ()
end
