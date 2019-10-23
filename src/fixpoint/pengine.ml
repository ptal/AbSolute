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

module type Pengine_sig =
sig
  type t
  type event = int
  type task_id = int

  val empty: unit -> t
  val extend_event: t -> t
  val extend_task: t -> t
  val react: t -> event list -> unit
  val subscribe: t -> task_id -> event list -> t
  val num_active_tasks: t -> int
  val fixpoint: t -> ('a -> task_id -> ('a * bool * event list)) -> 'a -> (t * 'a)
  val delta: t -> event list
end

module Pengine =
struct
  type task_id = int
  type event = int

  type t = {
    (* Records the active constraints (those not yet entailed).
       This field is backtracked automatically. *)
    actives: bool Parray.t;
    num_actives: int;

    (* `reactor.(v)` contains the set of constraints that contains the variable `v`. *)
    reactor: (task_id list) Parray.t;

    (* Contains all the constraint that must be propagated in order to reach a fixpoint.
       We also have the companion `inside_queue.(i)` that is true if the constraint `i` is inside the queue.
       This field is local to a node, so it is not backtracked nor copied. *)
    scheduler: task_id CCDeque.t;
    inside_queue: CCBV.t;

    (* Cache the events that happened since the last call to the function `delta`. *)
    delta: CCBV.t;
  }

  let empty () = {
    actives = Tools.empty_parray ();
    num_actives = 0;
    reactor = Tools.empty_parray ();
    scheduler=CCDeque.create ();
    inside_queue=CCBV.empty ();
    delta=CCBV.empty ();
  }

  let extend_event engine =
    let reactor = Tools.extend_parray engine.reactor [] in
    { engine with reactor }

  let extend_task engine =
    let actives = Tools.extend_parray engine.actives true in
    let num_actives = engine.num_actives + 1 in
    { engine with actives; num_actives }

  (* Schedule a task if it is active and not already present in the scheduler. *)
  let schedule engine task =
    if Parray.get engine.actives task &&
       not (CCBV.get engine.inside_queue task) then
    begin
      CCDeque.push_back engine.scheduler task;
      CCBV.set engine.inside_queue task
    end

  (* Retrieve the next task to execute. *)
  let pop engine =
    let task = CCDeque.take_front engine.scheduler in
    CCBV.reset engine.inside_queue task;
    task

  let react engine events =
    let react_on_event engine ev =
      CCBV.set engine.delta ev;
      List.iter (schedule engine) (Parray.get engine.reactor ev) in
    List.iter (react_on_event engine) events

  let deactivate_task engine task =
    let actives = Parray.set engine.actives task false in
    let num_actives = engine.num_actives-1 in
    { engine with actives; num_actives }

  let num_active_tasks engine = engine.num_actives

  (* An invariant is that the scheduler is empty at the end of this function, thus it is idempotent. *)
  let fixpoint engine f acc =
    let rec aux engine acc =
      if CCDeque.is_empty engine.scheduler then
        engine, acc
      else
      begin
        let task = pop engine in
        let acc, task_fixpoint, events = f acc task in
        let engine = if task_fixpoint then
            deactivate_task engine task
          else engine in
        react engine events;
        aux engine acc
      end
    in
    try
      aux engine acc
    with e -> begin
      CCDeque.clear engine.scheduler;
      CCBV.clear engine.inside_queue;
      CCBV.clear engine.delta;
      raise e
    end

  let subscribe engine task events =
    let subscribe_to_event reactor ev = Parray.set reactor ev (task::(Parray.get reactor ev)) in
    let reactor = List.fold_left subscribe_to_event engine.reactor events in
    { engine with reactor }

  let delta engine =
    let acc = ref [] in
    CCBV.iter_true engine.delta (fun i ->
      CCBV.reset engine.delta i;
      acc := i::!acc);
    !acc
end